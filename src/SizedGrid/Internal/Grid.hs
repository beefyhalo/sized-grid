{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

-- |
-- Module      :  SizedGrid.Internal.Grid
-- License     :  MIT -style (see the file LICENSE)
--
-- The `Grid` representation and everything defined over it.
--
-- This module is hidden. It exists so that the `Grid` constructor can be shared
-- with "SizedGrid.Grid.Unsafe" without also being shared with the world: the
-- one invariant this library exists to enforce is that a @Grid cs a@ holds
-- exactly @MaxCoordSize cs@ elements, and an exported constructor is a licence
-- to break it. "SizedGrid.Grid.Grid" re-exports the safe half of what is here.
--
-- Everything below is free to use the constructor directly. The obligation that
-- comes with that is on each function in turn: it must not change the length of
-- the vector except in step with the type.
module SizedGrid.Internal.Grid
  ( -- * Representation
    Grid(..)
  , unsafeGridFromVector
    -- * Construction and access
  , gridVector
  , gridFromVector
  , gridFromList
  , collapseGrid
    -- * Type-level machinery
  , Head
  , Tail
  , CollapseGrid
  , AllGridSizeKnown
    -- * Rearranging
  , transposeGrid
  , splitGrid
  , combineGrid
  , combineHigherDim
  , splitHigherDim
  , dropGrid
  , takeGrid
  , mapLowerDim
  , zipLowerDim
  , scanl1Grid
    -- * Windows and tiles
  , ShrinkableGrid(..)
  , gridTiles
    -- * Vector helpers
  , splitVectorBySize
  ) where

import           SizedGrid.Coord
import           SizedGrid.Coord.Class
import           SizedGrid.Internal.Type (requiring, windowFits)

import           Control.Applicative   (ZipList (..))
import           Control.Lens          hiding (index)
import           Data.Aeson
import           Data.Constraint
import           Data.Distributive
import           Data.Functor.Classes
import           Data.Functor.Rep
import qualified Data.Vector           as V
import           Generics.SOP
import qualified GHC.Generics          as GHC
import           GHC.TypeLits
import qualified GHC.TypeLits          as GHC
import Data.Kind (Type)

-- | A multi dimensional sized grid.
--
-- The field is called @unGrid@ rather than @gridVector@ so that the derived
-- `Show` output is unchanged from earlier versions. It is not exported as a
-- field anywhere: a record field in scope permits record update syntax, and
-- @g { unGrid = V.empty }@ is exactly the unsound construction the constructor
-- is being hidden to prevent. Use `gridVector` to read it.
newtype Grid (cs :: [Type]) a = Grid
  { unGrid :: V.Vector a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Eq1, Show1, GHC.Generic)

-- | The escape hatch, re-exported from "SizedGrid.Grid.Unsafe".
--
-- Asserts what `gridFromVector` checks: that the vector holds exactly
-- @MaxCoordSize cs@ elements. Nothing verifies it, and a grid that fails it
-- makes `Data.Functor.Rep.index` throw on positions its own type calls valid.
--
-- Only construction needs to be unsafe. Reading the vector back out cannot
-- invalidate anything, so that direction is `gridVector`, which is public. A
-- bidirectional pattern synonym would have covered both at once, but it would
-- also have dragged the safe direction into the unsafe import -- and naming a
-- pattern synonym in an import list requires the importing module to enable
-- @PatternSynonyms@, which is a poor toll to charge for reading a vector.
--
-- The name is deliberately alarming. The constructor it stands for is spelled
-- @Grid@, which reads as ordinary code at a use site and gives no hint that the
-- library's central invariant is being asserted rather than established.
unsafeGridFromVector :: V.Vector a -> Grid cs a
unsafeGridFromVector = Grid

-- | Read a grid's elements in row-major order.
--
-- Safe in the direction that matters: reading a vector out cannot invalidate
-- anything. The result always has @MaxCoordSize cs@ elements.
gridVector :: Grid cs a -> V.Vector a
gridVector = unGrid

-- | Build a grid from a vector, checking that its length is the one the type
-- claims. `Nothing` if it is not.
--
-- This is the safe counterpart to `UnsafeGrid`, and the reason the constructor
-- no longer needs to be public.
gridFromVector ::
       forall cs a. AllSizedKnown cs
    => V.Vector a
    -> Maybe (Grid cs a)
gridFromVector v =
    withDict
        (sizeProof @cs)
        (if V.length v ==
            fromIntegral (GHC.natVal (Proxy :: Proxy (MaxCoordSize cs)))
             then Just (Grid v)
             else Nothing)

-- | Left-to-right scan over the whole grid in row-major order, keeping the
-- shape. Accumulates strictly, as a running total over a boxed vector otherwise
-- builds a chain of thunks the length of the grid.
--
-- Row-major means that on a multi-dimensional grid the scan runs off the end of
-- one row and straight into the next. To scan each row independently, which is
-- usually what is wanted, compose it with `mapLowerDim`:
--
-- > runIdentity (mapLowerDim (Identity . scanl1Grid (+)) g)
--
-- This exists so that prefix sums -- the summed-area-table build-up being the
-- common case -- do not need the escape hatch. Length preservation is
-- guaranteed by 'V.scanl1'', so no size constraint is needed.
scanl1Grid :: (a -> a -> a) -> Grid cs a -> Grid cs a
scanl1Grid f (Grid v) = Grid (V.scanl1' f v)

instance AllSizedKnown cs => Applicative (Grid cs) where
    pure =
        withDict
            (sizeProof @cs)
            (Grid .
             V.replicate
                 (fromIntegral $ GHC.natVal (Proxy :: Proxy (MaxCoordSize cs))))
    Grid fs <*> Grid as = Grid $ V.zipWith ($) fs as

instance (AllSizedKnown cs, All IsCoordLifted cs) =>
         Monad (Grid cs) where
  g >>= f = imap (\p a -> f a `index` p) g

instance (AllSizedKnown cs, All IsCoordLifted cs) =>
         Distributive (Grid cs) where
  distribute = distributeRep

instance (All IsCoordLifted cs, AllSizedKnown cs) =>
         Representable (Grid cs) where
  type Rep (Grid cs) = Coord cs
  -- 'V.fromListN' rather than 'V.fromList': a list of statically unknown length
  -- makes the vector grow by doubling, so it is allocated and copied several
  -- times over, and the length is known --- it is 'coordSpaceSize'.
  --
  -- The traversals below deliberately do not do this, and the difference is
  -- that 'tabulate' ends in a vector whatever happens. They do not: they hand
  -- their list straight to a 'V.zipWith' that fuses with it.
  tabulate func = Grid $ V.fromListN (coordSpaceSize @cs) $ map func allCoord
  index (Grid v) c = v V.! coordPosition c

-- | @V.fromList allCoord@ looks like it materialises the whole coordinate list
-- on every traversal, and that is what @sized-grid-uvd@ was raised about, but
-- it does not: 'V.zipWith' fuses with it, so the coordinates are produced and
-- consumed one at a time and die in the nursery. Replacing it with anything the
-- simplifier cannot see through --- a vector built by a recursive function, or
-- the same list under 'V.fromListN' --- forces 90,000 coordinates to be live at
-- once and made @imap@ 23% more allocation and 59% slower, measured. Leave it
-- alone.
--
-- The cost that /was/ real is in `coordPosition`, which these traversals'
-- callers almost always apply to the coordinate they are handed.
instance (All IsCoordLifted cs) => FunctorWithIndex (Coord cs) (Grid cs) where
  imap func (Grid v) = Grid $ V.zipWith func (V.fromList allCoord) v

-- | 'ifoldr' and 'ifoldl'' are given outright because the class otherwise
-- builds them out of 'ifoldMap' and an 'Endo' chain, which is a closure per
-- cell on top of the coordinate.
instance (All IsCoordLifted cs) => FoldableWithIndex (Coord cs) (Grid cs) where
  ifoldMap func (Grid v) = foldMap id $ V.zipWith func (V.fromList allCoord) v
  ifoldr func z (Grid v) = V.foldr ($) z $ V.zipWith func (V.fromList allCoord) v
  ifoldl' func z (Grid v) =
    V.foldl' (&) z $ V.zipWith (\c x acc -> func c acc x) (V.fromList allCoord) v

instance (All IsCoordLifted cs) => TraversableWithIndex (Coord cs) (Grid cs) where
  itraverse func (Grid v) =
    Grid <$> sequenceA (V.zipWith func (V.fromList allCoord) v)

-- | The first element of a type level list
type family Head xs where
  Head (x ': xs) = x

-- | All but the first elements of a type level list
type family Tail xs where
  Tail (x ': xs) = xs

-- | Given a grid type, give back a series of nested lists repesenting the grid. The lists will have a number of layers equal to the dimensionality.
type family CollapseGrid cs a where
  CollapseGrid '[] a = a
  CollapseGrid (c ': cs) a = [CollapseGrid cs a]

-- | A Constraint that all grid sizes are instances of `KnownNat`
type family AllGridSizeKnown cs :: Constraint where
  AllGridSizeKnown '[] = ()
  AllGridSizeKnown cs  = ( GHC.KnownNat (CoordNat (Head cs))
                        , GHC.KnownNat (MaxCoordSize (Tail cs))
                        , GHC.KnownNat (MaxCoordSize (cs))
                        , AllGridSizeKnown (Tail cs))


-- | Convert a vector into a list of `Vector`s, where all the elements of the
-- list have the given size.
--
-- If @n@ does not divide the length, the final chunk is short. Every caller in
-- this module is protected from that by the size invariant on 'Grid', so the
-- short chunk is unreachable for them -- but it is a silent malformation rather
-- than a failure, so do not rely on it.
--
-- A size of zero would otherwise loop forever taking empty prefixes.
splitVectorBySize :: Int -> V.Vector a -> [V.Vector a]
splitVectorBySize n v
  | n <= 0 = error $ "splitVectorBySize: chunk size must be positive, got " ++ show n
  | V.length v >= n = V.take n v : splitVectorBySize n (V.drop n v)
  | V.null v = []
  | otherwise = [v]

-- | Convert a grid to a series of nested lists. This removes type level information, but it is sometimes easier to work with lists
collapseGrid ::
     forall cs a. (SListI cs, AllGridSizeKnown cs)
  => Grid cs a
  -> CollapseGrid cs a
collapseGrid (Grid v) =
  case (shape :: Shape cs) of
    ShapeNil -> v V.! 0
    ShapeCons (_ :: Shape xs) ->
      map (collapseGrid . Grid @xs) $
      splitVectorBySize
        (fromIntegral $ GHC.natVal (Proxy @(MaxCoordSize xs)))
        v

-- | Convert a series of nested lists to a grid. If the size of the grid does not match the size of lists this will be `Nothing`
gridFromList ::
     forall cs a. (SListI cs, AllGridSizeKnown cs)
  => CollapseGrid cs a
  -> Maybe (Grid cs a)
gridFromList cg =
  case (shape :: Shape cs) of
    ShapeNil -> Just $ Grid $ V.singleton $ cg
    ShapeCons _ ->
      if length cg == fromIntegral (GHC.natVal (Proxy @(CoordNat (Head cs))))
        then Grid . mconcat <$>
             traverse (fmap unGrid . gridFromList @(Tail cs)) cg
        else Nothing

instance (AllGridSizeKnown cs, ToJSON a, SListI cs) => ToJSON (Grid cs a) where
  toJSON (Grid v) =
    case (shape :: Shape cs) of
      ShapeNil -> toJSON (v V.! 0)
      ShapeCons _ ->
        toJSON $
        map (toJSON . Grid @(Tail cs)) $
        splitVectorBySize
          (fromIntegral $ GHC.natVal (Proxy @(MaxCoordSize (Tail cs))))
          v

-- | Decoding validates the length at every dimension, so a successfully decoded
-- grid always satisfies @V.length (unGrid g) == MaxCoordSize cs@. Without the
-- check a short or ragged array decoded to a `Grid` whose vector disagreed with
-- its type, which then made `index` throw and ('<*>') silently truncate.
--
-- The constraints match `ToJSON`\'s: the `KnownNat` evidence is what makes the
-- check possible.
instance (AllGridSizeKnown cs, SListI cs, FromJSON a) =>
         FromJSON (Grid cs a) where
  parseJSON v =
    case (shape :: Shape cs) of
      ShapeNil -> Grid . V.singleton <$> parseJSON v
      ShapeCons _ -> do
        a :: [Grid (Tail cs) a] <- parseJSON v
        let expected =
              fromIntegral $ GHC.natVal (Proxy @(CoordNat (Head cs))) :: Int
        if length a == expected
          then return $ Grid $ foldMap unGrid a
          else fail $
               "Grid: expected " ++
               show expected ++ " elements, got " ++ show (length a)

transposeGrid ::
     ( IsCoord h
     , IsCoord w
     , GHC.KnownNat x
     , GHC.KnownNat y
     , 1 <= y
     , 1 <= x
     )
  => Grid '[ w x, h y] a
  -> Grid '[ h y, w x] a
transposeGrid g = tabulate $ \i -> index g $ tranposeCoord i

splitGrid ::
       forall c cs a. (AllSizedKnown cs)
    => Grid (c ': cs) a
    -> Grid '[ c] (Grid cs a)
splitGrid (Grid v) =
    withDict
        (sizeProof @cs)
        (Grid $
         V.fromList $
         map
             Grid
             (splitVectorBySize
                  (fromIntegral $ GHC.natVal (Proxy :: Proxy (MaxCoordSize cs)))
                  v))

combineGrid :: Grid '[c] (Grid cs a) -> Grid (c ': cs) a
combineGrid (Grid v) = Grid (v >>= unGrid)

-- | @IsCoord c@ used to be demanded here. It buys nothing: the size of a coord
-- comes from @CoordNat@ on the `SizedGrid.Coord.Class.IsCoordLifted` instance,
-- not from `IsCoord`, so the class could not have justified the @n + m@ in the
-- result even in principle.
combineHigherDim ::
       Grid (c n ': as) x
    -> Grid (c m ': as) x
    -> Grid (c (n + m) ': as) x
combineHigherDim (Grid v1) (Grid v2) = Grid (v1 <> v2)

-- | @n <= m@ is required: without it @dropGrid \@9@ of a 3-grid typechecked and
-- produced a grid whose vector was empty while its type claimed @3 - 9@.
dropGrid ::
       forall n m c x. (KnownNat n, n <= m)
    => Proxy n
    -> Grid '[ c m] x
    -> Grid '[ c (m - n)] x
dropGrid p (Grid v) = requiring @(n <= m) $ Grid $ V.drop (fromIntegral $ natVal p) v

-- | @n <= m@ is required: 'V.take' cannot conjure elements, so without the
-- constraint @takeGrid \@9@ of a 3-grid returned 3 elements under a type that
-- promised 9.
takeGrid ::
       forall n m c x. (KnownNat n, n <= m)
    => Proxy n
    -> Grid '[ c m] x
    -> Grid '[ c n] x
takeGrid p (Grid v) = requiring @(n <= m) $ Grid $ V.take (fromIntegral $ natVal p) v

-- | The second component is @x - y@, not a free type variable. It used to be
-- free, which let the caller annotate the remainder with any size at all and
-- get a grid whose vector did not match.
splitHigherDim ::
       forall c as x y a.
       ( KnownNat y
       , y <= x
       , AllSizedKnown as
       )
    => Grid (c x ': as) a
    -> (Grid (c y ': as) a, Grid (c (x - y) ': as) a)
splitHigherDim (Grid v) =
    requiring @(y <= x) $
    let (a, b) =
            withDict
                (sizeProof @as)
                (V.splitAt
                     (fromIntegral $
                      GHC.natVal (Proxy @y) *
                      GHC.natVal (Proxy @(MaxCoordSize as)))
                     v)
     in (Grid a, Grid b)

-- | Split a grid into its @CoordNat c@ sub-grids along the outermost axis,
-- apply @f@ to each, and glue the results back together.
--
-- The effects of @f@ are combined with @traverse@, so the choice of @f@ decides
-- how the per-sub-grid results are combined, and the obvious choice is usually
-- the wrong one. With @f ~ []@ this is the list applicative -- a cartesian
-- product of one result per sub-grid, @n ^ n@ grids for @n@ sub-grids each
-- returning @n@ results, not @n@ grids. That is almost never what a caller
-- taking slices means; use 'zipLowerDim' for that. @f ~ Identity@ (a
-- length-preserving map over each sub-grid) and @f ~ Maybe@ (a fallible one)
-- behave as expected.
mapLowerDim ::
       forall as bs x y c f. (AllSizedKnown as, Applicative f)
    => (Grid as x -> f (Grid bs y))
    -> Grid (c ': as) x
    -> f (Grid (c ': bs) y)
mapLowerDim f (Grid v) =
    withDict
        (sizeProof @as)
        (fmap (Grid . V.concat) $
         traverse (fmap unGrid . f . Grid) $
         splitVectorBySize
             (fromIntegral (GHC.natVal (Proxy @(MaxCoordSize as))))
             v)

-- | 'mapLowerDim' where @f@ returns many results per sub-grid and they should
-- be zipped positionally rather than multiplied: the @k@th result is built from
-- the @k@th result of every sub-grid.
--
-- This is what slicing a grid along its second axis needs. @zipLowerDim
-- 'gridTiles'@ on a 9x9 board gives the 9 columns; @mapLowerDim 'gridTiles'@
-- gives 387,420,489 grids, one for every way of picking a cell from each row.
--
-- The result is as long as the shortest per-sub-grid list, so it is only the
-- expected length when @f@ returns the same number of results for every
-- sub-grid -- which 'gridTiles' does, since its count is fixed by the types.
zipLowerDim ::
       forall as bs x y c. AllSizedKnown as
    => (Grid as x -> [Grid bs y])
    -> Grid (c ': as) x
    -> [Grid (c ': bs) y]
zipLowerDim f = getZipList . mapLowerDim (ZipList . f)

class ShrinkableGrid (cs :: [Type]) (as :: [Type]) (bs :: [Type]) where
  shrinkGrid :: Coord cs -> Grid as x -> Grid bs x

instance ShrinkableGrid '[] '[] '[] where
  shrinkGrid _ (Grid v) = Grid v

-- | @x + z <= y + 1@ says: a window of @z@ taken at any of the @x@ offsets
-- still fits inside the source of size @y@.
--
-- This was previously written @z <= x - y + 1@, which has @x@ and @y@ the wrong
-- way round. It only ever typechecked because the sole test case used
-- @x == y == 3@, where both readings collapse to @z <= 1@; the honest case of
-- windowing a 5-grid into three positions of 3 was rejected. Stating it as an
-- addition rather than a truncating subtraction also keeps it in reach of the
-- Nat solver.
--
-- @KnownNat x@ is new: 'asSizeProxy' recovers the offset's type-level value by
-- comparing against the coord's size at runtime, now that an
-- 'SizedGrid.Ordinal.Ordinal' no longer carries that dictionary in every value.
instance ( KnownNat x
         , KnownNat z
         , AllSizedKnown as
         , IsCoord c
         , ShrinkableGrid cs as bs
         , x + z <= y + 1
         ) =>
         ShrinkableGrid (c x ': cs) (c y ': as) (c z ': bs) where
    shrinkGrid (c :| cs) =
        combineGrid . fmap (shrinkGrid cs) . helper . splitGrid
      where
        helper :: Grid '[ c y] a -> Grid '[ c z] a
        helper g =
            asSizeProxy c $ \(pTake :: Proxy n) ->
                case windowFits @n @y @z of
                    Dict -> takeGrid (Proxy :: Proxy z) (dropGrid pTake g)


-- | Cut a grid into disjoint tiles along its outermost axis: an @Ordinal 9@
-- axis tiled by @Ordinal 3@ gives three tiles, not seven overlapping windows.
-- The tiles partition the source, so concatenating them reproduces it.
--
-- This was called @gridWindows@, which said the opposite of what it does. A
-- sliding window and a disjoint tiling are different operations; this is the
-- tiling. Sliding windows are 'shrinkGrid' at each offset (sized-grid-3t6).
--
-- @CoordNat big \`Mod\` CoordNat small ~ 0@ makes a tiling that does not divide
-- evenly a type error, so the result is always exactly
-- @CoordNat big \`Div\` CoordNat small@ tiles with no short remainder.
--
-- To tile along the /second/ axis, reach for 'zipLowerDim' and not
-- 'mapLowerDim':
--
-- > rows    = gridTiles                :: Board -> [Grid '[Ordinal 1, Ordinal 9] a]
-- > columns = zipLowerDim gridTiles    :: Board -> [Grid '[Ordinal 9, Ordinal 1] a]
gridTiles :: forall small big rest a.
               ( KnownNat (MaxCoordSize (small ': rest)),
                 CoordNat big `Mod` CoordNat small ~ 0
               )
            => Grid (big ': rest) a
            -> [Grid (small ': rest) a]
gridTiles (Grid v) =
    requiring @(CoordNat big `Mod` CoordNat small ~ 0) $
    let size = fromIntegral $ natVal (Proxy @(MaxCoordSize (small ': rest)))
    in map Grid $ splitVectorBySize size v