-- GHC2024 plus the default-extensions in grid-sized.cabal cover everything this
-- module used to list.

-- |
-- Module      :  Data.Grid.Sized.Internal.Grid
-- License     :  MIT -style (see the file LICENSE)
--
-- The `Grid` representation and everything defined over it.
--
-- This module is hidden. It exists so that the `Grid` constructor can be shared
-- with "Data.Grid.Sized.Unsafe" without also being shared with the world: the
-- one invariant this library exists to enforce is that a @Grid cs a@ holds
-- exactly @MaxCoordSize cs@ elements, and an exported constructor is a licence
-- to break it. "Data.Grid.Sized" re-exports the safe half of what is here.
--
-- Everything below is free to use the constructor directly. The obligation that
-- comes with that is on each function in turn: it must not change the length of
-- the vector except in step with the type.
module Data.Grid.Sized.Internal.Grid
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
  , AllGridSizeKnown(..)
  , GridSizeProof(..)
    -- * Rearranging
  , transposeGrid
  , splitGrid
  , combineGrid
  , combineHigherDim
  , splitHigherDim
  , dropGrid
  , takeGrid
  , sliceGrid
  , mapLowerDim
  , zipLowerDim
  , scanl1Grid
    -- * Windows and tiles
  , ShrinkableGrid(..)
  , gridTiles
    -- * Vector helpers
  , splitVectorBySize
  ) where

import           Data.Grid.Sized.Coord
import           Data.Grid.Sized.Coord.Class
import           Data.Grid.Sized.Internal.Type (requiring, windowFits)

import           Control.Applicative   (ZipList (..))
import           Control.Lens          hiding (index)
import           Data.Aeson
import           Data.Constraint
import           Data.Distributive
import           Data.Functor.Classes
import           Data.Functor.Rep
import           Data.Proxy            (Proxy (..))
import qualified Data.Vector           as V
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

-- | The escape hatch, re-exported from "Data.Grid.Sized.Unsafe".
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

instance (AllSizedKnown cs, IsCoordList cs) =>
         Monad (Grid cs) where
  g >>= f = imap (\p a -> f a `index` p) g

instance (AllSizedKnown cs, IsCoordList cs) =>
         Distributive (Grid cs) where
  distribute = distributeRep

instance (IsCoordList cs, AllSizedKnown cs) =>
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
-- callers almost always apply to the coordinate they are handed. That one is
-- now gone too: the fold moved into `IsCoordList`, so it unrolls and
-- constant-folds at a concrete axis list. `ifoldl'` over this grid went from
-- 30 MB to 2.7 MB, and `imap` from 35 MB to 7.6 MB, measured.
instance (IsCoordList cs) => FunctorWithIndex (Coord cs) (Grid cs) where
  imap func (Grid v) = Grid $ V.zipWith func (V.fromList allCoord) v

-- | 'ifoldr' and 'ifoldl'' are given outright because the class otherwise
-- builds them out of 'ifoldMap' and an 'Endo' chain, which is a closure per
-- cell on top of the coordinate.
instance (IsCoordList cs) => FoldableWithIndex (Coord cs) (Grid cs) where
  ifoldMap func (Grid v) = foldMap id $ V.zipWith func (V.fromList allCoord) v
  ifoldr func z (Grid v) = V.foldr ($) z $ V.zipWith func (V.fromList allCoord) v
  ifoldl' func z (Grid v) =
    V.foldl' (&) z $ V.zipWith (\c x acc -> func c acc x) (V.fromList allCoord) v

instance (IsCoordList cs) => TraversableWithIndex (Coord cs) (Grid cs) where
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

-- | Evidence that every axis of @cs@ has a statically known size, and so does
-- every suffix of @cs@ -- which is what a function recursing down the axis list
-- needs.
--
-- This was a type family until sized-grid-k6n, and the difference is entirely
-- about who does the work. A family does no solving: it expanded to a
-- conjunction containing @KnownNat (MaxCoordSize cs)@ and that raw goal landed
-- in the caller's context, where at @cs ~ '[Clamped n, Clamped n]@ it reads
-- @KnownNat (n * (n * 1))@. GHC cannot get that from @KnownNat n@, so the
-- caller wrote it out by hand:
--
-- > parse :: (KnownNat n, KnownNat (n * n)) => String -> Maybe (Grid '[Clamped n, Clamped n] Cell)
--
-- As a class the same obligation is discharged during instance resolution,
-- inductively, from the per-axis 'GHC.KnownNat's -- so @KnownNat n@ alone now
-- suffices at the call site. 'Data.Grid.Sized.Coord.AllSizedKnown' has always been a
-- class for exactly this reason; this is the same treatment applied to the
-- structural recursions.
--
-- A type-checker plugin cannot substitute for this. @-fplugin@ is not
-- transitive: @ghc-typelits-knownnat@ being enabled for this library says
-- nothing about the consumer, which solves its own constraints. That is why
-- sized-grid-h56 could not fix this and an API change had to.
class GHC.KnownNat (MaxCoordSize cs) => AllGridSizeKnown (cs :: [Type]) where
  -- | The size of the head axis and the tail's own instance.
  --
  -- The tail's dictionary has to be carried in the value: a class dictionary
  -- cannot be run backwards through its own instance context, so there is
  -- otherwise no way to recover @AllGridSizeKnown xs@ from
  -- @AllGridSizeKnown (x ': xs)@.
  gridSizeProof :: GridSizeProof cs

-- | What 'AllGridSizeKnown' carries, and the reason the recursions below no
-- longer ask for @SListI cs@: matching on this refines @cs@ to nil or cons just
-- as @Generics.SOP.Shape cs@ did, and brings the evidence for that shape into
-- scope at the same time, which a @Shape@ could not do.
data GridSizeProof (cs :: [Type]) where
  GridSizeNil :: GridSizeProof '[]
  GridSizeCons ::
       forall c n cs. (GHC.KnownNat n, AllGridSizeKnown cs)
    => GridSizeProof (c n ': cs)

instance AllGridSizeKnown '[] where
  gridSizeProof = GridSizeNil

-- | @KnownNat (MaxCoordSize (c n ': as))@ is @KnownNat (n * MaxCoordSize as)@,
-- which @ghc-typelits-knownnat@ derives from the @KnownNat n@ here and the
-- @KnownNat (MaxCoordSize as)@ that is this class's own superclass on the tail.
-- That plugin step is the induction, and it happens here rather than at the
-- call site.
instance (GHC.KnownNat n, AllGridSizeKnown as) =>
         AllGridSizeKnown ((c n) ': as) where
  gridSizeProof = GridSizeCons

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
     forall cs a. AllGridSizeKnown cs
  => Grid cs a
  -> CollapseGrid cs a
collapseGrid (Grid v) =
  case gridSizeProof @cs of
    GridSizeNil -> v V.! 0
    GridSizeCons @_ @_ @xs ->
      map (collapseGrid . Grid @xs) $
      splitVectorBySize
        (fromIntegral $ GHC.natVal (Proxy @(MaxCoordSize xs)))
        v

-- | Convert a series of nested lists to a grid. If the size of the grid does not match the size of lists this will be `Nothing`
gridFromList ::
     forall cs a. AllGridSizeKnown cs
  => CollapseGrid cs a
  -> Maybe (Grid cs a)
gridFromList cg =
  case gridSizeProof @cs of
    GridSizeNil -> Just $ Grid $ V.singleton $ cg
    GridSizeCons @_ @n @xs ->
      if length cg == fromIntegral (GHC.natVal (Proxy @n))
        then Grid . mconcat <$>
             traverse (fmap unGrid . gridFromList @xs) cg
        else Nothing

instance (AllGridSizeKnown cs, ToJSON a) => ToJSON (Grid cs a) where
  toJSON (Grid v) =
    case gridSizeProof @cs of
      GridSizeNil -> toJSON (v V.! 0)
      GridSizeCons @_ @_ @xs ->
        toJSON $
        map (toJSON . Grid @xs) $
        splitVectorBySize
          (fromIntegral $ GHC.natVal (Proxy @(MaxCoordSize xs)))
          v

-- | Decoding validates the length at every dimension, so a successfully decoded
-- grid always satisfies @V.length (unGrid g) == MaxCoordSize cs@. Without the
-- check a short or ragged array decoded to a `Grid` whose vector disagreed with
-- its type, which then made `index` throw and ('<*>') silently truncate.
--
-- The constraints match `ToJSON`\'s: the `KnownNat` evidence is what makes the
-- check possible.
instance (AllGridSizeKnown cs, FromJSON a) =>
         FromJSON (Grid cs a) where
  parseJSON v =
    case gridSizeProof @cs of
      GridSizeNil -> Grid . V.singleton <$> parseJSON v
      GridSizeCons @_ @n @xs -> do
        a :: [Grid xs a] <- parseJSON v
        let expected = fromIntegral $ GHC.natVal (Proxy @n) :: Int
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
-- comes from @CoordNat@ on the `Data.Grid.Sized.Coord.Class.IsCoordLifted` instance,
-- not from `IsCoord`, so the class could not have justified the @n + m@ in the
-- result even in principle.
combineHigherDim ::
       Grid (c n ': as) x
    -> Grid (c m ': as) x
    -> Grid (c (n + m) ': as) x
combineHigherDim (Grid v1) (Grid v2) = Grid (v1 <> v2)

-- | Drop the first @n@ elements of a one-dimensional grid:
--
-- > dropGrid 2 g   -- rather than dropGrid (Proxy @2) g
--
-- @n <= m@ is required: without it @dropGrid 9@ of a 3-grid typechecked and
-- produced a grid whose vector was empty while its type claimed @3 - 9@.
dropGrid ::
       forall m c x. forall n -> (KnownNat n, n <= m)
    => Grid '[ c m] x
    -> Grid '[ c (m - n)] x
dropGrid n (Grid v) =
    requiring @(n <= m) $ Grid $ V.drop (fromIntegral $ natVal (Proxy @n)) v

-- | Keep the first @n@ elements of a one-dimensional grid:
--
-- > takeGrid 2 g   -- rather than takeGrid (Proxy @2) g
--
-- @n <= m@ is required: 'V.take' cannot conjure elements, so without the
-- constraint @takeGrid 9@ of a 3-grid returned 3 elements under a type that
-- promised 9.
takeGrid ::
       forall m c x. forall n -> (KnownNat n, n <= m)
    => Grid '[ c m] x
    -> Grid '[ c n] x
takeGrid n (Grid v) =
    requiring @(n <= m) $ Grid $ V.take (fromIntegral $ natVal (Proxy @n)) v

-- | Keep @len@ elements of a one-dimensional grid starting at offset @off@:
--
-- > sliceGrid 1 2 g   -- elements 1 and 2 of a 3-grid
--
-- This is @takeGrid len . dropGrid off@ with the intermediate size fused away,
-- and the fusion is the entire point (sized-grid-wrc). Composed, the two state
-- the window bound as @len <= m - off@ over GHC's /truncating/ subtraction;
-- when @off@ is an existential -- exactly the case in 'shrinkGrid', where it
-- comes from 'reifyCoord' -- that is out of reach of ghc-typelits-natnormalise,
-- and it used to be supplied by an @unsafeCoerce@ axiom. Written @off + len <= m@
-- the same fact is ordinary linear arithmetic, the solver discharges it, and
-- the axiom is gone without taking on another type-checker plugin.
--
-- @off + len <= m@ is also precisely 'V.slice'\'s own precondition, so the
-- bounds check it performs can never fire here.
sliceGrid ::
       forall m c x. forall off len -> ( KnownNat off
                                       , KnownNat len
                                       , off + len <= m)
    => Grid '[ c m] x
    -> Grid '[ c len] x
sliceGrid off len (Grid v) =
    requiring @(off + len <= m) $
    Grid $
    V.slice
        (fromIntegral $ natVal (Proxy @off))
        (fromIntegral $ natVal (Proxy @len))
        v

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
-- @KnownNat x@ is new: 'reifyCoord' recovers the offset's type-level value by
-- comparing against the coord's size at runtime, now that an
-- 'Data.Grid.Sized.Ordinal.Ordinal' no longer carries that dictionary in every value.
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
            reifyCoord c $ \n ->
                withDict (windowFits @n @x @y @z) $ sliceGrid n z g


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