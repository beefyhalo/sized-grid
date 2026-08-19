{-# LANGUAGE AllowAmbiguousTypes #-}

-- | The `GridOf` representation and everything defined over it.
module Data.Grid.Sized.Internal.Grid
  ( -- * Representation
    GridOf(..)
  , Grid
  , unsafeGridFromVector
    -- * Construction and access
  , gridVector
  , gridFromVector
  , gridFromList
  , collapseGrid
    -- * Bulk operations
    --
    -- $bulk
  , tabulateGrid
  , indexGrid
  , mapGrid
  , imapGrid
  , zipWithGrid
  , foldlGrid'
  , scanl1Grid
    -- * Type-level machinery
  , CollapseGrid
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
  , MapAxis(..)
  , mapAxis
  , scanAxis
    -- * Windows and tiles
  , ShrinkableGrid(..)
  , gridTiles
  , gridWindows
    -- * Vector helpers
  , splitVectorBySize
  ) where

import           Data.Grid.Sized.Coord
import           Data.Grid.Sized.Coord.Class
import           Data.Grid.Sized.Internal.Type (requiring, windowFits)

import           Control.Applicative           (ZipList (..))
import           Control.DeepSeq               (NFData)
import           Control.Lens                  hiding (index)
import           Data.Aeson
import           Data.Aeson.Types              (Parser)
import           Data.Constraint
import           Data.Distributive
import           Data.Foldable                 (fold)
import           Data.Functor.Classes
import           Data.Functor.Rep
import           Data.Kind                     (Type)
import           Data.Proxy                    (Proxy (..))
import qualified Data.Vector                   as V
import qualified Data.Vector.Generic           as VG
import qualified GHC.Generics                  as GHC
import           GHC.TypeLits
import qualified GHC.TypeLits                  as GHC

-- | The @unGrid@ field is not exported: a record field in scope would permit
-- record-update syntax, which could break the length invariant.
newtype GridOf v (cs :: [Type]) a = Grid
  { unGrid :: v a
  } deriving stock (GHC.Generic)

-- | Kept nullary so it stays partially applicable, e.g. @'Functor' ('Grid' cs)@.
type Grid = GridOf V.Vector

deriving stock instance Eq (v a) => Eq (GridOf v cs a)

deriving stock instance Show (v a) => Show (GridOf v cs a)

deriving newtype instance NFData (v a) => NFData (GridOf v cs a)

deriving newtype instance Eq1 v => Eq1 (GridOf v cs)

deriving newtype instance Show1 v => Show1 (GridOf v cs)

deriving newtype instance Functor v => Functor (GridOf v cs)

deriving newtype instance Foldable v => Foldable (GridOf v cs)

-- | Written by hand: @GeneralizedNewtypeDeriving@ can't coerce under the
-- applicative parameter.
instance Traversable v => Traversable (GridOf v cs) where
  traverse f (Grid v) = Grid <$> traverse f v

-- | Asserts, rather than checks, that the vector holds exactly
-- @MaxCoordSize cs@ elements. Re-exported from "Data.Grid.Sized.Unsafe".
-- Nothing verifies it, and a grid that fails it makes `indexGrid` read
-- whatever is at that offset in memory instead of bounds-checking. Only
-- construction needs to be unsafe; reading back out is `gridVector`, kept
-- separate and public.
unsafeGridFromVector :: v a -> GridOf v cs a
unsafeGridFromVector = Grid
{-# INLINE unsafeGridFromVector #-}

-- | Elements in row-major order.
gridVector :: GridOf v cs a -> v a
gridVector = unGrid
{-# INLINE gridVector #-}

gridFromVector ::
       forall v cs a. (VG.Vector v a, AllSizedKnown cs)
    => v a
    -> Maybe (GridOf v cs a)
gridFromVector v =
    if VG.length v == fromIntegral (GHC.natVal (Proxy :: Proxy (MaxCoordSize cs)))
        then Just (Grid v)
        else Nothing
{-# INLINABLE gridFromVector #-}

-- $bulk
-- Operations that carry an element constraint and so cannot be class methods.

-- | `tabulate` for grids that cannot be `Representable`. Uses `VG.fromListN`,
-- not `VG.fromList`, since the length is known up front.
tabulateGrid ::
       forall v cs a. (VG.Vector v a, IsCoordList cs)
    => (Coord cs -> a)
    -> GridOf v cs a
tabulateGrid func = Grid $ VG.fromListN (coordSpaceSize @cs) $ map func allCoord
{-# INLINABLE tabulateGrid #-}

-- | Read the element at a coordinate. `Data.Functor.Rep.index` for grids
-- whose element type cannot support `Representable`. Uses 'VG.unsafeIndex':
-- @coordPosition c@ is in range by the `Ordinal` invariant on every axis and
-- the vector has exactly that many elements by `GridOf`'s size invariant,
-- so the bounds check can never fire on a grid built through the safe API
-- -- only `unsafeGridFromVector` can falsify that.
indexGrid ::
       forall v cs a. (VG.Vector v a, IsCoordList cs)
    => GridOf v cs a
    -> Coord cs
    -> a
indexGrid (Grid v) c = VG.unsafeIndex v (coordPosition c)
{-# INLINE indexGrid #-}

mapGrid ::
       (VG.Vector v a, VG.Vector v b)
    => (a -> b)
    -> GridOf v cs a
    -> GridOf v cs b
mapGrid f (Grid v) = Grid (VG.map f v)
{-# INLINE mapGrid #-}

-- | Walks the coordinate list alongside the vector rather than materialising
-- it; see the `FunctorWithIndex` instance below.
imapGrid ::
       forall v cs a b. (VG.Vector v a, VG.Vector v b, IsCoordList cs)
    => (Coord cs -> a -> b)
    -> GridOf v cs a
    -> GridOf v cs b
imapGrid f (Grid v) = Grid (VG.imap (\i x -> f (allCoordVector VG.! i) x) v)
  where
    allCoordVector :: V.Vector (Coord cs)
    allCoordVector = V.fromListN (coordSpaceSize @cs) allCoord
{-# INLINABLE imapGrid #-}

-- | Pointwise combination of two grids of the same shape.
zipWithGrid ::
       (VG.Vector v a, VG.Vector v b, VG.Vector v c)
    => (a -> b -> c)
    -> GridOf v cs a
    -> GridOf v cs b
    -> GridOf v cs c
zipWithGrid f (Grid a) (Grid b) = Grid (VG.zipWith f a b)
{-# INLINE zipWithGrid #-}

-- | Strict left fold in row-major order. `Data.Foldable.foldl'` for grids whose
-- element type cannot support `Foldable`.
foldlGrid' :: VG.Vector v a => (b -> a -> b) -> b -> GridOf v cs a -> b
foldlGrid' f z (Grid v) = VG.foldl' f z v
{-# INLINE foldlGrid' #-}

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
-- guaranteed by 'VG.scanl1'', so no size constraint is needed.
scanl1Grid :: VG.Vector v a => (a -> a -> a) -> GridOf v cs a -> GridOf v cs a
scanl1Grid f (Grid v) = Grid (VG.scanl1' f v)
{-# INLINE scanl1Grid #-}

-- | Boxed only, and necessarily so: `pure` must produce a grid of /any/ element
-- type, which no unboxed vector can hold. 'tabulateGrid' is the unboxed
-- counterpart for the cases that have a concrete element type in hand.
instance AllSizedKnown cs => Applicative (Grid cs) where
    pure =
        Grid . V.replicate (fromIntegral $ GHC.natVal (Proxy :: Proxy (MaxCoordSize cs)))
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
  -- The bodies are 'tabulateGrid' and 'indexGrid' at @v ~ V.Vector@, where the
  -- 'VG.Vector' constraint is discharged for every element type.
  --
  -- The traversals below deliberately do not use 'V.fromListN', and the
  -- difference is that 'tabulate' ends in a vector whatever happens. They do
  -- not: they hand their list straight to a 'V.zipWith' that fuses with it.
  tabulate = tabulateGrid
  index = indexGrid

-- | @V.fromList allCoord@ looks like it materialises the whole coordinate
-- list on every traversal, but it does not: 'V.zipWith' fuses with it, so
-- coordinates are produced and consumed one at a time and die in the
-- nursery. Replacing it with anything the simplifier cannot see through
-- forces all of them to be live at once -- measured 23% more allocation and
-- 59% slower. Leave it alone.
instance (IsCoordList cs) => FunctorWithIndex (Coord cs) (Grid cs) where
  imap func (Grid v) = Grid $ V.zipWith func (V.fromList allCoord) v

-- | 'ifoldr' and 'ifoldl'' are given outright because the class otherwise
-- builds them out of 'ifoldMap' and an 'Endo' chain, which is a closure per
-- cell on top of the coordinate.
instance (IsCoordList cs) => FoldableWithIndex (Coord cs) (Grid cs) where
  ifoldMap func (Grid v) = fold $ V.zipWith func (V.fromList allCoord) v
  ifoldr func z (Grid v) = V.foldr ($) z $ V.zipWith func (V.fromList allCoord) v
  ifoldl' func z (Grid v) =
    V.foldl' (&) z $ V.zipWith (\c x acc -> func c acc x) (V.fromList allCoord) v

instance (IsCoordList cs) => TraversableWithIndex (Coord cs) (Grid cs) where
  itraverse func (Grid v) =
    Grid <$> sequenceA (V.zipWith func (V.fromList allCoord) v)

-- | Given a grid type, give back a series of nested lists repesenting the grid. The lists will have a number of layers equal to the dimensionality.
type family CollapseGrid cs a where
  CollapseGrid '[] a = a
  CollapseGrid (c ': cs) a = [CollapseGrid cs a]

-- | Convert a vector into a list of `Data.Vector.Generic.Vector`s, where all the
-- elements of the list have the given size.
--
-- If @n@ does not divide the length, the final chunk is short. Every caller in
-- this module is protected from that by the size invariant on 'GridOf', so the
-- short chunk is unreachable for them -- but it is a silent malformation rather
-- than a failure, so do not rely on it.
--
-- A size of zero would otherwise loop forever taking empty prefixes.
splitVectorBySize :: VG.Vector v a => Int -> v a -> [v a]
splitVectorBySize n v
  | n <= 0    = error $ "splitVectorBySize: chunk size must be positive, got " ++ show n
  | otherwise = [ VG.slice i (min n (len - i)) v | i <- [0, n .. len - 1] ]
  where
    len = VG.length v
{-# INLINABLE splitVectorBySize #-}

-- $recursion
--
-- 'collapseGrid', 'gridFromList' and the two JSON instances all recurse
-- /down the axis list/. A naive recursion keeping the grid generic in @v@
-- needs a fresh @'VG.Vector' v a@ dictionary at each level, which GHC will
-- not specialise through (an @INLINABLE@ pragma does not help, measured),
-- leaving every 'VG.take'\/'VG.drop'\/'VG.concat' as an indirect call --
-- 60-300% slower than a monomorphic version. So the recursion here is
-- monomorphic on boxed "Data.Vector" ('splitBoxedBySize'), converting at
-- the boundary with 'VG.convert'; plain-list recursion and closure-passing
-- were both measured and are worse.
--
-- 'collapseGrid' and 'gridFromList' get the boxed case back for free via a
-- RULE matching @v ~ "Data.Vector".Vector@, bypassing 'VG.convert' entirely
-- (GHC's own fusion turns a boxed-to-boxed 'VG.convert' into a 'clone', not
-- a no-op, so this has to be done explicitly). The same trick can't reach
-- 'toJSON'\/'parseJSON': as class methods, a RULE only sees the opaque
-- 'ToJSON'\/'FromJSON' dictionary at the call site, with no way to recover
-- the underlying @'VG.Vector' v a@ from it.
--
-- Keep it this way: growing a @'VG.Vector' v a@ constraint on the recursive
-- helper, or switching to the exported generic 'splitVectorBySize', brings
-- the regression straight back.

-- | 'splitVectorBySize' at a boxed vector, for the recursions below.
--
-- Written out rather than calling the exported generic one, and this is the
-- single change that mattered: inside a function that is itself
-- polymorphically recursive, the generic version is reached through a
-- @'VG.Vector' V.Vector a@ dictionary and its 'VG.take' and 'VG.drop' never
-- reduce to the O(1) slice they are. Here they do. Restoring this one helper
-- took 'collapseGrid' from 83% above baseline back to level.
splitBoxedBySize :: Int -> V.Vector a -> [V.Vector a]
splitBoxedBySize n v
  | n <= 0    = error $ "splitBoxedBySize: chunk size must be positive, got " ++ show n
  | otherwise = [ V.slice i (min n (len - i)) v | i <- [0, n .. len - 1] ]
  where
    len = V.length v

-- | The axis-list recursion of 'collapseGrid', at a concrete boxed vector.
nestByShape :: forall cs a. AllSizedKnown cs => V.Vector a -> CollapseGrid cs a
nestByShape v =
  case sizeProof @cs of
    SizeNil -> v V.! 0
    SizeCons @_ @_ @rest ->
      map (nestByShape @rest) $
      splitBoxedBySize (fromIntegral $ GHC.natVal (Proxy @(MaxCoordSize rest))) v

-- | The axis-list recursion of 'gridFromList', flattening to row-major order
-- and checking the length at every dimension on the way.
flattenByShape ::
     forall cs a. AllSizedKnown cs
  => CollapseGrid cs a
  -> Maybe (V.Vector a)
flattenByShape cg =
  case sizeProof @cs of
    SizeNil -> Just $ V.singleton cg
    SizeCons @_ @n @rest ->
      if length cg == fromIntegral (GHC.natVal (Proxy @n))
        then V.concat <$> traverse (flattenByShape @rest) cg
        else Nothing

-- | Convert a grid to a series of nested lists. This removes type level information, but it is sometimes easier to work with lists
collapseGrid ::
     forall v cs a. (VG.Vector v a, AllSizedKnown cs)
  => GridOf v cs a
  -> CollapseGrid cs a
collapseGrid (Grid v) = nestByShape @cs (VG.convert v)
{-# INLINABLE [1] collapseGrid #-}

-- | At a boxed grid, 'VG.convert' in 'collapseGrid' is a copy of a vector to
-- itself; this rule bypasses it when GHC can see @v ~ V.Vector@ at the call
-- site (see \"Recursing down the axis list\"). It does nothing for other
-- vector types, which still need the real conversion.
--
-- Phase control matters here the way it does for @map@\/@build@ fusion in
-- "Data.List": unphased, 'collapseGrid' is small enough that the simplifier
-- inlines its body before this rule gets a chance to match, silently
-- disarming it (GHC warns "may never fire" for exactly this reason). Marking
-- 'collapseGrid' @[1]@ (inlinable only from phase 1) and the rule @[~1]@
-- (active only before phase 1, i.e. in phase 2) gives the rule first refusal;
-- 'collapseGrid' still inlines normally from phase 1 on for every @v@ the
-- rule does not match.
{-# RULES
"collapseGrid/boxed" [~1] forall (g :: GridOf V.Vector cs a).
  collapseGrid g = nestByShape @cs (unGrid g)
  #-}

-- | Convert a series of nested lists to a grid. If the size of the grid does not match the size of lists this will be `Nothing`
gridFromList ::
     forall v cs a. (VG.Vector v a, AllSizedKnown cs)
  => CollapseGrid cs a
  -> Maybe (GridOf v cs a)
gridFromList cg = Grid . VG.convert <$> flattenByShape @cs cg
{-# INLINABLE [1] gridFromList #-}

-- | As 'collapseGrid/boxed', for the other direction. @v@ only appears in the
-- /result/ here, so it is pinned with an explicit type application rather than
-- an argument annotation.
{-# RULES
"gridFromList/boxed" [~1] forall (cg :: CollapseGrid cs a).
  gridFromList @V.Vector @cs @a cg = Grid <$> flattenByShape @cs cg
  #-}

instance (VG.Vector v a, AllSizedKnown cs, ToJSON a) =>
         ToJSON (GridOf v cs a) where
  toJSON (Grid v) = nestedToJSON @cs (VG.convert v)

-- | 'toJSON' for a grid, at a concrete boxed vector. Separate from the instance
-- for the reason given under \"Recursing down the axis list\": the recursion
-- must not carry the vector parameter.
nestedToJSON ::
     forall cs a. (AllSizedKnown cs, ToJSON a)
  => V.Vector a
  -> Value
nestedToJSON v =
  case sizeProof @cs of
    SizeNil -> toJSON (v V.! 0)
    SizeCons @_ @_ @rest ->
      toJSON $
      map (nestedToJSON @rest) $
      splitBoxedBySize (fromIntegral $ GHC.natVal (Proxy @(MaxCoordSize rest))) v

-- | Decoding validates the length at every dimension, so a successfully decoded
-- grid always satisfies @VG.length (gridVector g) == MaxCoordSize cs@. Without
-- the check a short or ragged array decoded to a `GridOf` whose vector
-- disagreed with its type, which then made `index` throw and ('<*>') silently
-- truncate.
--
-- The constraints match `ToJSON`\'s: the `KnownNat` evidence is what makes the
-- check possible.
instance (VG.Vector v a, AllSizedKnown cs, FromJSON a) =>
         FromJSON (GridOf v cs a) where
  parseJSON val = Grid . VG.convert <$> nestedParseJSON @cs val

-- | 'parseJSON' for a grid, producing the flat row-major vector. Separate from
-- the instance so the recursion does not carry the vector parameter; see
-- \"Recursing down the axis list\".
nestedParseJSON ::
     forall cs a. (AllSizedKnown cs, FromJSON a)
  => Value
  -> Parser (V.Vector a)
nestedParseJSON val =
  case sizeProof @cs of
    SizeNil -> V.singleton <$> parseJSON val
    SizeCons @_ @n @rest -> do
      vals :: [Value] <- parseJSON val
      let expected = fromIntegral $ GHC.natVal (Proxy @n) :: Int
      if length vals == expected
        then V.concat <$> traverse (nestedParseJSON @rest) vals
        else fail $
             "Grid: expected " ++
             show expected ++ " elements, got " ++ show (length vals)

transposeGrid ::
     ( VG.Vector v a
     , IsCoord h
     , IsCoord w
     , GHC.KnownNat x
     , GHC.KnownNat y
     , 1 <= y
     , 1 <= x
     )
  => GridOf v '[ w x, h y] a
  -> GridOf v '[ h y, w x] a
transposeGrid g = tabulateGrid (indexGrid g . tranposeCoord)
{-# INLINABLE transposeGrid #-}

-- | The outer grid holds grids, and a grid is never an unboxed element, so the
-- outer vector is boxed whatever @v@ is. That asymmetry is what makes the whole
-- shape algebra shareable: only the /inner/ representation follows @v@, and
-- 'combineGrid' puts it back.
splitGrid ::
       forall v c cs a. (VG.Vector v a, AllSizedKnown cs)
    => GridOf v (c ': cs) a
    -> Grid '[ c] (GridOf v cs a)
splitGrid (Grid v) =
    Grid $
    V.fromList $
    map
        Grid
        (splitVectorBySize
             (fromIntegral $ GHC.natVal (Proxy :: Proxy (MaxCoordSize cs)))
             v)
{-# INLINABLE splitGrid #-}

combineGrid ::
       forall v c cs a. VG.Vector v a
    => Grid '[ c] (GridOf v cs a)
    -> GridOf v (c ': cs) a
combineGrid (Grid v) = Grid $ VG.concat $ map unGrid $ V.toList v
{-# INLINE combineGrid #-}

-- | @IsCoord c@ used to be demanded here. It buys nothing: the size of a coord
-- comes from @CoordNat@ on the `Data.Grid.Sized.Coord.Class.IsCoordLifted` instance,
-- not from `IsCoord`, so the class could not have justified the @n + m@ in the
-- result even in principle.
combineHigherDim ::
       VG.Vector v x
    => GridOf v (c n ': as) x
    -> GridOf v (c m ': as) x
    -> GridOf v (c (n + m) ': as) x
combineHigherDim (Grid v1) (Grid v2) = Grid (v1 VG.++ v2)
{-# INLINE combineHigherDim #-}

-- | Drop the first @n@ elements of a one-dimensional grid:
--
-- > dropGrid 2 g   -- rather than dropGrid (Proxy @2) g
--
-- @n <= m@ is required: without it @dropGrid 9@ of a 3-grid typechecked and
-- produced a grid whose vector was empty while its type claimed @3 - 9@.
dropGrid ::
       forall v m c x. forall n -> (VG.Vector v x, KnownNat n, n <= m)
    => GridOf v '[ c m] x
    -> GridOf v '[ c (m - n)] x
dropGrid n (Grid v) =
    requiring @(n <= m) $ Grid $ VG.drop (fromIntegral $ natVal (Proxy @n)) v
{-# INLINABLE dropGrid #-}

-- | Keep the first @n@ elements of a one-dimensional grid:
--
-- > takeGrid 2 g   -- rather than takeGrid (Proxy @2) g
--
-- @n <= m@ is required: 'VG.take' cannot conjure elements, so without the
-- constraint @takeGrid 9@ of a 3-grid returned 3 elements under a type that
-- promised 9.
takeGrid ::
       forall v m c x. forall n -> (VG.Vector v x, KnownNat n, n <= m)
    => GridOf v '[ c m] x
    -> GridOf v '[ c n] x
takeGrid n (Grid v) =
    requiring @(n <= m) $ Grid $ VG.take (fromIntegral $ natVal (Proxy @n)) v
{-# INLINABLE takeGrid #-}

-- | Keep @len@ elements of a one-dimensional grid starting at offset @off@:
--
-- > sliceGrid 1 2 g   -- elements 1 and 2 of a 3-grid
--
-- This is @takeGrid len . dropGrid off@ with the intermediate size fused
-- away. Composed, the two state the window bound as @len <= m - off@ over
-- GHC's truncating subtraction, out of reach of ghc-typelits-natnormalise
-- once @off@ is an existential (as it is in 'shrinkGrid', from
-- 'reifyCoord'). Written @off + len <= m@ instead, it's ordinary linear
-- arithmetic the solver discharges directly.
--
-- @off + len <= m@ is also precisely 'VG.slice'\'s own precondition, so the
-- bounds check it performs can never fire here.
sliceGrid ::
       forall v m c x. forall off len -> ( VG.Vector v x
                                         , KnownNat off
                                         , KnownNat len
                                         , off + len <= m)
    => GridOf v '[ c m] x
    -> GridOf v '[ c len] x
sliceGrid off len (Grid v) =
    requiring @(off + len <= m) $
    Grid $
    VG.slice
        (fromIntegral $ natVal (Proxy @off))
        (fromIntegral $ natVal (Proxy @len))
        v
{-# INLINABLE sliceGrid #-}

-- | The second component is @x - y@, not a free type variable. It used to be
-- free, which let the caller annotate the remainder with any size at all and
-- get a grid whose vector did not match.
splitHigherDim ::
       forall v c as x y a.
       ( VG.Vector v a
       , KnownNat y
       , y <= x
       , AllSizedKnown as
       )
    => GridOf v (c x ': as) a
    -> (GridOf v (c y ': as) a, GridOf v (c (x - y) ': as) a)
splitHigherDim (Grid v) =
    requiring @(y <= x) $
    let (a, b) =
            VG.splitAt
                (fromIntegral $
                 GHC.natVal (Proxy @y) * GHC.natVal (Proxy @(MaxCoordSize as)))
                v
     in (Grid a, Grid b)
{-# INLINABLE splitHigherDim #-}

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
--
-- The element type may change, so both @v x@ and @v y@ have to be vectors.
mapLowerDim ::
       forall v as bs x y c f.
       (VG.Vector v x, VG.Vector v y, AllSizedKnown as, Applicative f)
    => (GridOf v as x -> f (GridOf v bs y))
    -> GridOf v (c ': as) x
    -> f (GridOf v (c ': bs) y)
mapLowerDim f (Grid v) =
    fmap (Grid . VG.concat) $
    traverse (fmap unGrid . f . Grid) $
    splitVectorBySize (fromIntegral (GHC.natVal (Proxy @(MaxCoordSize as)))) v
{-# INLINABLE mapLowerDim #-}

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
       forall v as bs x y c. (VG.Vector v x, VG.Vector v y, AllSizedKnown as)
    => (GridOf v as x -> [GridOf v bs y])
    -> GridOf v (c ': as) x
    -> [GridOf v (c ': bs) y]
zipLowerDim f = getZipList . mapLowerDim (ZipList . f)
{-# INLINABLE zipLowerDim #-}

-- | Transpose a flat, row-major vector holding an @rows × cols@ matrix into
-- its @cols × rows@ transpose. Pure index arithmetic -- unlike 'transposeGrid'
-- this needs no coordinate machinery, because by the time 'mapAxis' reaches
-- for it the axis being moved has already been reduced to a size, not a
-- coordinate type.
--
-- 'VG.unsafeIndex': @i@ ranges over @[0, rows * cols)@ and @(c, r)@ is its
-- @'divMod' rows@, so @r < rows@ and @c < cols@, and @r * cols + c@ is
-- therefore always in @[0, rows * cols)@ too. The bounds check 'VG.!' would
-- do can never fire.
transposeFlat :: VG.Vector v a => Int -> Int -> v a -> v a
transposeFlat rows cols v =
    VG.generate (rows * cols) $ \i ->
        let (c, r) = i `divMod` rows
         in VG.unsafeIndex v (r * cols + c)
{-# INLINABLE transposeFlat #-}

-- | The recursion behind 'mapAxis' and 'scanAxis': peel outer axes one at a
-- time, exactly as 'mapLowerDim' does, until the target axis @n@ is
-- outermost, then hand off to 'mapAxisHere'.
--
-- @c@, the axis type 'mapAxis' hands its function, is a plain (fundep-determined)
-- class parameter rather than an associated type family. An associated type
-- was tried first and does not work: at the abstract @n@ inside the recursive
-- instance below, GHC has to check the two instances' @AxisAt@ equations for
-- confluence the same way it would for any other open family, and @c@ against
-- @AxisAt (n - 1) as@ do not look equal to that check even though the
-- 'OVERLAPPING'\/'OVERLAPPABLE' pragmas make the /instances/ unambiguous.
-- Plain unification through nested instance resolution has no such check --
-- the base instance ties @c@ to the head axis by construction, and every
-- recursive instance forwards the very same @c@ -- so it is what determines
-- @c@ here instead.
class MapAxis (n :: Nat) (cs :: [Type]) (c :: Type) | n cs -> c where
  -- | Apply a length-preserving function to every fibre along axis @n@,
  -- independently for each combination of the other axes. See 'mapAxis'.
  mapAxisImpl ::
       (VG.Vector v x, VG.Vector v y)
    => (GridOf v '[c] x -> GridOf v '[c] y)
    -> GridOf v cs x
    -> GridOf v cs y

instance {-# OVERLAPPING #-} AllSizedKnown as => MapAxis 0 (c ': as) c where
  mapAxisImpl = mapAxisHere

instance {-# OVERLAPPABLE #-}
         (MapAxis (n - 1) as c, AllSizedKnown as) =>
         MapAxis n (c0 ': as) c where
  mapAxisImpl f = runIdentity . mapLowerDim (Identity . mapAxisImpl @(n - 1) @as @c f)

-- | The base case 'MapAxis' recurses down to: the target axis @c@ is
-- outermost, so every other axis, @as@, forms one contiguous block per @c@
-- value -- the same layout 'transposeGrid' swaps for a fixed pair of axes,
-- generalised here to swapping @c@ against @as@ taken as a single unit.
--
-- Transposing brings @c@ contiguous, turning each of the @'MaxCoordSize' as@
-- fibres into one chunk of @'splitVectorBySize'@; @f@ runs on each in turn,
-- and a second transpose undoes the first.
--
-- @as ~ '[]@ (@c@ is already the sole axis, the case every call this
-- recursion bottoms out at eventually gets to) skips the transpose rather
-- than pay for two no-op copies of the whole fibre: measured on the
-- 300x300 summed-area build, going through the general path regardless
-- cost 62.7 ms against 29 ms for the equivalent @'mapLowerDim' .
-- 'scanl1Grid'@, entirely in the two @'transposeFlat'@ copies and the
-- one-chunk @'splitVectorBySize'@\/@'VG.concat'@ around them, all three
-- provably no-ops whenever @'MaxCoordSize' as@ is @1@.
mapAxisHere ::
     forall v c as x y. (VG.Vector v x, VG.Vector v y, AllSizedKnown as)
  => (GridOf v '[c] x -> GridOf v '[c] y)
  -> GridOf v (c ': as) x
  -> GridOf v (c ': as) y
mapAxisHere f (Grid v) =
    let restSize = fromIntegral (GHC.natVal (Proxy @(MaxCoordSize as)))
     in if restSize == 1
          then Grid (unGrid (f (Grid v)))
          else let axisSize = VG.length v `div` restSize
                in Grid $
                   transposeFlat restSize axisSize $
                   VG.concat $
                   map (unGrid . f . Grid) $
                   splitVectorBySize axisSize $
                   transposeFlat axisSize restSize v
{-# INLINABLE mapAxisHere #-}

-- | Apply a length-preserving function to one named axis of a grid,
-- independently for every combination of the others -- 'mapLowerDim'
-- generalised from the outermost axis to any of them by position.
--
-- > mapAxis 1 f g   -- rather than mapAxis (Proxy @1) f g
--
-- Lets a caller reach an axis by name instead of physically rotating the
-- grid to bring it to the front (@transposeGrid . f . transposeGrid@), a
-- trick that stops working past two dimensions since there is no
-- 'transposeGrid' for an arbitrary pair of axes.
mapAxis ::
     forall v cs x y c. forall n -> (MapAxis n cs c, VG.Vector v x, VG.Vector v y)
  => (GridOf v '[c] x -> GridOf v '[c] y)
  -> GridOf v cs x
  -> GridOf v cs y
mapAxis n = mapAxisImpl @n
{-# INLINABLE mapAxis #-}

-- | Prefix-scan one named axis of a grid, independently for every
-- combination of the others.
--
-- > scanAxis 1 (+) g   -- rather than scanAxis (Proxy @1) (+) g
--
-- The summed-area-table build-up that motivated this, restated without the
-- transpose trick 'mapAxis' retires:
--
-- > sat = scanAxis 0 (+) . scanAxis 1 (+) . tabulateGrid power
scanAxis ::
     forall v cs a c. forall n -> (MapAxis n cs c, VG.Vector v a)
  => (a -> a -> a)
  -> GridOf v cs a
  -> GridOf v cs a
scanAxis n f = mapAxis n (scanl1Grid f)
{-# INLINABLE scanAxis #-}

-- | Taking a window out of a grid, one axis at a time.
--
-- The /window arithmetic/ here is element-agnostic: it slices whole sub-grids
-- and never looks inside one. The constraint is nevertheless @'VG.Vector' v x@,
-- because the recursion goes through 'splitGrid' and 'combineGrid', which take
-- the underlying vector apart and concatenate it back.
--
-- @v@ and @x@ are kind-annotated because nothing in the signature forces them
-- to be: left implicit, GHC generalises @v@ to @k -> Type@ and the instance
-- below cannot then match it against a @Type -> Type@ vector.
class ShrinkableGrid (cs :: [Type]) (as :: [Type]) (bs :: [Type]) where
  shrinkGrid ::
       forall (v :: Type -> Type) (x :: Type). VG.Vector v x
    => Coord cs
    -> GridOf v as x
    -> GridOf v bs x

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
--
-- The split is boxed, so the slice that picks the window is a boxed one
-- whatever @v@ the grid being shrunk uses. See 'splitGrid'.
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
        helper ::
             Grid '[ c y] (GridOf v as a)
          -> Grid '[ c z] (GridOf v as a)
        helper g =
            reifyCoord c $ \n ->
                withDict (windowFits @n @x @y @z) $ sliceGrid n z g


-- | Cut a grid into disjoint tiles along its outermost axis: an @Ordinal 9@
-- axis tiled by @Ordinal 3@ gives three tiles, not seven overlapping windows.
-- The tiles partition the source, so concatenating them reproduces it.
--
-- This was called @gridWindows@, which said the opposite of what it does. A
-- sliding window and a disjoint tiling are different operations; this is the
-- tiling. Sliding windows are 'gridWindows'.
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
-- @small@ before @v@, for the reason given on 'gridWindows'.
gridTiles :: forall small v big rest a.
               ( VG.Vector v a,
                 KnownNat (MaxCoordSize (small ': rest)),
                 CoordNat big `Mod` CoordNat small ~ 0
               )
            => GridOf v (big ': rest) a
            -> [GridOf v (small ': rest) a]
gridTiles (Grid v) =
    requiring @(CoordNat big `Mod` CoordNat small ~ 0) $
    let size = fromIntegral $ natVal (Proxy @(MaxCoordSize (small ': rest)))
    in map Grid $ splitVectorBySize size v
{-# INLINABLE gridTiles #-}

-- | Every overlapping window of size @CoordNat small@ along a grid's outermost
-- axis, stride 1: an @Ordinal 9@ axis windowed by @Ordinal 3@ gives seven
-- overlapping windows -- @0..2@, @1..3@, ..., @6..8@ -- not the three disjoint
-- tiles 'gridTiles' would.
--
-- This is 'shrinkGrid' at every valid offset along the outermost axis, with the
-- other axes left untouched at each one, stated at the vector level for the
-- same reason 'gridTiles' is: 'shrinkGrid' walks its whole @Coord@ list one
-- axis at a time, which would need an identity offset invented for every axis
-- in @rest@ (an axis of size 1 in the same family, so its window equals its
-- source) purely to state "leave this alone". Nothing here needs that: a window
-- of the outer axis is a contiguous run of whole @rest@-blocks, so it is one
-- 'VG.slice', and 'gridTiles'\'s own trick of reading the block size off the
-- vector rather than the type carries over unchanged. The property that ties
-- the two readings together -- this agrees with 'shrinkGrid' at every offset --
-- is checked in "Test.Windows" rather than assumed here.
--
-- @CoordNat small <= CoordNat big@ makes a window larger than its source a type
-- error, mirroring 'gridTiles'\'s own @Mod ~ 0@: both are preconditions the
-- vector-level implementation cannot check for itself, so the type states them
-- instead.
-- @small@ is quantified before @v@, and both of these deliberately break the
-- \"vector first\" order the rest of the module uses. Nothing determines @small@
-- -- it appears only in the element type of the result list -- so the caller
-- always supplies it by type application, whereas @v@ is read off the argument.
-- The parameter that must be written comes first, so @gridWindows \@(Ordinal 3)@
-- keeps working rather than becoming @gridWindows \@_ \@(Ordinal 3)@.
gridWindows :: forall small v big rest a.
               ( VG.Vector v a
               , AllSizedKnown rest
               , KnownNat (CoordNat small)
               , CoordNat small <= CoordNat big
               )
            => GridOf v (big ': rest) a
            -> [GridOf v (small ': rest) a]
gridWindows (Grid v) =
    requiring @(CoordNat small <= CoordNat big) $
    let restSize = fromIntegral $ natVal (Proxy @(MaxCoordSize rest))
        smallSize = fromIntegral $ natVal (Proxy @(CoordNat small))
        windowSize = smallSize * restSize
        bigSize = VG.length v `div` restSize
    in [ Grid (VG.slice (off * restSize) windowSize v)
       | off <- [0 .. bigSize - smallSize]
       ]
{-# INLINABLE gridWindows #-}
