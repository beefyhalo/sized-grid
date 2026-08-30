{-# LANGUAGE AllowAmbiguousTypes #-}

-- | The `GridOf` representation, its instances, and the operations that work
-- on the flat vector without regard to the shape of the axis list.
--
-- Everything here is re-exported from "Data.Grid.Sized.Internal.Grid"; the
-- shape algebra that /does/ care about the axis list lives in
-- "Data.Grid.Sized.Internal.Grid.Shape",
-- "Data.Grid.Sized.Internal.Grid.Axis" and
-- "Data.Grid.Sized.Internal.Grid.Windows".
module Data.Grid.Sized.Internal.Grid.Core
  ( -- * Representation
    GridOf (..),
    Grid,
    unsafeGridFromVector,

    -- * Construction and access
    gridVector,
    gridFromVector,
    gridFromList,
    collapseGrid,

    -- * Single-cell access
    cellLens,

    -- * Bulk operations

    --
    -- $bulk
    tabulateGrid,
    indexGrid,
    mapGrid,
    imapGrid,
    zipWithGrid,
    foldlGrid',
    scanl1Grid,

    -- * Shrinking to a bare vector

    --
    -- $shrinking
    mapMaybeGrid,
    filterGrid,
    catMaybesGrid,
    witherGrid,

    -- * Type-level machinery
    CollapseGrid,

    -- * Vector helpers
    splitVectorBySize,
  )
where

import Control.DeepSeq (NFData)
import Control.Lens hiding (index)
import Data.Aeson
import Data.Align (Semialign (..))
import Data.Distributive
import Data.Foldable1 (Foldable1 (..))
import Data.Functor.Bind (Apply (..), Bind (..), liftF2)
import Data.Functor.Classes
import Data.Functor.Rep
import Data.Grid.Sized.Coord
import Data.Grid.Sized.Internal.Grid.Nest
import Data.Hashable (Hashable (..))
import Data.Kind (Type)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import Data.These (These (..))
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Unboxed qualified as VU
import Data.Zip (Unzip (..), Zip (..))
import GHC.Generics qualified as GHC
import GHC.TypeLits qualified as GHC

-- | The @unGrid@ field is not exported: a record field in scope would permit
-- record-update syntax, which could break the length invariant.
newtype GridOf v (cs :: [Type]) a = Grid
  { unGrid :: v a
  }
  deriving stock (GHC.Generic, Eq, Ord, Show)
  deriving newtype (NFData, Eq1, Ord1, Show1, Functor)

type instance Index (GridOf v cs a) = Coord cs

type instance IxValue (GridOf v cs a) = a

-- | @ix@ /is/ 'cellLens'. A @'Lens'' s a@ is a @'Traversal'' s a@, so the
-- method typechecks unchanged -- and saying it with a lens is the honest
-- reading: the coordinate is in range by construction, so this traversal has
-- exactly one focus and can never miss.
--
-- Eta-expanded to @ix c f@ rather than written point-free: `Lens'` quantifies
-- its functor under `Functor` and `Traversal'` under `Applicative`, so the
-- two arguments have to be in scope before the weaker constraint can be
-- discharged from the stronger one.
instance (VG.Vector v a, IsCoordList cs) => Ixed (GridOf v cs a) where
  ix c f = cellLens c f
  {-# INLINE ix #-}

-- | Kept nullary so it stays partially applicable, e.g. @'Functor' ('Grid' cs)@.
type Grid = GridOf V.Vector

-- | Derived, and deliberately so. @GeneralizedNewtypeDeriving@ coerces @v@'s
-- whole dictionary, so every method @v@ overrides comes across with it, and
-- `Data.Vector`'s `Foldable` overrides @foldr@, @foldl@, @foldr'@, @foldl'@,
-- @toList@, @length@, @null@, @elem@, @maximum@, @minimum@, @sum@ and
-- @product@ -- the reducing four as @Bundle.foldl'@\/@Bundle.foldl1'@ over the
-- stream, already strict. A grid inherits all of it. (Checked against
-- vector-0.13.2.0, inside the @>=0.13 && <0.14@ bound, rather than assumed.)
--
-- sized-grid-adr.10 proposed writing this instance by hand to fix two things.
-- Neither was broken, and the second cannot be fixed at any acceptable price:
--
--   1. __The strict-fold overrides.__ @sum@ over a 90,000-cell boxed
--      @Grid '[Clamped 300, Clamped 300] Int@ allocates 1,128 bytes -- the
--      same as `foldlGrid'` on the same grid, where a @foldl@ thunk chain
--      would be about 2.9 MB. There is no thunk chain to remove. adr.10's
--      evidence for one was the @mapGrid 300x300@ benchmark reading
--      slower than @foldlGrid' 300x300@, but that benchmark sums nothing:
--      its body is @nf (mapGrid (+ 1))@, which builds and deep-forces a whole
--      second 90,000-cell boxed grid (sized-grid-iiah).
--   2. __@length@ and @null@ as compile-time constants.__ Both are already
--      /O(1)/. At @-O2@ and a concrete axis list, @length@ is one field
--      unpack -- @case v of Vector _ n _ -> I# n@ -- and @null@ is that plus
--      a comparison against @0@. The literal would save the unpack.
--
--      It is not available anyway. @length _ = coordSpaceSize \@cs@ needs
--      `IsCoordList cs` on the /instance head/, since a method cannot acquire
--      a constraint of its own; `Foldable` is a superclass of `Traversable`,
--      `FoldableWithIndex` and `TraversableWithIndex`, so it spreads to all
--      four instances and from there to every call site of every method of
--      any of them. Tried, and it reaches out of the library: @randomGrid =
--      sequence $ pure getRandom@ in @ising-example@ needs only
--      `AllSizedKnown cs` today and would have to carry `IsCoordList cs` too,
--      so that a @length@ it never calls could be a literal. sized-grid-o9s
--      had just finished taking `AllSizedKnown` off `Apply` and `Bind`;
--      this would push a constraint back the other way to buy nothing.
deriving newtype instance (Foldable v) => Foldable (GridOf v cs)

-- | Written by hand: @GeneralizedNewtypeDeriving@ can't coerce under the
-- applicative parameter.
instance (Traversable v) => Traversable (GridOf v cs) where
  traverse f (Grid v) = Grid <$> traverse f v

instance (IsCoordList cs) => Each (Grid cs a) (Grid cs b) a b where
  each = traverse

-- | A grid hashes as its elements in row-major order, salted first with the
-- cell count. Lawful for the same reason the derived 'Eq' is: both are
-- structural over the one field, so equal grids fold to equal hashes and the
-- @a == b ==> hashWithSalt s a == hashWithSalt s b@ law holds pointwise. It
-- touches only the flat vector, so it carries no `IsCoordList` and sits with
-- the size-agnostic instances rather than the coordinate-aware ones. This is
-- what lets a `Grid` be a `HashMap` \/ `HashSet` key -- a transposition table
-- keyed on board state, or a hash-consed quadtree node (sized-grid-h6ki).
--
-- The leading @hashWithSalt salt (V.length v)@ mixes in the length the way
-- @hashable@\'s own list and array instances do; here the length is fixed by
-- @cs@, so it only ever separates grids of different shape that share an
-- element type, but it is one cheap word and keeps the instance's shape the
-- same as the rest of the ecosystem's.
--
-- Boxed only, like 'Applicative' and 'Num' above: @Hashable@\'s `Eq`
-- superclass is discharged here by the stock-derived @Eq (Grid cs a)@, which
-- reduces to `Eq a`; a @GridOf v cs@ form would have to carry @Eq (v a)@ on
-- the head for no caller that has asked.
instance (Hashable a) => Hashable (Grid cs a) where
  hashWithSalt salt (Grid v) =
    V.foldl' hashWithSalt (hashWithSalt salt (V.length v)) v
  {-# INLINE hashWithSalt #-}

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
  forall v cs a.
  (VG.Vector v a, AllSizedKnown cs) =>
  v a ->
  Maybe (GridOf v cs a)
gridFromVector v =
  if VG.length v == fromIntegral (GHC.natVal (Proxy :: Proxy (MaxCoordSize cs)))
    then Just (Grid v)
    else Nothing
{-# INLINEABLE gridFromVector #-}

-- $bulk
-- Operations that carry an element constraint and so cannot be class methods.

-- | `tabulate` for grids that cannot be `Representable`.
--
-- sized-grid-adr.4 spiked three ways to close the gap between this and
-- `pure` (same vector, no coordinate). sized-grid-adr.16 then changed which
-- one wins, by changing what a coordinate costs to make; all four are
-- recorded so none is re-tried blind.
--
--   1. __An odometer.__ Walk `Coord` directly with a hand-rolled successor
--      (increment the last axis, carry into earlier ones on overflow) and
--      feed it to `VG.unfoldrExactN`, so no coordinate list is ever built.
--      Measured /worse/: 78-93% slower than the `VG.fromListN` version it
--      replaced, allocating more, not less. The `NP` state `unfoldrExactN`
--      threaded through its generator did not specialise the way a plain
--      `Int` accumulator would. Moot now -- there is no `NP` to thread --
--      and subsumed by (4).
--   2. __Make the list fuse, the way `imap` did.__ @V.zipWith func
--      (V.fromList allCoord) v@ fuses because it has a second vector to zip
--      against; `tabulateGrid` has none, since producing @v@ /is/ the job,
--      so it was handed a throwaway @V.replicate size ()@ to trigger the
--      same fusion. That was this function's body until adr.16, and it is
--      what (4) replaced.
--   3. __`VG.generate` with `coordFromPosition`.__ Measured far worse at the
--      time: 297-650% slower, because re-deriving every axis of every
--      coordinate from scratch -- one `quotRem` per axis, an `NP` rebuilt --
--      cost more than either building the list or walking it.
--   4. __`VG.generate` with the position /as/ the coordinate.__ What (3)
--      becomes once a coordinate is its row-major position: there is nothing
--      to re-derive, so `unsafeCoordFromPosition` is a coercion and the
--      generator is @func . coerce@. adr.4's measurement of (3) was a
--      measurement of the representation, not of `VG.generate`, and it
--      inverted when the representation did: 2.44 ms to 1.65 ms boxed and
--      529 to 354 us unboxed against (2), with the throwaway unit vector and
--      the `VG.convert` after it both gone.
--
-- The index `VG.generate` supplies is below the vector's length, and that
-- length is @coordSpaceSize \@cs@, so the `unsafeCoordFromPosition`
-- precondition is discharged by construction -- the same argument
-- `indexGrid`\'s `VG.unsafeIndex` rests on.
tabulateGrid ::
  forall v cs a.
  (VG.Vector v a, IsCoordList cs) =>
  (Coord cs -> a) ->
  GridOf v cs a
tabulateGrid func =
  Grid $ VG.generate (coordSpaceSize @cs) (func . unsafeCoordFromPosition)
{-# INLINEABLE tabulateGrid #-}

-- | Read the element at a coordinate. `Data.Functor.Rep.index` for grids
-- whose element type cannot support `Representable`. Uses 'VG.unsafeIndex':
-- @coordPosition c@ is in range by the `Ordinal` invariant on every axis and
-- the vector has exactly that many elements by `GridOf`'s size invariant,
-- so the bounds check can never fire on a grid built through the safe API
-- -- only `unsafeGridFromVector` can falsify that.
--
-- No @IsCoordList cs@ any more: after sized-grid-adr.16 a coordinate /is/ its
-- position, so there is no fold left here to need the axis sizes.
indexGrid ::
  forall v cs a.
  (VG.Vector v a) =>
  GridOf v cs a ->
  Coord cs ->
  a
indexGrid (Grid v) c = VG.unsafeIndex v (coordPosition c)
{-# INLINE indexGrid #-}

-- | The one cell at a coordinate, read and written. The single definition
-- behind @'ix'@, "Data.Grid.Sized.Optics".@cell@ and
-- @'Data.Grid.Sized.Class.gridIndex'@, which were three copies of it.
--
-- Both directions are unchecked, for the reason `indexGrid` gives above:
-- @coordPosition c@ is in range by the `Ordinal` invariant on every axis and
-- the vector has exactly that many elements by `GridOf`'s size invariant. A
-- bounds check on the write could only ever have passed, and paying for one
-- would also contradict the type -- a `Lens'` promises a focus is there.
-- Only `unsafeGridFromVector` can falsify that.
--
-- No @IsCoordList cs@, for the same reason `indexGrid` dropped it: a
-- coordinate /is/ its position since sized-grid-adr.16, so there is no fold
-- here that needs the axis sizes. The call sites that still carry the
-- constraint keep it as a published signature, not as a need.
cellLens ::
  forall v cs a.
  (VG.Vector v a) =>
  Coord cs ->
  Lens' (GridOf v cs a) a
cellLens c = lens getter setter
  where
    position = coordPosition c
    getter (Grid v) = VG.unsafeIndex v position
    setter (Grid v) value = Grid (VG.unsafeUpd v [(position, value)])
{-# INLINE cellLens #-}

mapGrid ::
  (VG.Vector v a, VG.Vector v b) =>
  (a -> b) ->
  GridOf v cs a ->
  GridOf v cs b
mapGrid f (Grid v) = Grid (VG.map f v)
{-# INLINE mapGrid #-}

-- | The index `VG.imap` already has /is/ the coordinate after
-- sized-grid-adr.16, so there is nothing to zip against and nothing to look
-- up. This used to build a boxed @V.Vector (Coord cs)@ of every coordinate up
-- front and index into it, which was the cheaper of the two options while a
-- coordinate was a spine of boxes; it is pure waste now.
imapGrid ::
  forall v cs a b.
  (VG.Vector v a, VG.Vector v b) =>
  (Coord cs -> a -> b) ->
  GridOf v cs a ->
  GridOf v cs b
imapGrid f (Grid v) = Grid (VG.imap (f . unsafeCoordFromPosition) v)
{-# INLINEABLE imapGrid #-}
{-# SPECIALIZE imapGrid :: forall cs. (Coord cs -> Int -> Int) -> GridOf VU.Vector cs Int -> GridOf VU.Vector cs Int #-}
{-# SPECIALIZE imapGrid :: forall cs. (Coord cs -> Double -> Double) -> GridOf VU.Vector cs Double -> GridOf VU.Vector cs Double #-}
{-# SPECIALIZE imapGrid :: forall cs. (Coord cs -> Int -> Int) -> GridOf V.Vector cs Int -> GridOf V.Vector cs Int #-}
{-# SPECIALIZE imapGrid :: forall cs. (Coord cs -> Double -> Double) -> GridOf V.Vector cs Double -> GridOf V.Vector cs Double #-}

-- | Pointwise combination of two grids of the same shape.
zipWithGrid ::
  (VG.Vector v a, VG.Vector v b, VG.Vector v c) =>
  (a -> b -> c) ->
  GridOf v cs a ->
  GridOf v cs b ->
  GridOf v cs c
zipWithGrid f (Grid a) (Grid b) = Grid (VG.zipWith f a b)
{-# INLINE zipWithGrid #-}

-- | Strict left fold in row-major order. `Data.Foldable.foldl'` for grids whose
-- element type cannot support `Foldable`.
foldlGrid' :: (VG.Vector v a) => (b -> a -> b) -> b -> GridOf v cs a -> b
foldlGrid' f z (Grid v) = VG.foldl' f z v
{-# INLINE foldlGrid' #-}

-- | Left-to-right scan over the whole grid in row-major order, keeping the
-- shape. Accumulates strictly, as a running total over a boxed vector otherwise
-- builds a chain of thunks the length of the grid.
--
-- Row-major means that on a multi-dimensional grid the scan runs off the end of
-- one row and straight into the next. To scan each row independently, which is
-- usually what is wanted, compose it with @mapLowerDim@:
--
-- > runIdentity (mapLowerDim (Identity . scanl1Grid (+)) g)
--
-- This exists so that prefix sums -- the summed-area-table build-up being the
-- common case -- do not need the escape hatch. Length preservation is
-- guaranteed by 'VG.scanl1'', so no size constraint is needed.
scanl1Grid :: (VG.Vector v a) => (a -> a -> a) -> GridOf v cs a -> GridOf v cs a
scanl1Grid f (Grid v) = Grid (VG.scanl1' f v)
{-# INLINE scanl1Grid #-}

-- $shrinking
--
-- A `GridOf` fixes its cell count at @'MaxCoordSize' cs@ and every class
-- instance it has preserves that count, so there is no lawful
-- @Filterable@\/@Witherable@ instance: @catMaybes :: Grid cs (Maybe a) -> Grid
-- cs a@ has no @cs@-shaped result to return when a cell is `Nothing`, and the
-- @Filterable@ law @catMaybes . fmap Just = id@ only pins the all-@Just@ case,
-- so it cannot even rule out a total-but-wrong instance (sized-grid-g74j).
--
-- What a fixed grid /can/ offer, in the spirit of `foldlGrid'` and the rest of
-- @$bulk@, is the same selection done honestly: the result is a plain
-- `Data.Vector.Generic.Vector` in row-major order, its length decided by the
-- predicate rather than the type. Reach for these to collect a subset of cells
-- -- the alive cells of a board, the walls of a maze -- without pretending the
-- grid shrank. They are @Data.Vector.Generic@\'s own @mapMaybe@\/@filter@ with
-- the newtype peeled off; no @witherable@ dependency is involved.

-- | Row-major cells for which @f@ says `Just`, as a bare vector.
mapMaybeGrid ::
  (VG.Vector v a, VG.Vector v b) =>
  (a -> Maybe b) ->
  GridOf v cs a ->
  v b
mapMaybeGrid f (Grid v) = VG.mapMaybe f v
{-# INLINE mapMaybeGrid #-}

-- | Row-major cells satisfying the predicate, as a bare vector.
filterGrid ::
  (VG.Vector v a) =>
  (a -> Bool) ->
  GridOf v cs a ->
  v a
filterGrid p (Grid v) = VG.filter p v
{-# INLINE filterGrid #-}

-- | The `Just` cells of a grid of `Maybe`s, in row-major order, as a bare
-- vector. @'mapMaybeGrid' id@.
catMaybesGrid ::
  (VG.Vector v (Maybe a), VG.Vector v a) =>
  GridOf v cs (Maybe a) ->
  v a
catMaybesGrid (Grid v) = VG.mapMaybe id v
{-# INLINE catMaybesGrid #-}

-- | Effectful `mapMaybeGrid`: run @f@ on every cell in row-major order, then
-- keep the `Just`s as a bare vector. Goes through a list so no
-- @'VG.Vector' v ('Maybe' b)@ is needed for the intermediate, which keeps it
-- usable at an unboxed @v@.
witherGrid ::
  (Applicative f, VG.Vector v a, VG.Vector v b) =>
  (a -> f (Maybe b)) ->
  GridOf v cs a ->
  f (v b)
witherGrid f (Grid v) =
  VG.fromList . catMaybes <$> traverse f (VG.toList v)
{-# INLINEABLE witherGrid #-}

-- | `AllSizedKnown` is `Applicative`'s cost, not `Apply`'s: it is only there
-- for `pure`, which has to materialise a vector of the right length out of
-- nothing. '(<.>)' is a zipWith and needs none of that -- so a grid
-- polymorphic in @cs@ with no `KnownNat` evidence on every axis can still be
-- `Apply`\'d, where it cannot be `Applicative`\'d (sized-grid-o9s).
instance (IsCoordList cs) => Apply (Grid cs) where
  (<.>) = zipWithGrid ($)

-- | As 'Apply' above: `Monad`\'s `AllSizedKnown` comes from `Representable`'s
-- `index`, not from what a bind actually needs. `indexGrid` itself takes only
-- `IsCoordList` (see its haddock), so `Bind` drops the constraint `Monad`
-- cannot.
instance (IsCoordList cs) => Bind (Grid cs) where
  g >>- f = imap (\p a -> indexGrid (f a) p) g

instance (IsCoordList cs) => Semialign (Grid cs) where
  alignWith f = zipWithGrid (\a b -> f (These a b))

instance (IsCoordList cs) => Zip (Grid cs) where
  zipWith = zipWithGrid

-- | Splitting a grid of pairs gives two grids of the same shape, so this needs
-- no size evidence: both halves inherit the source's length, and the length
-- invariant holds for each.
--
-- semialign-1.4 moved `Unzip` to the bottom of the hierarchy, directly above
-- `Functor`, making it a superclass of `Semialign`; under 1.3 it sat above
-- `Zip` instead. This instance satisfies either hierarchy, so the
-- @>=1.3 && <1.5@ bound stays honest.
instance (IsCoordList cs) => Unzip (Grid cs) where
  unzip (Grid v) = let (as, bs) = V.unzip v in (Grid as, Grid bs)

-- | Boxed only, and necessarily so: `pure` must produce a grid of /any/ element
-- type, which no unboxed vector can hold. 'tabulateGrid' is the unboxed
-- counterpart for the cases that have a concrete element type in hand.
instance (AllSizedKnown cs) => Applicative (Grid cs) where
  pure =
    Grid . V.replicate (fromIntegral $ GHC.natVal (Proxy :: Proxy (MaxCoordSize cs)))
  Grid fs <*> Grid as = Grid $ V.zipWith ($) fs as

-- | A grid has no concatenation operation that preserves its shape, so the
-- semigroup operation is pointwise. This is the same reading as for an array:
-- values at corresponding coordinates are combined.
instance (IsCoordList cs, Semigroup a) => Semigroup (Grid cs a) where
  (<>) = zipWithGrid (<>)

-- | The monoid operation is pointwise, and 'mempty' fills every cell with the
-- element monoid's identity.
instance (AllSizedKnown cs, IsCoordList cs, Monoid a) => Monoid (Grid cs a) where
  mempty = pure mempty

-- | Arithmetic on a grid is pointwise. In particular, multiplication is the
-- Hadamard product, while 'abs' and 'signum' act independently on each cell.
instance (AllSizedKnown cs, IsCoordList cs, Num a) => Num (Grid cs a) where
  (+) = zipWithGrid (+)
  (*) = zipWithGrid (*)
  abs = mapGrid abs
  signum = mapGrid signum
  negate = mapGrid negate
  fromInteger = pure . fromInteger

-- | Defined via '(>>-)' so the two cannot drift.
instance
  (AllSizedKnown cs, IsCoordList cs) =>
  Monad (Grid cs)
  where
  g >>= f = g >>- f

instance
  (AllSizedKnown cs, IsCoordList cs) =>
  Distributive (Grid cs)
  where
  distribute = distributeRep

instance
  (IsCoordList cs, AllSizedKnown cs) =>
  Representable (Grid cs)
  where
  type Rep (Grid cs) = Coord cs

  -- The bodies are 'tabulateGrid' and 'indexGrid' at @v ~ V.Vector@, where the
  -- 'VG.Vector' constraint is discharged for every element type.
  tabulate = tabulateGrid
  index = indexGrid

-- | All three of these zipped against @V.fromList allCoord@ until
-- sized-grid-adr.16, relying on 'V.zipWith' fusing with the coordinate list
-- so the coordinates were produced and consumed one at a time rather than
-- being live all at once. That is no longer the cheapest shape, or even a
-- sensible one: a coordinate is its row-major position, so the index the
-- vector combinator already carries is the coordinate, and the list has
-- nothing left to produce. Going through it cost 3.7x on 'imap' and 1.4x on
-- 'ifoldl'' against the index forms below.
--
-- As in 'imapGrid', the index is below the vector's length and the length is
-- @coordSpaceSize \@cs@, which discharges 'unsafeCoordFromPosition'.
instance FunctorWithIndex (Coord cs) (Grid cs) where
  imap func (Grid v) = Grid $ V.imap (func . unsafeCoordFromPosition) v

-- | 'ifoldr' and 'ifoldl'' are given outright because the class otherwise
-- builds them out of 'ifoldMap' and an 'Endo' chain, which is a closure per
-- cell on top of the coordinate.
instance FoldableWithIndex (Coord cs) (Grid cs) where
  ifoldMap func (Grid v) =
    V.ifoldr (\i x acc -> func (unsafeCoordFromPosition i) x <> acc) mempty v
  ifoldr func z (Grid v) = V.ifoldr (func . unsafeCoordFromPosition) z v
  ifoldl' func z (Grid v) =
    V.ifoldl' (\acc i x -> func (unsafeCoordFromPosition i) acc x) z v

instance TraversableWithIndex (Coord cs) (Grid cs) where
  itraverse func (Grid v) =
    Grid <$> sequenceA (V.imap (func . unsafeCoordFromPosition) v)

-- | 'IsCoordList cs' proves the backing vector non-empty -- every axis has
-- @1 <= 'CoordNat'@, so @'MaxCoordSize' cs@ is a product of positive factors
-- and is itself @>= 1@ -- which is what makes the partial vector primitives
-- below total here and is why the instance carries the constraint the
-- unconditional 'Foldable' cannot (the adr.10 note above is about not putting
-- it on /that/ head, where it would spread to four classes' every call site;
-- 'Foldable1' is a fresh leaf class, so it stays contained, exactly as it
-- does on 'Apply' and 'Bind').
--
-- Every fold primitive is given outright: left to the class, each is built
-- from 'foldMap1' by way of a rebuilt 'NonEmpty', a cons cell per element on
-- top of the coordinate-free traversal. 'head', 'last', 'maximum' and
-- 'minimum' come straight from the vector's own O(1) access and fused stream
-- reductions rather than through a fold.
instance (IsCoordList cs) => Foldable1 (Grid cs) where
  foldMap1 f (Grid v) =
    V.foldr (\x acc -> f x <> acc) (f (V.unsafeLast v)) (V.unsafeInit v)
  foldMap1' f (Grid v) =
    let z = f (V.unsafeHead v)
     in z `seq` V.foldl' (\acc x -> acc <> f x) z (V.unsafeTail v)
  foldrMap1 g f (Grid v) = V.foldr f (g (V.unsafeLast v)) (V.unsafeInit v)
  foldrMap1' g f (Grid v) = V.foldr' f (g (V.unsafeLast v)) (V.unsafeInit v)
  foldlMap1 g f (Grid v) = V.foldl f (g (V.unsafeHead v)) (V.unsafeTail v)
  foldlMap1' g f (Grid v) =
    let z = g (V.unsafeHead v) in z `seq` V.foldl' f z (V.unsafeTail v)
  toNonEmpty (Grid v) = V.unsafeHead v NE.:| V.toList (V.unsafeTail v)
  head (Grid v) = V.unsafeHead v
  last (Grid v) = V.unsafeLast v
  maximum (Grid v) = V.maximum v
  minimum (Grid v) = V.minimum v

-- | The 'Traversable1' companion to the hand-written 'Traversable': fold the
-- non-empty vector into an 'Apply' chain with no 'pure', seeded on the last
-- cell, and repack the collected list with 'V.fromListN' so the length
-- invariant is carried across. Not a hot path -- clarity over the tighter
-- mutable-write form 'itraverse' would need.
--
-- The class is in scope from @Control.Lens@'s re-export of it, the same route
-- the 'FoldableWithIndex' \/ 'TraversableWithIndex' instances above take for
-- theirs; 'Foldable1' has no such re-export and is imported directly.
instance (IsCoordList cs) => Traversable1 (Grid cs) where
  traverse1 f (Grid v) =
    Grid . V.fromListN (V.length v)
      <$> V.foldr
        (liftF2 (:) . f)
        ((: []) <$> f (V.unsafeLast v))
        (V.unsafeInit v)

-- | Convert a vector into a list of `Data.Vector.Generic.Vector`s, where all the
-- elements of the list have the given size.
--
-- If @n@ does not divide the length, the final chunk is short. Every caller in
-- this module is protected from that by the size invariant on 'GridOf', so the
-- short chunk is unreachable for them -- but it is a silent malformation rather
-- than a failure, so do not rely on it.
--
-- A size of zero would otherwise loop forever taking empty prefixes.
splitVectorBySize :: (VG.Vector v a) => Int -> v a -> [v a]
splitVectorBySize n v
  | n <= 0 = error $ "splitVectorBySize: chunk size must be positive, got " ++ show n
  | otherwise = [VG.slice i (min n (len - i)) v | i <- [0, n .. len - 1]]
  where
    len = VG.length v
{-# INLINEABLE splitVectorBySize #-}

-- | Convert a grid to a series of nested lists. This removes type level information, but it is sometimes easier to work with lists
collapseGrid ::
  forall v cs a.
  (VG.Vector v a, AllSizedKnown cs) =>
  GridOf v cs a ->
  CollapseGrid cs a
collapseGrid (Grid v) = nestByShape @cs (VG.convert v)
{-# INLINEABLE [1] collapseGrid #-}

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
  collapseGrid g =
    nestByShape @cs (unGrid g)
  #-}

-- | Convert a series of nested lists to a grid. If the size of the grid does not match the size of lists this will be `Nothing`
gridFromList ::
  forall v cs a.
  (VG.Vector v a, AllSizedKnown cs) =>
  CollapseGrid cs a ->
  Maybe (GridOf v cs a)
gridFromList cg = Grid . VG.convert <$> flattenByShape @cs cg
{-# INLINEABLE [1] gridFromList #-}

-- | As 'collapseGrid/boxed', for the other direction. @v@ only appears in the
-- /result/ here, so it is pinned with an explicit type application rather than
-- an argument annotation.
{-# RULES
"gridFromList/boxed" [~1] forall (cg :: CollapseGrid cs a).
  gridFromList @V.Vector @cs @a cg =
    Grid <$> flattenByShape @cs cg
  #-}

instance
  (VG.Vector v a, AllSizedKnown cs, ToJSON a) =>
  ToJSON (GridOf v cs a)
  where
  toJSON (Grid v) = nestedToJSON @cs (VG.convert v)

-- | Decoding validates the length at every dimension, so a successfully decoded
-- grid always satisfies @VG.length (gridVector g) == MaxCoordSize cs@. Without
-- the check a short or ragged array decoded to a `GridOf` whose vector
-- disagreed with its type, which then made `index` throw and ('<*>') silently
-- truncate.
--
-- The constraints match `ToJSON`\'s: the `KnownNat` evidence is what makes the
-- check possible.
instance
  (VG.Vector v a, AllSizedKnown cs, FromJSON a) =>
  FromJSON (GridOf v cs a)
  where
  parseJSON val = Grid . VG.convert <$> nestedParseJSON @cs val
