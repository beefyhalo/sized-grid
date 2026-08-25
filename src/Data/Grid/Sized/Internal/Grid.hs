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
  , permuteGrid
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
  , axisFibres
  , axis
  , scanAxis
    -- * Windows and tiles
  , ShrinkableGrid(..)
  , gridTiles
  , tiles
  , gridWindows
  , windows
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
import           Data.Align                   (Semialign (..))
import           Data.Constraint
import           Data.Distributive
import           Data.Functor.Bind             (Apply (..), Bind (..))
import           Data.Functor.Classes
import           Data.Functor.Rep
import           Data.Kind                     (Type)
import           Data.Proxy                    (Proxy (..))
import           Data.These                    (These (..))
import           Data.Zip                      (Unzip (..), Zip (..))
import qualified Data.Vector                   as V
import qualified Data.Vector.Generic           as VG
import qualified Data.Vector.Generic.Mutable   as VGM
import qualified GHC.Generics                  as GHC
import           GHC.TypeLits
import qualified GHC.TypeLits                  as GHC

-- | The @unGrid@ field is not exported: a record field in scope would permit
-- record-update syntax, which could break the length invariant.
newtype GridOf v (cs :: [Type]) a = Grid
  { unGrid :: v a
  } deriving stock (GHC.Generic, Eq, Show)
    deriving newtype (NFData, Eq1, Show1, Functor)

type instance Index (GridOf v cs a) = Coord cs
type instance IxValue (GridOf v cs a) = a

instance (VG.Vector v a, IsCoordList cs) => Ixed (GridOf v cs a) where
  ix c f (Grid v) =
    let position = coordPosition c
        replace value = Grid (v VG.// [(position, value)])
    in replace <$> f (VG.unsafeIndex v position)

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
deriving newtype instance Foldable v => Foldable (GridOf v cs)

-- | Written by hand: @GeneralizedNewtypeDeriving@ can't coerce under the
-- applicative parameter.
instance Traversable v => Traversable (GridOf v cs) where
  traverse f (Grid v) = Grid <$> traverse f v

instance IsCoordList cs => Each (Grid cs a) (Grid cs b) a b where
  each = traverse

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
       forall v cs a. (VG.Vector v a, IsCoordList cs)
    => (Coord cs -> a)
    -> GridOf v cs a
tabulateGrid func =
    Grid $ VG.generate (coordSpaceSize @cs) (func . unsafeCoordFromPosition)
{-# INLINABLE tabulateGrid #-}

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
       forall v cs a. VG.Vector v a
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

-- | The index `VG.imap` already has /is/ the coordinate after
-- sized-grid-adr.16, so there is nothing to zip against and nothing to look
-- up. This used to build a boxed @V.Vector (Coord cs)@ of every coordinate up
-- front and index into it, which was the cheaper of the two options while a
-- coordinate was a spine of boxes; it is pure waste now.
imapGrid ::
       forall v cs a b. (VG.Vector v a, VG.Vector v b)
    => (Coord cs -> a -> b)
    -> GridOf v cs a
    -> GridOf v cs b
imapGrid f (Grid v) = Grid (VG.imap (f . unsafeCoordFromPosition) v)
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

-- | `AllSizedKnown` is `Applicative`'s cost, not `Apply`'s: it is only there
-- for `pure`, which has to materialise a vector of the right length out of
-- nothing. '(<.>)' is a zipWith and needs none of that -- so a grid
-- polymorphic in @cs@ with no `KnownNat` evidence on every axis can still be
-- `Apply`\'d, where it cannot be `Applicative`\'d (sized-grid-o9s).
instance IsCoordList cs => Apply (Grid cs) where
  (<.>) = zipWithGrid ($)

-- | As 'Apply' above: `Monad`\'s `AllSizedKnown` comes from `Representable`'s
-- `index`, not from what a bind actually needs. `indexGrid` itself takes only
-- `IsCoordList` (see its haddock), so `Bind` drops the constraint `Monad`
-- cannot.
instance IsCoordList cs => Bind (Grid cs) where
  g >>- f = imap (\p a -> indexGrid (f a) p) g

instance IsCoordList cs => Semialign (Grid cs) where
  alignWith f = zipWithGrid (\a b -> f (These a b))

instance IsCoordList cs => Zip (Grid cs) where
  zipWith = zipWithGrid

-- | Splitting a grid of pairs gives two grids of the same shape, so this needs
-- no size evidence: both halves inherit the source's length, and the length
-- invariant holds for each.
--
-- semialign-1.4 moved `Unzip` to the bottom of the hierarchy, directly above
-- `Functor`, making it a superclass of `Semialign`; under 1.3 it sat above
-- `Zip` instead. This instance satisfies either hierarchy, so the
-- @>=1.3 && <1.5@ bound stays honest.
instance IsCoordList cs => Unzip (Grid cs) where
  unzip (Grid v) = let (as, bs) = V.unzip v in (Grid as, Grid bs)

-- | Boxed only, and necessarily so: `pure` must produce a grid of /any/ element
-- type, which no unboxed vector can hold. 'tabulateGrid' is the unboxed
-- counterpart for the cases that have a concrete element type in hand.
instance AllSizedKnown cs => Applicative (Grid cs) where
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
instance (AllSizedKnown cs, IsCoordList cs) =>
         Monad (Grid cs) where
  g >>= f = g >>- f

instance (AllSizedKnown cs, IsCoordList cs) =>
         Distributive (Grid cs) where
  distribute = distributeRep

instance (IsCoordList cs, AllSizedKnown cs) =>
         Representable (Grid cs) where
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
-- The last axis is a case of its own, and that is sized-grid-adr.12: aeson's
-- own @'ToJSON' ('V.Vector' a)@ turns the innermost row into an 'Array'
-- directly. Without it the general branch reaches this row too, and splits it
-- into one single-element slice per cell only for @SizeNil@ to read the cell
-- back out of it and @'toJSON' :: ['Value'] -> 'Value'@ to rebuild a vector
-- from the list of results -- three intermediates per cell to produce what the
-- vector instance produces in one pass. Measured on @toJSON 100x100@:
-- 652 us and 3.7 MB became 288 us and 1.3 MB.
nestedToJSON v =
  case sizeProof @cs of
    SizeNil -> toJSON (v V.! 0)
    SizeCons @_ @_ @rest ->
      case sizeProof @rest of
        SizeNil -> toJSON v
        SizeCons{} ->
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
-- The last axis is a case of its own, for the reason given on 'nestedToJSON'
-- and with the same measurement behind it (sized-grid-adr.12). At the
-- innermost row aeson's own @'FromJSON' [a]@ parses the elements in one pass;
-- the general branch would parse the row to a @['Value']@ first and then hand
-- each 'Value' to @SizeNil@, which wraps every cell in a one-element vector
-- for 'V.concat' to copy straight back out. The explicit value traversal also
-- avoids Aeson's special @[Char]@ instance, which expects a JSON string rather
-- than the array of singleton strings emitted for a character row. Measured on
-- the isolated parse of
-- a 100x100 grid: 1.91 ms and 8.1 MB became 957 us and 5.1 MB, which is
-- within 1.5x of what aeson charges to parse the same 'Value' as a plain
-- @[[Int]]@ -- the floor this recursion can reach without replacing aeson's
-- element parser.
--
-- The order of the two failures differs at that row and only there: the
-- element parses now happen before the length check, so a row that is both
-- ragged /and/ badly typed reports the element rather than the length. Both
-- are still rejected, which is what
-- "Test.Invariant"'s @Malformed JSON must be rejected@ pins.
nestedParseJSON val =
  case sizeProof @cs of
    SizeNil -> V.singleton <$> parseJSON val
    SizeCons @_ @n @rest ->
      case sizeProof @rest of
        SizeNil -> do
          vals :: [Value] <- parseJSON val
          checkLength @n (length vals)
          V.fromList <$> traverse parseJSON vals
        SizeCons{} -> do
          vals :: [Value] <- parseJSON val
          checkLength @n (length vals)
          V.concat <$> traverse (nestedParseJSON @rest) vals

-- | The per-dimension length check both branches of 'nestedParseJSON' share:
-- @n@ elements at this axis, or a parse failure naming both counts.
checkLength :: forall n. GHC.KnownNat n => Int -> Parser ()
checkLength got
  | got == expected = pure ()
  | otherwise =
      fail $ "Grid: expected " ++ show expected ++ " elements, got " ++ show got
  where
    expected = fromIntegral (GHC.natVal (Proxy @n)) :: Int

-- | @tabulate (index g . f)@ for a coordinate endomorphism-or-relabelling @f@
-- is a permutation of the underlying vector: which source position feeds
-- which target position depends only on @cs@, @ds@ and @f@, never on @g@'s
-- elements. So it can be computed once as a table of positions and applied
-- with 'VG.unsafeBackpermute', a tight loop with no @Coord@ built, permuted
-- or destroyed per cell -- unlike @tabulateGrid (indexGrid g . f)@, which is
-- exactly that per cell. 'transposeGrid' is this at a fixed @f@.
--
-- INLINE, not just INLINABLE. An offered-but-declined unfolding leaves @f@,
-- @cs@ and @ds@ opaque to whichever module actually calls 'permuteGrid' --
-- this library's own benchmark executable among them -- and the coordinate
-- machinery behind 'coordPosition' and 'allCoord' only unrolls into flat
-- arithmetic when the axis list is known at the point that inlines it.
-- Measured at INLINABLE: 22-29 ms and 85-92 MB for 'transposeGrid' at
-- 300x300, an order of magnitude worse than the per-cell 'tabulateGrid' this
-- is meant to beat, and unchanged by raising the whole project to -O2 -- so
-- it was not an optimisation-level problem, it was this unfolding never
-- being offered anywhere the coordinate machinery could use it. Forcing it
-- with INLINE dropped the same benchmark to 873 μs / 704 KB boxed and
-- 368 μs / 703 KB unboxed -- 82% under the per-cell baseline on both, per
-- @bench/baseline-ghc9.12.3-aarch64-darwin.csv@.
--
-- 'VG.unsafeBackpermute': every entry of the table is @coordPosition (f c)@
-- for some real @Coord cs@ @c@, so by the same argument as 'indexGrid' it
-- lands in @[0, MaxCoordSize ds)@ -- which is @VG.length@ of the source
-- vector, by the `GridOf` size invariant. The bounds check
-- 'VG.backpermute' would do can never fire.
--
-- A caller cannot supply a bad permutation: they supply a coordinate
-- function, and @Coord ds@ is only inhabited by in-range coordinates, so
-- whatever @f@ returns is safe to look up.
-- @IsCoordList ds@ is likewise gone: the table is @coordPosition@ of whatever
-- @f@ returns, and that is now a field read rather than a fold.
permuteGrid ::
       forall v cs ds a.
       (VG.Vector v a, VG.Vector v Int, IsCoordList cs)
    => (Coord cs -> Coord ds)
    -> GridOf v ds a
    -> GridOf v cs a
permuteGrid f (Grid v) = Grid (VG.unsafeBackpermute v idx)
  where
    idx = VG.fromListN (coordSpaceSize @cs) $ map (coordPosition . f) allCoord
{-# INLINE permuteGrid #-}

transposeGrid ::
     ( VG.Vector v a
     , VG.Vector v Int
     , IsCoord h
     , IsCoord w
     , GHC.KnownNat x
     , GHC.KnownNat y
     , 1 <= y
     , 1 <= x
     )
  => GridOf v '[ w x, h y] a
  -> GridOf v '[ h y, w x] a
transposeGrid = permuteGrid transposeCoord
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
    Grid <$>
    traverseChunks (fromIntegral (GHC.natVal (Proxy @(MaxCoordSize as)))) f v
{-# INLINABLE mapLowerDim #-}

-- | The engine shared by 'mapLowerDim' and 'tiles': split a flat vector into
-- equally sized chunks, traverse each chunk as a sub-grid, and concatenate the
-- results back into one vector. The two callers differ only in how the chunk
-- size is computed -- @'MaxCoordSize' as@ for 'mapLowerDim', where the chunk
-- count is fixed by the axis @c@ being peeled off, versus @'MaxCoordSize'
-- (small ': rest)@ for 'tiles', where the chunk size is fixed and the count
-- falls out of the vector's length.
traverseChunks ::
     forall v x y as bs f. (VG.Vector v x, VG.Vector v y, Applicative f)
  => Int
  -> (GridOf v as x -> f (GridOf v bs y))
  -> v x
  -> f (v y)
traverseChunks size f =
    fmap VG.concat . traverse (fmap unGrid . f . Grid) . splitVectorBySize size
{-# INLINE traverseChunks #-}

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

-- | The geometry of one axis inside a flat row-major vector: how many
-- elements the axis has, and how far apart consecutive ones are.
--
-- Row-major means the axes below @n@ vary fastest, so the elements of a
-- fibre along axis @n@ sit @'MaxCoordSize'@-of-the-axes-below apart, and
-- @'MaxCoordSize'@ of the axes at @n@ and below is the block that one
-- combination of the axes /above/ @n@ occupies. Both are products of
-- statically known sizes, so at a concrete axis list this pair is two
-- literals.
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
  -- | The size of axis @n@ and its stride, in that order. See 'mapAxis'.
  --
  -- This is all the recursion produces now. It used to carry the whole
  -- operation -- a @mapAxisImpl@ that peeled one axis per level with
  -- 'mapLowerDim', splitting and re-concatenating the vector at every one
  -- (sized-grid-adr.5).
  axisSizeAndStride :: (Int, Int)

-- | The target axis is the head, so the axes below it are all of @as@.
instance {-# OVERLAPPING #-} (KnownNat n, AllSizedKnown as) =>
         MapAxis 0 (c n ': as) (c n) where
  axisSizeAndStride =
    ( fromIntegral (GHC.natVal (Proxy @n))
    , fromIntegral (GHC.natVal (Proxy @(MaxCoordSize as))))
  {-# INLINE axisSizeAndStride #-}

-- | Peeling an axis off the front changes neither the target axis's size nor
-- its stride: both are products over the axes at or below it, and the one
-- being dropped is above.
instance {-# OVERLAPPABLE #-} MapAxis (n - 1) as c => MapAxis n (c0 ': as) c where
  axisSizeAndStride = axisSizeAndStride @(n - 1) @as @c
  {-# INLINE axisSizeAndStride #-}

-- | The engine under 'mapAxis': apply @f@ to every fibre of a flat row-major
-- vector along an axis of the given size and stride.
--
-- A fibre is @axisSize@ elements @stride@ apart, and the fibres partition the
-- vector: the ones starting in @[b, b + stride)@ cover the block
-- @[b, b + axisSize * stride)@ exactly, and the blocks tile the vector. So
-- every element is read once, written once, and belongs to one call of @f@.
--
-- Gather-apply-scatter, rather than the two whole-grid transposes this used
-- to be (sized-grid-adr.5). @f@ takes a @'GridOf' v '[c] x@, which is a
-- contiguous vector, so the gather cannot be avoided while that is its type
-- -- but it copies one fibre at a time, where transposing copied the whole
-- grid to bring the fibres contiguous, copied it again to reassemble, and
-- copied it a third time to put the axes back.
--
-- @stride == 1@ is the innermost axis, whose fibres are already contiguous.
-- It skips the gather /and/ the mutable scatter: the fibres are slices,
-- 'VG.concat' puts the results back in order, and the whole operation is one
-- allocation.
--
-- Unsafe indexing throughout, and the bounds are the ones above: @base@ runs
-- over @[0, len)@ with @base + (axisSize - 1) * stride < len@ by
-- construction, so no @unsafeIndex@, @unsafeRead@ or @unsafeWrite@ here can
-- leave the vector.
--
-- @INLINE@, not @INLINABLE@, and the difference is load-bearing -- see
-- 'scanAxisStrided', where it is measured.
mapAxisStrided ::
     forall v x y. (VG.Vector v x, VG.Vector v y)
  => Int -- ^ The axis's size: how many elements a fibre has.
  -> Int -- ^ The axis's stride: how far apart consecutive elements of a fibre are.
  -> (v x -> v y)
  -> v x
  -> v y
mapAxisStrided axisSize stride f v
  | stride == 1 && axisSize > 0 = VG.concat (map f (splitVectorBySize axisSize v))
  | otherwise =
      VG.create $ do
        out <- VGM.unsafeNew len
        let scatterFrom base =
              VG.imapM_ (\k -> VGM.unsafeWrite out (base + k * stride)) $
              f (VG.generate axisSize (\k -> VG.unsafeIndex v (base + k * stride)))
            fibresOf blockStart base
              | base >= blockStart + stride = pure ()
              | otherwise = scatterFrom base >> fibresOf blockStart (base + 1)
            blocks blockStart
              | blockStart >= len = pure ()
              | otherwise =
                  fibresOf blockStart blockStart >> blocks (blockStart + block)
        blocks 0
        pure out
  where
    len = VG.length v
    block = axisSize * stride
{-# INLINE mapAxisStrided #-}

-- | 'scanAxis''s engine, and the reason it does not go through
-- 'mapAxisStrided': a prefix scan needs no fibre in hand, only the element
-- one stride back, so it can be done in a single in-order pass with the
-- result vector as its own accumulator and nothing else allocated
-- (sized-grid-adr.5).
--
-- The first @stride@ elements of each block are the fibres' first elements
-- and copy across unchanged; every later element combines the one a stride
-- behind it -- already written, already forced -- with its own. Both the
-- reads and the writes run straight up the vector, unlike the strided walk
-- 'mapAxisStrided' has to make.
--
-- Walking whole fibres instead, one at a time with the running total in an
-- argument rather than read back out of @out@, was written and measured
-- first: it is the obvious shape and it is slower, 679 us against 575 us
-- boxed and 254 us against 68 us unboxed on @'scanAxis' 0@ over 300x300.
-- The accumulator argument saves a read; taking the whole grid in
-- @stride@-sized strides costs more than the read does.
--
-- Strict in the accumulator, for the reason 'scanl1Grid' is: a running total
-- written unforced into a boxed vector leaves a chain of thunks as long as
-- the axis.
--
-- @INLINE@ rather than @INLINABLE@, and it is worth 2.1x boxed and 8.7x
-- unboxed on the same benchmark. @f@ is an argument, so with the loop left
-- behind a call it stays unknown, every combined value is a boxed thunk
-- passed to it, and the pass allocates a word per cell. Inlined at a call
-- site where @f@ is @(+)@, the accumulator unboxes and the whole scan
-- allocates its result and nothing else: 703 KB for 90,000 'Int's.
scanAxisStrided ::
     forall v a. VG.Vector v a
  => Int -- ^ The axis's size: how many elements a fibre has.
  -> Int -- ^ The axis's stride: how far apart consecutive elements of a fibre are.
  -> (a -> a -> a)
  -> v a
  -> v a
scanAxisStrided axisSize stride f v
  | stride == 1 && axisSize > 0 =
      VG.concat (map (VG.scanl1' f) (splitVectorBySize axisSize v))
  | otherwise =
      VG.create $ do
        out <- VGM.unsafeNew len
        let heads blockStart i
              | i >= blockStart + stride = pure ()
              | otherwise = do
                  let !a0 = VG.unsafeIndex v i
                  VGM.unsafeWrite out i a0
                  heads blockStart (i + 1)
            rest blockEnd i
              | i >= blockEnd = pure ()
              | otherwise = do
                  prev <- VGM.unsafeRead out (i - stride)
                  let !acc = f prev (VG.unsafeIndex v i)
                  VGM.unsafeWrite out i acc
                  rest blockEnd (i + 1)
            blocks blockStart
              | blockStart >= len = pure ()
              | otherwise = do
                  heads blockStart blockStart
                  rest (blockStart + block) (blockStart + stride)
                  blocks (blockStart + block)
        blocks 0
        pure out
  where
    len = VG.length v
    block = axisSize * stride
{-# INLINE scanAxisStrided #-}

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
--
-- 'axis' is the same operation as a 'Setter'.
mapAxis ::
     forall v cs x y c. forall n -> (MapAxis n cs c, VG.Vector v x, VG.Vector v y)
  => (GridOf v '[c] x -> GridOf v '[c] y)
  -> GridOf v cs x
  -> GridOf v cs y
mapAxis n f (Grid v) =
    let (axisSize, stride) = axisSizeAndStride @n @cs @c
     in Grid (mapAxisStrided axisSize stride (unGrid . f . Grid) v)
{-# INLINE mapAxis #-}

-- | Enumerate the fibres along one named axis in row-major order.
axisFibres ::
     forall v cs a c. forall n -> (MapAxis n cs c, VG.Vector v a)
  => GridOf v cs a
  -> [GridOf v '[c] a]
axisFibres n (Grid v) =
    let (axisSize, stride) = axisSizeAndStride @n @cs @c
        block = axisSize * stride
        len = VG.length v
        fibre blockStart base
          | base >= blockStart + stride = []
          | otherwise =
              Grid (VG.generate axisSize (\k -> VG.unsafeIndex v (base + k * stride)))
              : fibre blockStart (base + 1)
        blocks blockStart
          | blockStart >= len = []
          | otherwise = fibre blockStart blockStart ++ blocks (blockStart + block)
     in blocks 0
{-# INLINABLE axisFibres #-}

-- | 'mapAxis' as an optic: a 'Setter' whose foci are the fibres along axis
-- @n@, one for every combination of the other axes.
--
-- > over (axis 1) (mapGrid negate) g   -- what mapAxis 1 (mapGrid negate) gives
--
-- What the optic adds over the bare function is composition. A 'Setter' goes
-- in a chain with the other setters; a function does not.
--
-- > over (mapped . axis 0) (scanl1Grid (+)) gridsInSomeFunctor
--
-- @'scanAxis' n f@ agrees with @'over' ('axis' n) ('scanl1Grid' f)@ on every
-- grid, and is tested against it, but is not defined that way: a scan needs
-- no fibre in hand, so it walks the axis directly (see 'scanAxisStrided').
--
-- A 'Setter' and no more. The fibres along one axis are disjoint and cover
-- the grid, so a lawful 'Traversal' does exist -- it would additionally read
-- the fibres out ('Control.Lens.toListOf') and admit fallible per-fibre
-- transforms ('Control.Lens.traverseOf'). What stands in the way is no
-- longer the implementation, which sized-grid-adr.5 has now settled, but
-- what the 'Traversal' would cost the 'Setter': an 'Applicative'
-- 'mapAxisStrided' has to hold every fibre at once to sequence the effects,
-- where the loop below holds one, so @'over' ('axis' n)@ would pay for a
-- generality it never uses. See sized-grid-0s1d.
axis ::
     forall v cs x y c. forall n -> (MapAxis n cs c, VG.Vector v x, VG.Vector v y)
  => Setter (GridOf v cs x) (GridOf v cs y) (GridOf v '[c] x) (GridOf v '[c] y)
axis n = sets (mapAxis n)
{-# INLINABLE axis #-}

-- | Prefix-scan one named axis of a grid, independently for every
-- combination of the others.
--
-- > scanAxis 1 (+) g   -- rather than scanAxis (Proxy @1) (+) g
--
-- The summed-area-table build-up that motivated this, restated without the
-- transpose trick 'mapAxis' retires:
--
-- > sat = scanAxis 0 (+) . scanAxis 1 (+) . tabulateGrid power
--
-- Equal to @'mapAxis' n ('scanl1Grid' f)@, which is how it used to be
-- written, but not built from it: a scan reads one element back rather than
-- a whole fibre, which is a single in-order pass over the grid and one
-- allocation (sized-grid-adr.5). On that summed-area build it is now 1.6x
-- the hand-fused @'transposeGrid'@ pipeline boxed and 2.2x it unboxed,
-- where before the rewrite it was 2.8x and 3.2x /slower/ than the same
-- pipeline.
scanAxis ::
     forall v cs a c. forall n -> (MapAxis n cs c, VG.Vector v a)
  => (a -> a -> a)
  -> GridOf v cs a
  -> GridOf v cs a
scanAxis n f (Grid v) =
    let (axisSize, stride) = axisSizeAndStride @n @cs @c
     in Grid (scanAxisStrided axisSize stride f v)
{-# INLINE scanAxis #-}

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
-- @1 <= x@ and @IsCoordList cs@ are what @(':|')@ costs after
-- sized-grid-adr.16: splitting a coordinate is a division by the tail's
-- stride, so the tail's sizes have to be in scope, and the head axis has to
-- be one 'Data.Grid.Sized.Coord.Class.IsCoordLifted' can speak for. Neither
-- narrows what this instance covers --- an axis of size zero has no
-- coordinates to shrink.
instance ( KnownNat x
         , KnownNat z
         , AllSizedKnown as
         , IsCoord c
         , IsCoordList cs
         , ShrinkableGrid cs as bs
         , 1 <= x
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

-- | 'gridTiles' as an optic. The @Mod ~ 0@ constraint that makes 'gridTiles'
-- total is exactly the statement that the tiles are disjoint and exactly
-- partition the grid, which is the precondition for a lawful 'Traversal':
-- setting through disjoint, covering foci has only one sensible meaning,
-- unlike 'gridWindows' (see 'windows'), whose foci overlap.
--
-- The write-back concatenates the (possibly modified) tiles in order, which
-- reproduces the source whenever the traversal is used as a getter -- the
-- same fact 'gridTiles'\'s own Haddock states for the read-only direction.
-- It shares its engine with 'mapLowerDim' via 'traverseChunks'.
tiles :: forall small v big rest a.
          ( VG.Vector v a
          , KnownNat (MaxCoordSize (small ': rest))
          , CoordNat big `Mod` CoordNat small ~ 0
          )
       => Traversal' (GridOf v (big ': rest) a) (GridOf v (small ': rest) a)
tiles f (Grid v) =
    requiring @(CoordNat big `Mod` CoordNat small ~ 0) $
    Grid <$>
    traverseChunks (fromIntegral $ natVal (Proxy @(MaxCoordSize (small ': rest)))) f v
{-# INLINABLE tiles #-}

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

-- | 'gridWindows' as an optic -- and, on purpose, no more than a 'Fold'.
--
-- A window of @Ordinal 3@ over an @Ordinal 9@ axis puts cell 2 in three
-- overlapping windows (see 'gridWindows'). A 'Traversal'\'s foci must be
-- disjoint for @'over' l f@ to have a single meaning; here it would not --
-- @over windows f@ would have to pick which of three writes to cell 2 wins,
-- and whichever it picked, the 'Traversal' law
-- @'over' l f . 'over' l g == 'over' l (f . g)@ would fail. So the only
-- lawful optic over 'gridWindows' is read-only.
--
-- Do not be tempted to write this as a 'Traversal' on the grounds that a
-- caller can be trusted to use it read-only: the failure of the law is
-- silent, wrong values rather than a type error, which is the one failure
-- mode this library organises its types against.
windows :: forall small v big rest a.
           ( VG.Vector v a
           , AllSizedKnown rest
           , KnownNat (CoordNat small)
           , CoordNat small <= CoordNat big
           )
        => Fold (GridOf v (big ': rest) a) (GridOf v (small ': rest) a)
windows = folding (gridWindows @small)
{-# INLINABLE windows #-}
