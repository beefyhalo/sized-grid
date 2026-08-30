{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Reaching one named axis of a grid by position, and walking its fibres.
--
-- A fibre along axis @n@ is the elements that differ only in axis @n@. In a
-- flat row-major vector those sit a fixed stride apart, so every operation
-- here is the same loop over @(axisSize, stride)@ -- which 'MapAxis' computes
-- from the axis list, and which is two literals once that list is concrete.
module Data.Grid.Sized.Internal.Grid.Axis
  ( MapAxis (..),
    mapAxis,
    axisFibres,
    axis,
    scanAxis,
    DropAxis,
    foldAxis',
  )
where

import Control.Lens hiding (index)
import Data.Grid.Sized.Coord
import Data.Grid.Sized.Internal.Grid.Core
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import GHC.TypeLits
import GHC.TypeLits qualified as GHC

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
  -- @mapLowerDim@, splitting and re-concatenating the vector at every one
  -- (sized-grid-adr.5).
  axisSizeAndStride :: (Int, Int)

-- | The target axis is the head, so the axes below it are all of @as@.
instance
  {-# OVERLAPPING #-}
  (KnownNat n, AllSizedKnown as) =>
  MapAxis 0 (c n ': as) (c n)
  where
  axisSizeAndStride =
    ( fromIntegral (GHC.natVal (Proxy @n)),
      fromIntegral (GHC.natVal (Proxy @(MaxCoordSize as)))
    )
  {-# INLINE axisSizeAndStride #-}

-- | Peeling an axis off the front changes neither the target axis's size nor
-- its stride: both are products over the axes at or below it, and the one
-- being dropped is above.
instance {-# OVERLAPPABLE #-} (MapAxis (n - 1) as c) => MapAxis n (c0 ': as) c where
  axisSizeAndStride = axisSizeAndStride @(n - 1) @as @c
  {-# INLINE axisSizeAndStride #-}

-- | One fibre, gathered: the @axisSize@ elements of @v@ that start at @base@
-- and sit @stride@ apart, copied out contiguous.
--
-- The one definition of the fibre read. 'mapAxisStrided' gathers a fibre to
-- hand to @f@ and 'axisFibres' gathers the same fibre to hand to its caller,
-- and they were the same expression written twice.
--
-- 'VG.unsafeIndex', on the bounds 'mapAxisStrided' states: every @base@ a
-- caller here walks satisfies @base + (axisSize - 1) * stride < 'VG.length' v@
-- by construction, so no read can leave the vector.
--
-- Only the read is shared. The two walks that produce the @base@ values are
-- not: 'mapAxisStrided' sequences them in @ST@ so the loop fuses and nothing
-- but the result is allocated, and 'axisFibres' produces a lazy list.
--
-- Sharing those too was written and measured, so it is not re-tried blind
-- (sized-grid-6kor.7). A @fibreBases block stride len@ producing
-- @[0, block .. len - 1] >>= \\b -> [b .. b + stride - 1]@, consumed by
-- @'mapM_' scatterFrom@ here and by 'map' in 'axisFibres', is the whole
-- extraction, and what it costs depends on the optimisation level of the code
-- that /calls/ this: 'mapAxisStrided' is @INLINE@, so its body is optimised
-- in the caller's context. On @mapAxis 0@ over an unboxed 300x300 grid, at
-- @-O2@ the list fuses away and the benchmark is unmoved -- 145 us against
-- 149 us -- while at @-O1@ it does not fuse and the same benchmark costs
-- 120%: 170 us against 374 us, on 2.2 MB either way, the scatter running off
-- a cons cell per fibre instead of an unboxed counter.
--
-- @-O1@ is what cabal gives an executable by default, so that is the arm that
-- decides it: two saved lines are not worth doubling the consumer's inner
-- loop. sized-grid-y99h tracks the same @-O1@\/@-O2@ split elsewhere in the
-- library, and the numbers above were taken the way it says -- by rebuilding
-- the benchmark component with @--ghc-options=-O2@ and comparing.
fibreAt ::
  forall v a.
  (VG.Vector v a) =>
  -- | The axis's size: how many elements the fibre has.
  Int ->
  -- | The axis's stride: how far apart consecutive elements of it are.
  Int ->
  -- | The whole flat row-major vector.
  v a ->
  -- | Where the fibre starts.
  Int ->
  v a
fibreAt axisSize stride v base =
  VG.generate axisSize (\k -> VG.unsafeIndex v (base + k * stride))
{-# INLINE fibreAt #-}

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
  forall v x y.
  (VG.Vector v x, VG.Vector v y) =>
  -- | The axis's size: how many elements a fibre has.
  Int ->
  -- | The axis's stride: how far apart consecutive elements of a fibre are.
  Int ->
  (v x -> v y) ->
  v x ->
  v y
mapAxisStrided axisSize stride f v
  | stride == 1 && axisSize > 0 = VG.concat (map f (splitVectorBySize axisSize v))
  | otherwise =
      VG.create $ do
        out <- VGM.unsafeNew len
        let scatterFrom base =
              VG.imapM_ (\k -> VGM.unsafeWrite out (base + k * stride)) $
                f (fibreAt axisSize stride v base)
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
  forall v a.
  (VG.Vector v a) =>
  -- | The axis's size: how many elements a fibre has.
  Int ->
  -- | The axis's stride: how far apart consecutive elements of a fibre are.
  Int ->
  (a -> a -> a) ->
  v a ->
  v a
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
-- independently for every combination of the others -- @mapLowerDim@
-- generalised from the outermost axis to any of them by position.
--
-- > mapAxis 1 f g   -- rather than mapAxis (Proxy @1) f g
--
-- Lets a caller reach an axis by name instead of physically rotating the
-- grid to bring it to the front (@transposeGrid . f . transposeGrid@), a
-- trick that stops working past two dimensions since there is no
-- @transposeGrid@ for an arbitrary pair of axes.
--
-- 'axis' is the same operation as a 'Setter'.
mapAxis ::
  forall v cs x y c.
  forall n ->
  (MapAxis n cs c, VG.Vector v x, VG.Vector v y) =>
  (GridOf v '[c] x -> GridOf v '[c] y) ->
  GridOf v cs x ->
  GridOf v cs y
mapAxis n f (Grid v) =
  let (axisSize, stride) = axisSizeAndStride @n @cs @c
   in Grid (mapAxisStrided axisSize stride (unGrid . f . Grid) v)
{-# INLINE mapAxis #-}

-- | Enumerate the fibres along one named axis in row-major order.
axisFibres ::
  forall v cs a c.
  forall n ->
  (MapAxis n cs c, VG.Vector v a) =>
  GridOf v cs a ->
  [GridOf v '[c] a]
axisFibres n (Grid v) =
  let (axisSize, stride) = axisSizeAndStride @n @cs @c
      block = axisSize * stride
      len = VG.length v
      fibre blockStart base
        | base >= blockStart + stride = []
        | otherwise =
            Grid (fibreAt axisSize stride v base)
              : fibre blockStart (base + 1)
      blocks blockStart
        | blockStart >= len = []
        | otherwise = fibre blockStart blockStart ++ blocks (blockStart + block)
   in blocks 0
{-# INLINEABLE axisFibres #-}

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
  forall v cs x y c.
  forall n ->
  (MapAxis n cs c, VG.Vector v x, VG.Vector v y) =>
  Setter (GridOf v cs x) (GridOf v cs y) (GridOf v '[c] x) (GridOf v '[c] y)
axis n = sets (mapAxis n)
{-# INLINEABLE axis #-}

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
-- the hand-fused @@transposeGrid@@ pipeline boxed and 2.2x it unboxed,
-- where before the rewrite it was 2.8x and 3.2x /slower/ than the same
-- pipeline.
scanAxis ::
  forall v cs a c.
  forall n ->
  (MapAxis n cs c, VG.Vector v a) =>
  (a -> a -> a) ->
  GridOf v cs a ->
  GridOf v cs a
scanAxis n f (Grid v) =
  let (axisSize, stride) = axisSizeAndStride @n @cs @c
   in Grid (scanAxisStrided axisSize stride f v)
{-# INLINE scanAxis #-}

-- | Type family that removes one axis from an axis list by position.
--
-- The result is the axis list with the axis at position @n@ deleted.
type family DropAxis (n :: Nat) (cs :: [Type]) :: [Type] where
  DropAxis 0 (c ': cs) = cs
  DropAxis n (c ': cs) = c ': DropAxis (n - 1) cs

-- | Strict left fold along one named axis, removing it.
--
-- > foldAxis' 1 (+) 0 g   -- rather than foldAxis' (Proxy @1) (+) 0 g
--
-- Fold every fibre along axis @n@ with the strict left fold @f@, producing
-- a lower-dimensional grid with that axis removed. The fold runs strictly,
-- left to right, in increasing index order along the axis, with the
-- accumulator as the left argument.
--
-- The output is a grid whose shape is @DropAxis n cs@: the input's axis
-- list with the folded axis deleted. Every surviving axis keeps its
-- boundary policy -- the result has the same axis types in the same
-- positions, just with one removed.
--
-- The constraint @'IsCoordLifted' c@ excludes the empty axis (zero-sized),
-- because the fold divides the output size by the axis size, which would
-- divide by zero for a zero-sized axis. On a grid built through the safe API,
-- every axis satisfies 'IsCoordLifted' by construction.
--
-- This is the output-driven strict-write loop measured in
-- @docs/superpowers/specs/2026-08-29-axis-fold-design.md@: one output
-- cell at a time, gathering its fibre with a strided inner loop that keeps
-- the accumulator in an argument, forcing before the write.
foldAxis' ::
  forall v cs x y c.
  forall n ->
  (MapAxis n cs c, IsCoordLifted c, VG.Vector v x, VG.Vector v y) =>
  (y -> x -> y) ->
  y ->
  GridOf v cs x ->
  GridOf v (DropAxis n cs) y
foldAxis' n f z (Grid v) =
  Grid (VG.fromList (blocks 0))
  where
   (axisSize, stride) = axisSizeAndStride @n @cs @c
   len = VG.length v
   block = axisSize * stride
   -- Fold a single fibre starting from base
   foldFibre base i acc
     | i >= axisSize = acc
     | otherwise = foldFibre base (i + 1) $! f acc (VG.unsafeIndex v (base + i * stride))
   -- Process all fibres in a block
   fibresOf blockStart base
     | base >= blockStart + stride = []
     | otherwise = foldFibre base 0 z : fibresOf blockStart (base + 1)
   -- Process all blocks
   blocks blockStart
     | blockStart >= len = []
     | otherwise = fibresOf blockStart blockStart ++ blocks (blockStart + block)
{-# INLINE foldAxis' #-}



