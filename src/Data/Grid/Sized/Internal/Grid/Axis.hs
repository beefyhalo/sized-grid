{-# LANGUAGE ConstraintKinds #-}

-- | Reaching one named axis of a grid by position, and walking its fibres.
--
-- A fibre along axis @n@ is the elements that differ only in axis @n@. In a
-- flat row-major vector those sit a fixed stride apart, so every operation
-- here is the same loop over @(axisSize, stride)@ -- which 'AxisSize' and
-- 'AxisStride' read off the axis list, and which is two literals once that
-- list is concrete.
module Data.Grid.Sized.Internal.Grid.Axis
  ( AxisAt,
    AxisSize,
    AxisStride,
    KnownAxis,
    mapAxis,
    axisFibres,
    axis,
    scanAxis,
    DropAxis,
    foldAxis',
    reduceAxis,
  )
where

import Control.Lens hiding (index)
import Data.Grid.Sized.Coord
import Data.Grid.Sized.Internal.Grid.Core
import Data.Grid.Sized.Internal.Type (requiring)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import GHC.TypeLits
import GHC.TypeLits qualified as GHC

-- | The type of axis @n@ of @cs@: the element 'mapAxis' hands its function,
-- as @'GridOf' v '['AxisAt' n cs] x@.
--
-- Deliberately a closed type family, not an associated type of a class. An
-- associated type was tried and does not work: inside a recursive class
-- instance at an abstract @n@, GHC checks the instances' equations for
-- confluence the way it would for any open family, and @c@ against
-- @AxisAt (n - 1) as@ does not pass that check even with
-- 'OVERLAPPING'\/'OVERLAPPABLE' on the instances. A standalone closed family
-- has no such check -- each call either matches an equation or is stuck.
type family AxisAt (n :: Nat) (cs :: [Type]) :: Type where
  AxisAt 0 (c ': _) = c
  AxisAt n (_ ': cs) = AxisAt (n - 1) cs

-- | The size of axis @n@ of @cs@: how many elements a fibre along it has.
type family AxisSize (n :: Nat) (cs :: [Type]) :: Nat where
  AxisSize 0 (_ m ': _) = m
  AxisSize n (_ ': cs) = AxisSize (n - 1) cs

-- | The stride of axis @n@ of @cs@ inside a flat row-major vector: how far
-- apart consecutive elements of a fibre along it sit.
--
-- Row-major means the axes below @n@ vary fastest, so a fibre's elements sit
-- @'MaxCoordSize'@-of-the-axes-below apart. Peeling an axis off the front
-- leaves that untouched -- the dropped axis is above @n@ -- so the recursive
-- equation just forwards. Both this and 'AxisSize' are products of statically
-- known sizes, so at a concrete axis list each is a literal.
type family AxisStride (n :: Nat) (cs :: [Type]) :: Nat where
  AxisStride 0 (_ ': cs) = MaxCoordSize cs
  AxisStride n (_ ': cs) = AxisStride (n - 1) cs

-- | Enough statically known sizes to turn axis @n@ of @cs@ into the
-- @(axisSize, stride)@ pair every function here loops over. At a concrete
-- axis list both components reduce to literals and this is discharged for
-- free.
type KnownAxis (n :: Nat) (cs :: [Type]) =
  ( KnownNat (AxisSize n cs),
    KnownNat (AxisStride n cs)
  ) ::
    Constraint

-- | The size of axis @n@ of @cs@ and its stride, in that order: one 'natVal'
-- each off 'AxisSize' and 'AxisStride'. See 'mapAxis'.
axisSizeAndStride ::
  forall n cs ->
  (KnownAxis n cs) =>
  (Int, Int)
axisSizeAndStride n cs =
  ( fromIntegral (GHC.natVal (Proxy @(AxisSize n cs))),
    fromIntegral (GHC.natVal (Proxy @(AxisStride n cs)))
  )
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

-- | The block\/fibre walk every strided operation here is built on, as one
-- @INLINE@ combinator instead of a hand-rolled copy per caller.
--
-- The fibre heads along an axis of the given size and stride are the @stride@
-- bases @[blockStart, blockStart + stride)@ of each block, and the blocks tile
-- the vector in steps of @axisSize * stride@. This visits those bases in
-- row-major order, threading @f@'s accumulator through them; @len@ and the
-- block width are computed here, once, from the vector and the pair.
--
-- The strict monadic left fold, for the @ST@ callers that fuse the walk and
-- allocate nothing but the result -- 'mapAxisStrided', 'reduceAxis'.
-- 'eachFibreBase' is the lazy counterpart for the list-producing callers.
--
-- @INLINE@, so at each call site @f@ is known and the walk is as tight as the
-- copy it replaces -- unboxed counters, no per-fibre allocation. This is the
-- CPS combinator sized-grid-6kor.7's note leaves open, not the lazy
-- @[Int] >>= @ form that note measured and rejected.
eachFibreBaseM ::
  forall v x m a.
  (VG.Vector v x, Monad m) =>
  -- | The whole flat row-major vector: gives @len@.
  v x ->
  -- | The axis's size.
  Int ->
  -- | The axis's stride.
  Int ->
  -- | Per-base action, accumulator first.
  (a -> Int -> m a) ->
  -- | Seed accumulator.
  a ->
  m a
eachFibreBaseM v axisSize stride f = goBlocks 0
  where
    len = VG.length v
    block = axisSize * stride
    goBlocks blockStart acc
      | blockStart >= len = pure acc
      | otherwise = goBases blockStart blockStart acc
    goBases blockStart base acc
      | base >= blockStart + stride = goBlocks (blockStart + block) acc
      | otherwise = f acc base >>= goBases blockStart (base + 1)
{-# INLINE eachFibreBaseM #-}

-- | The lazy right-fold form of 'eachFibreBaseM': visit the fibre heads in
-- row-major order, folding each into the tail of the result with @f@. Left
-- lazy in that tail, so a caller that conses gets a lazy list back -- the
-- contract 'axisFibres' and 'foldAxis'' rely on, against 'mapAxisStrided''s
-- strict @ST@ walk.
eachFibreBase ::
  forall v x b.
  (VG.Vector v x) =>
  -- | The whole flat row-major vector: gives @len@.
  v x ->
  -- | The axis's size.
  Int ->
  -- | The axis's stride.
  Int ->
  -- | Per-base step, result tail second.
  (Int -> b -> b) ->
  -- | Tail once every fibre is folded in.
  b ->
  b
eachFibreBase v axisSize stride f z = goBlocks 0
  where
    len = VG.length v
    block = axisSize * stride
    goBlocks blockStart
      | blockStart >= len = z
      | otherwise = goBases blockStart blockStart
    goBases blockStart base
      | base >= blockStart + stride = goBlocks (blockStart + block)
      | otherwise = f base (goBases blockStart (base + 1))
{-# INLINE eachFibreBase #-}

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
        out <- VGM.unsafeNew (VG.length v)
        let scatterFrom base =
              VG.imapM_ (\k -> VGM.unsafeWrite out (base + k * stride)) $
                f (fibreAt axisSize stride v base)
        eachFibreBaseM v axisSize stride (\() base -> scatterFrom base) ()
        pure out
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
      VG.create $ do
        out <- VGM.unsafeNew len
        let scanBlock blockStart i
              | i >= blockStart + axisSize = pure ()
              | i == blockStart = do
                  let !a0 = VG.unsafeIndex v i
                  VGM.unsafeWrite out i a0
                  scanBlock blockStart (i + 1)
              | otherwise = do
                  prev <- VGM.unsafeRead out (i - 1)
                  let !acc = f prev (VG.unsafeIndex v i)
                  VGM.unsafeWrite out i acc
                  scanBlock blockStart (i + 1)
            blocks blockStart
              | blockStart >= len = pure ()
              | otherwise = do
                  scanBlock blockStart blockStart
                  blocks (blockStart + axisSize)
        blocks 0
        pure out
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
  forall v cs x y.
  forall n ->
  (KnownAxis n cs, VG.Vector v x, VG.Vector v y) =>
  (GridOf v '[AxisAt n cs] x -> GridOf v '[AxisAt n cs] y) ->
  GridOf v cs x ->
  GridOf v cs y
mapAxis n f (Grid v) =
  let (axisSize, stride) = axisSizeAndStride n cs
   in Grid (mapAxisStrided axisSize stride (unGrid . f . Grid) v)
{-# INLINE mapAxis #-}

-- | Enumerate the fibres along one named axis in row-major order.
axisFibres ::
  forall v cs a.
  forall n ->
  (KnownAxis n cs, VG.Vector v a) =>
  GridOf v cs a ->
  [GridOf v '[AxisAt n cs] a]
axisFibres n (Grid v) =
  let (axisSize, stride) = axisSizeAndStride n cs
   in eachFibreBase
        v
        axisSize
        stride
        (\base rest -> Grid (fibreAt axisSize stride v base) : rest)
        []
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
  forall v cs x y.
  forall n ->
  (KnownAxis n cs, VG.Vector v x, VG.Vector v y) =>
  Setter (GridOf v cs x) (GridOf v cs y) (GridOf v '[AxisAt n cs] x) (GridOf v '[AxisAt n cs] y)
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
  forall v cs a.
  forall n ->
  (KnownAxis n cs, VG.Vector v a) =>
  (a -> a -> a) ->
  GridOf v cs a ->
  GridOf v cs a
scanAxis n f (Grid v) =
  let (axisSize, stride) = axisSizeAndStride n cs
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
-- The constraint @'IsCoordLifted' ('AxisAt' n cs)@ excludes the empty axis (zero-sized),
-- because the fold divides the output size by the axis size, which would
-- divide by zero for a zero-sized axis. On a grid built through the safe API,
-- every axis satisfies 'IsCoordLifted' by construction.
--
-- This is the output-driven strict-write loop measured in
-- @docs/superpowers/specs/2026-08-29-axis-fold-design.md@: one output
-- cell at a time, gathering its fibre with a strided inner loop that keeps
-- the accumulator in an argument, forcing before the write.
foldAxis' ::
  forall v cs x y.
  forall n ->
  (KnownAxis n cs, IsCoordLifted (AxisAt n cs), VG.Vector v x, VG.Vector v y) =>
  (y -> x -> y) ->
  y ->
  GridOf v cs x ->
  GridOf v (DropAxis n cs) y
foldAxis' n f z (Grid v) =
  requiring @(IsCoordLifted (AxisAt n cs)) $
    Grid (VG.fromList (eachFibreBase v axisSize stride step []))
  where
    (axisSize, stride) = axisSizeAndStride n cs
    -- One output cell: the strict left fold of the fibre based at base.
    step base rest = foldFibre base 0 z : rest
    foldFibre base i acc
      | i >= axisSize = acc
      | otherwise = foldFibre base (i + 1) $! f acc (VG.unsafeIndex v (base + i * stride))
{-# INLINE foldAxis' #-}

-- | Seedless strict left fold along one named axis, removing it.
--
-- > reduceAxis 1 max g   -- rather than reduceAxis (Proxy @1) max g
--
-- Like 'foldAxis'', but the first element of each fibre seeds the fold.  The
-- @'IsCoordLifted' ('AxisAt' n cs)@ constraint guarantees that every fibre has that first
-- element, so no @Maybe@ or identity is needed.  This is useful for
-- reductions such as @min@ and @max@ which have no suitable identity.
--
-- The fold runs strictly, left to right, in increasing index order along the
-- axis, with the accumulator as the left argument.  It is deliberately not a
-- wrapper around 'foldAxis'': seeding from the first element starts its inner
-- loop at index one, avoiding an extra application of @f@ per fibre.
reduceAxis ::
  forall v cs a.
  forall n ->
  (KnownAxis n cs, IsCoordLifted (AxisAt n cs), VG.Vector v a) =>
  (a -> a -> a) ->
  GridOf v cs a ->
  GridOf v (DropAxis n cs) a
reduceAxis n f (Grid v) =
  requiring @(IsCoordLifted (AxisAt n cs)) $
    Grid $
      VG.create $ do
        out <- VGM.unsafeNew (len `quot` axisSize)
        let reduceFibre base i acc
              | i >= axisSize = acc
              | otherwise =
                  reduceFibre base (i + 1) $! f acc (VG.unsafeIndex v (base + i * stride))
            -- Write one output cell, seeded by the fibre's first element, and
            -- carry the running output index forward.
            step outIndex base = do
              let !first = VG.unsafeIndex v base
                  !result = reduceFibre base 1 first
              VGM.unsafeWrite out outIndex result
              pure (outIndex + 1)
        _ <- eachFibreBaseM v axisSize stride step (0 :: Int)
        pure out
  where
    (axisSize, stride) = axisSizeAndStride n cs
    len = VG.length v
{-# INLINE reduceAxis #-}
