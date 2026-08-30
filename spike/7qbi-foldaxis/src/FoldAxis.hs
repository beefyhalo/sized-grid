{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Six ways to fold one named axis away, for @sized-grid-7qbi@.
--
-- Every one of them has the same type: a strict left fold along axis @n@,
-- returning a grid with that axis removed. They differ only in how the walk
-- is organised -- output-driven, input-driven, or built out of the library's
-- existing pieces -- and the point of the spike is which of those the boxed
-- and unboxed representations prefer.
--
-- Nothing here is a candidate to be copied wholesale into the library: the
-- winner is, the rest exist so the winner is a measurement rather than a
-- preference.
module FoldAxis
  ( DropAxis,
    foldAxisGenerate,
    foldAxisWrite,
    foldAxisSweep,
    foldAxisFibres,
    foldAxisSlices,
    foldAxisSplit,
    foldAxisTotal,
    foldAxisNonEmpty,
  )
where

import Data.Grid.Sized
import Data.Grid.Sized.Unsafe (unsafeGridFromVector)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import GHC.TypeLits (KnownNat, Nat, natVal, type (-))

-- | The axis list with the axis at position @n@ removed: the result shape of
-- every fold below.
--
-- A closed family, deliberately not an associated type of 'MapAxis'. The long
-- note on that class records why an associated type does not work there --
-- GHC checks the two overlapping instances' equations for confluence and the
-- abstract recursive case does not pass -- and the same argument applies to
-- any family that would have to be defined instance by instance. This one is
-- defined in one place, matches top to bottom like any closed family, and
-- reduces the moment @n@ is a literal, which it is at every call site
-- @RequiredTypeArguments@ produces.
type family DropAxis (n :: Nat) (cs :: [Type]) :: [Type] where
  DropAxis 0 (c ': cs) = cs
  DropAxis n (c ': cs) = c ': DropAxis (n - 1) cs

-- * The address arithmetic every candidate shares

--
-- Row-major means a position decomposes as
--
-- > p = hi * (axisSize * stride) + i * stride + lo
--
-- where @i@ is the index along axis @n@, @lo < stride@ is the combined index
-- of the axes below it and @hi@ is the combined index of the axes above.
-- Deleting axis @n@ deletes the middle term, so the fold that consumes every
-- @i@ at fixed @(hi, lo)@ writes to
--
-- > o = hi * stride + lo
--
-- which is exactly the row-major position of the same cell in @DropAxis n cs@.
-- So the output needs no permutation and no size evidence of its own: it is
-- the input's length divided by the axis's size, filled in the order the
-- fibres already come out in.

-- | Output-driven: one output cell at a time, gathering its fibre with a
-- strided inner loop that keeps the accumulator in an argument.
--
-- Reads jump @stride@ at a time; writes are sequential and there is no
-- mutable state beyond whatever 'VG.generate' uses.
foldAxisGenerate ::
  forall v cs x y c.
  forall n ->
  (MapAxis n cs c, VG.Vector v x, VG.Vector v y) =>
  (y -> x -> y) ->
  y ->
  GridOf v cs x ->
  GridOf v (DropAxis n cs) y
foldAxisGenerate n f z g =
  unsafeGridFromVector $ VG.generate outLen fibreFold
  where
    v = gridVector g
    (axisSize, stride) = axisSizeAndStride @n @cs @c
    len = VG.length v
    outLen = len `quot` axisSize
    block = axisSize * stride
    fibreFold o =
      case o `quotRem` stride of
        (hi, lo) -> go z axisSize (hi * block + lo)
    go !acc !k !p
      | k <= 0 = acc
      | otherwise = go (f acc (VG.unsafeIndex v p)) (k - 1) (p + stride)
{-# INLINE foldAxisGenerate #-}

-- | 'foldAxisGenerate''s loop with 'VG.generate' taken out of it: the same
-- output-driven walk, writing into a mutable vector directly.
--
-- Here to say whether a difference between the output-driven and input-driven
-- shapes is really about the walk or about what @'VG.generate'@ costs per
-- cell. Nothing else distinguishes it from 'foldAxisGenerate'.
foldAxisWrite ::
  forall v cs x y c.
  forall n ->
  (MapAxis n cs c, VG.Vector v x, VG.Vector v y) =>
  (y -> x -> y) ->
  y ->
  GridOf v cs x ->
  GridOf v (DropAxis n cs) y
foldAxisWrite n f z g = unsafeGridFromVector $ VG.create $ do
  out <- VGM.unsafeNew outLen
  let outer !o !hi !lo
        | o >= outLen = pure ()
        | lo >= stride = outer o (hi + 1) 0
        | otherwise = do
            -- Forced before the write: a boxed vector would otherwise hold
            -- a fibre-long thunk per cell, which is the hazard 'scanl1Grid'
            -- is strict for.
            let !acc = go z axisSize (hi * block + lo)
            VGM.unsafeWrite out o acc
            outer (o + 1) hi (lo + 1)
      go !acc !k !p
        | k <= 0 = acc
        | otherwise = go (f acc (VG.unsafeIndex v p)) (k - 1) (p + stride)
  outer 0 0 0
  pure out
  where
    v = gridVector g
    (axisSize, stride) = axisSizeAndStride @n @cs @c
    outLen = VG.length v `quot` axisSize
    block = axisSize * stride
{-# INLINE foldAxisWrite #-}

-- | Input-driven: one pass straight up the input vector, with the partial
-- results held in the mutable output and read back a stride at a time.
--
-- This is the shape 'scanAxis' won with -- see @scanAxisStrided@ in the
-- library, which beat the whole-fibre walk because taking the grid in
-- stride-sized steps costs more than the extra read does. A fold is not a
-- scan, though: a scan writes each output cell once, where this one writes
-- every output cell @axisSize@ times.
foldAxisSweep ::
  forall v cs x y c.
  forall n ->
  (MapAxis n cs c, VG.Vector v x, VG.Vector v y) =>
  (y -> x -> y) ->
  y ->
  GridOf v cs x ->
  GridOf v (DropAxis n cs) y
foldAxisSweep n f z g = unsafeGridFromVector $ VG.create $ do
  out <- VGM.replicate outLen z
  let -- One block of the input: @axisSize@ rows of @stride@ cells, all
      -- landing on the same @stride@ output cells.
      rows !p !end !o0
        | p >= end = pure ()
        | otherwise = cells p o0 (o0 + stride) >> rows (p + stride) end o0
      cells !p !o !oEnd
        | o >= oEnd = pure ()
        | otherwise = do
            acc <- VGM.unsafeRead out o
            let !acc' = f acc (VG.unsafeIndex v p)
            VGM.unsafeWrite out o acc'
            cells (p + 1) (o + 1) oEnd
      blocks !start !o0
        | start >= len = pure ()
        | otherwise =
            rows start (start + block) o0 >> blocks (start + block) (o0 + stride)
  blocks 0 0
  pure out
  where
    v = gridVector g
    (axisSize, stride) = axisSizeAndStride @n @cs @c
    len = VG.length v
    outLen = len `quot` axisSize
    block = axisSize * stride
{-# INLINE foldAxisSweep #-}

-- | What a caller writes today with the published API: gather every fibre,
-- fold each one, repack. The control.
--
-- 'axisFibres' hands the fibres back in exactly the output's order, so the
-- repack is a 'VG.fromListN' and nothing more. The cost is that each fibre is
-- a copy -- 'fibreAt' materialises it -- and the list is a cons cell per
-- output cell.
foldAxisFibres ::
  forall v cs x y c.
  forall n ->
  (MapAxis n cs c, VG.Vector v x, VG.Vector v y) =>
  (y -> x -> y) ->
  y ->
  GridOf v cs x ->
  GridOf v (DropAxis n cs) y
foldAxisFibres n f z g =
  unsafeGridFromVector $
    VG.fromListN outLen $
      map (foldlGrid' f z) (axisFibres n g)
  where
    (axisSize, _stride) = axisSizeAndStride @n @cs @c
    outLen = VG.length (gridVector g) `quot` axisSize
{-# INLINE foldAxisFibres #-}

-- | The innermost axis only: its fibres are contiguous, so each one is a
-- slice and the fold over it is the vector's own.
--
-- @stride == 1@ is the case 'mapAxisStrided' and @scanAxisStrided@ both
-- special-case in the library. This is the same idea for a fold, and it is
-- here to answer whether the general loop needs the special case at all --
-- @'foldAxisGenerate'@ at @stride == 1@ is already a contiguous walk with a
-- @quotRem@ by a literal 1 in front of it.
--
-- Falls back to 'foldAxisGenerate' off the fast path so the two can be
-- compared on the same axis.
foldAxisSlices ::
  forall v cs x y c.
  forall n ->
  (MapAxis n cs c, VG.Vector v x, VG.Vector v y) =>
  (y -> x -> y) ->
  y ->
  GridOf v cs x ->
  GridOf v (DropAxis n cs) y
foldAxisSlices n f z g
  | stride == 1 && axisSize > 0 =
      unsafeGridFromVector $
        VG.generate outLen $
          \o -> VG.foldl' f z (VG.unsafeSlice (o * axisSize) axisSize v)
  | otherwise = foldAxisGenerate n f z g
  where
    v = gridVector g
    (axisSize, stride) = axisSizeAndStride @n @cs @c
    outLen = VG.length v `quot` axisSize
{-# INLINE foldAxisSlices #-}

-- | The outermost axis only: fold the sub-grids into each other pointwise.
--
-- This is what a caller writes for axis 0 without reaching for 'axisFibres'
-- at all -- @splitGrid@ hands back the sub-grids and 'zipWithGrid' combines
-- them -- and it is the other control. It allocates a whole intermediate grid
-- per step, so it should lose; the question is by how much, since it is also
-- the one a reader finds first.
--
-- Restricted to axis 0 by its type, which is why it takes no @n@.
foldAxisSplit ::
  forall v c cs x y.
  (VG.Vector v x, VG.Vector v y, AllSizedKnown cs) =>
  (y -> x -> y) ->
  y ->
  GridOf v (c ': cs) x ->
  GridOf v cs y
foldAxisSplit f z g =
  case foldable (splitGrid g) of
    [] -> error "foldAxisSplit: the outermost axis is empty"
    subs@(s : _) -> foldl' (zipWithGrid f) (mapGrid (const z) s) subs
  where
    foldable :: Grid '[c] (GridOf v cs x) -> [GridOf v cs x]
    foldable = foldr (:) []
{-# INLINE foldAxisSplit #-}

-- | 'foldAxisWrite' with the division taken out of it: the output's length
-- comes from the result type's own @'MaxCoordSize'@, read off with one
-- 'natVal'.
--
-- This is not a performance variant -- the division happens once per call,
-- not per cell -- it is the total one. @len \`quot\` axisSize@ is a division
-- by zero when the axis being folded away is empty, and an empty axis is
-- constructible: 'Data.Grid.Sized.Coord.Class.IsCoordLifted' demands
-- @1 <= CoordNat x@, but 'AllSizedKnown' does not and neither does 'MapAxis',
-- so @'Data.Grid.Sized.gridFromVector' 'Data.Vector.empty'@ at
-- @Grid \'[Ordinal 3, Ordinal 0] Int@ is a 'Just', and folding axis 1 of it
-- dies where it should return three copies of the seed. This returns them:
-- the loop runs @MaxCoordSize (DropAxis n cs)@ times and every fibre is
-- empty, so every output cell is @z@.
--
-- The constraint is discharged by GHC alone wherever the axis list is
-- concrete, which is every call site @RequiredTypeArguments@ produces:
-- 'DropAxis' reduces, 'MaxCoordSize' reduces to a product of literals, and
-- the 'KnownNat' is that literal.
--
-- The first attempt at this length asked for it from a second class -- an
-- @AxisOuter n cs@ mirroring 'MapAxis', whose recursive instance multiplied
-- in each peeled axis's size, since 'MapAxis' itself cannot see the axes
-- above @n@. It typechecks and it is correct, and at -O2 it is the fastest
-- candidate here. At -O1 it is 5.7x slower than this one on the contiguous
-- axis unboxed (521 us against 92 us) with the loop and the allocation
-- otherwise identical: the chain of dictionaries does not collapse to a
-- literal there, and a loop bound that is not a literal is not one the
-- simplifier will work on. Carrying the same constraint but computing the
-- length by division put the benchmark back at 92 us, which is what pins the
-- cost on the method call rather than on the extra dictionary argument. One
-- 'natVal' of a type-level product has no chain to collapse.
foldAxisTotal ::
  forall v cs x y c.
  forall n ->
  ( MapAxis n cs c,
    KnownNat (MaxCoordSize (DropAxis n cs)),
    VG.Vector v x,
    VG.Vector v y
  ) =>
  (y -> x -> y) ->
  y ->
  GridOf v cs x ->
  GridOf v (DropAxis n cs) y
foldAxisTotal n f z g = unsafeGridFromVector $ VG.create $ do
  out <- VGM.unsafeNew outLen
  let outer !o !hi !lo
        | o >= outLen = pure ()
        | lo >= stride = outer o (hi + 1) 0
        | otherwise = do
            let !acc = go z axisSize (hi * block + lo)
            VGM.unsafeWrite out o acc
            outer (o + 1) hi (lo + 1)
      go !acc !k !p
        | k <= 0 = acc
        | otherwise = go (f acc (VG.unsafeIndex v p)) (k - 1) (p + stride)
  outer 0 0 0
  pure out
  where
    v = gridVector g
    (axisSize, stride) = axisSizeAndStride @n @cs @c
    outLen = fromIntegral (natVal (Proxy @(MaxCoordSize (DropAxis n cs))))
    block = axisSize * stride
{-# INLINE foldAxisTotal #-}

-- | The recommendation: 'foldAxisWrite'\'s loop, with the empty axis ruled
-- out by a constraint instead of paid for at run time.
--
-- @'IsCoordLifted' c@ is what every axis of every grid built through the safe
-- API already satisfies -- it is the per-axis obligation of 'IsCoordList',
-- which 'tabulateGrid' and 'Coord' itself demand -- and it carries
-- @1 <= 'CoordNat' c@. So the length division cannot divide by zero, and the
-- length still comes from the vector rather than from type-level evidence,
-- which is the difference 'foldAxisTotal' measures.
foldAxisNonEmpty ::
  forall v cs x y c.
  forall n ->
  ( MapAxis n cs c,
    IsCoordLifted c,
    VG.Vector v x,
    VG.Vector v y
  ) =>
  (y -> x -> y) ->
  y ->
  GridOf v cs x ->
  GridOf v (DropAxis n cs) y
foldAxisNonEmpty n f z g = unsafeGridFromVector $ VG.create $ do
  out <- VGM.unsafeNew outLen
  let outer !o !hi !lo
        | o >= outLen = pure ()
        | lo >= stride = outer o (hi + 1) 0
        | otherwise = do
            let !acc = go z axisSize (hi * block + lo)
            VGM.unsafeWrite out o acc
            outer (o + 1) hi (lo + 1)
      go !acc !k !p
        | k <= 0 = acc
        | otherwise = go (f acc (VG.unsafeIndex v p)) (k - 1) (p + stride)
  outer 0 0 0
  pure out
  where
    v = gridVector g
    (axisSize, stride) = axisSizeAndStride @n @cs @c
    outLen = VG.length v `quot` axisSize
    block = axisSize * stride
{-# INLINE foldAxisNonEmpty #-}
