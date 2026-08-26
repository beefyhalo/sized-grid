{-# LANGUAGE AllowAmbiguousTypes #-}

-- |
-- Module      :  Data.Grid.Sized.Stencil
-- License     :  MIT -style (see the file LICENSE)
--
-- A neighbourhood, precomputed once for a grid /type/ and then run over as many
-- grids of that type as you like.
--
-- The shape of a cellular-automaton step is
--
-- > imapGrid (\c here -> rule here (map (indexGrid g) (neighbours c))) g
--
-- and the cost of it is not the rule. Per cell it walks the axis list to build
-- a @Coord@ per neighbour --- an @NP I cs@ spine of boxes --- applies each
-- axis's boundary policy to decide whether that neighbour exists, and then
-- throws the whole coordinate away again through `coordPosition` to get the one
-- `Int` the vector is actually read at.
--
-- None of that depends on the grid. For a fixed axis list and radius, /which
-- vector positions are the neighbours of position i/ is a function of the type
-- alone: the axis sizes fix the strides, and the per-axis boundary policies fix
-- what happens at an edge. A `Stencil` is that function, computed once and
-- stored flat.
--
-- == The table
--
-- A @'Stencil' cs@ holds @'coordSpaceSize' \@cs * 'stencilWidth'@ positions in
-- one unboxed vector, row @i@ being the neighbours of position @i@. The width
-- is the largest neighbour count any cell has; cells with fewer --- which on a
-- bounded axis is every cell near an edge --- are padded with a sentinel, and
-- because the padding is a suffix, `stencilGrid` stops at the first one rather
-- than scanning past it. A grid all of whose axes are
-- `Data.Grid.Sized.Coord.Periodic.Periodic` has no short rows at all.
--
-- == Why you build one and hold it
--
-- Building the table costs one full pass of the very neighbourhood computation
-- it replaces --- rather more than one, because it also lays out a vector --- so
-- a `Stencil` used once is slower than not having bothered. Measured on the
-- @50 x 50@ automaton step in @bench\/Main.hs@, against the
-- @imapGrid@-over-@neighbours@ loop above rather than against
-- @extend neighbourSum@, so that the comparison is the neighbourhood and not
-- also a `Data.Grid.Sized.Focused.FocusedGrid` the rule never reads:
--
-- * the loop, one pass: 652 μs, 2.6 MB
-- * building the table (`mooreStencil`): 926 μs, 6.9 MB
-- * one pass with the table already built: 133 μs, 2.1 MB
--
-- (Building used to be 1.46 ms and 16 MB: before @sized-grid-fup0@,
-- `mooreStencil` built through `stencilFor`'s two-pass path --- once for a
-- length, once for a row (@sized-grid-adr.15@) --- the only path open to a
-- neighbourhood function with no bound anyone can derive. `mooreStencil` and
-- `vonNeumannStencil` do have one: @(2r+1)^d - 1@ from the radius and the
-- axis count, both already in hand. `stencilBounded` lays the table out at
-- that bound and fills it in a single pass, so the neighbourhood function
-- runs once per cell instead of twice. `stencilFor` itself is unchanged and
-- still pays the two passes, for a neighbourhood function --- a knight's
-- move, an asymmetric kernel --- that hands it no bound to allocate at. Net:
-- 1.6x faster and 2.3x less allocated.)
--
-- So a single pass through a stencil is about 1.7x slower than not bothering,
-- the second pass is where it breaks even, and from there each one is
-- roughly 4.9x cheaper. Which is what an automaton does:
--
-- > let s = mooreStencil 1
-- > in iterate (stencilGrid s rule) g !! 100
--
-- measured at 31.0 ms and 210 MB against 77.5 ms and 256 MB for the loop it
-- replaces.
--
-- That is why the table is a value rather than an argument to `stencilGrid`.
-- Written as @stencilGrid 1 rule g@ the sharing would have to come from GHC
-- floating the table out to a CAF, which happens only where the axis-list
-- dictionary is known statically and, when it does happen, retains the table
-- for the lifetime of the process. As a value the sharing is visible in the
-- code that wants it and ends when that code drops it.
--
-- == Why both kernels are @INLINE@ and not @INLINABLE@
--
-- `stencilGrid` and `stencilFoldGrid` carry `INLINE`, and that is a
-- performance decision rather than a stylistic one --- @sized-grid-v6ye@,
-- measured in @spike\/49hi-parexec@ and then again here.
--
-- @INLINABLE@ exposes an unfolding for GHC to /specialise on the types/; it
-- does not oblige GHC to /inline the body/. When GHC declines, the caller's
-- rule stays a lambda-bound variable inside the fill, so `stencilFoldGrid`'s
-- accumulator is a boxed @Int@ that @step@ re-boxes once per neighbour, and
-- the neighbour it folds in is boxed on the way out of an unboxed vector as
-- well. The tell is in the Core: an inner loop reading
-- @case acc of acc1 { I# ipv -> ... step acc1 (I# ds) }@ where a specialised
-- one reads @jump $wgo (+# ww 1#) (+# ww' ds)@. `INLINE` makes the rule known
-- where the loop is built, and the whole row folds in registers.
--
-- On the @50 x 50@ benchmarks below, changing only these two words ---
-- @bench\/baseline-ghc9.12.3-aarch64-darwin.csv@ before this change against
-- the same file after it, two full-suite runs on the same idle laptop:
--
-- * @stencilFoldStep@, one pass, table built: 48.3 μs \/ 479 KB -> 42.5 μs \/ 215 KB
-- * @stencilFoldStep@ x100, boxed: 19.3 ms \/ 47 MB -> 17.7 ms \/ 21 MB
-- * @stencilFoldStep@ x100, unboxed: 10.3 ms \/ 149 MB -> 1.38 ms \/ 1.9 MB
-- * @stencilStep@, one pass, table built: 148 μs \/ 2.4 MB -> 133 μs \/ 2.1 MB
-- * @stencilStep@ x100, boxed: 33.2 ms \/ 237 MB -> 31.0 ms \/ 210 MB
-- * @stencilStep@ x100, unboxed: 12.9 ms \/ 249 MB -> 12.9 ms \/ 221 MB
--
-- What licenses reading those as the pragma is the untouched code in the same
-- two files: @imapGrid over neighbours x100@ moved 77.9 ms -> 77.5 ms, and
-- the @extend neighbourSum@ and @tabulateGrid@ rows agree to within 3%.
-- Allocation is firmer still --- it is byte-identical across repeat runs
-- here, where times drift by tens of percent with the machine's thermal
-- state, so it is the column to read. (Two rows of the old file are stale for
-- an unrelated reason and are not this change: @mooreStencil@ and
-- @stencilStep, table built for this one pass@ both improved at
-- @sized-grid-fup0@, after that file was last recorded.)
--
-- The unboxed fold loop is the extreme case and the one that says what the
-- pragma buys: 1.9 MB across a hundred generations of 2,500 cells is
-- @100 * 2500 * 8@ bytes, the result vectors and /nothing else/. Every byte
-- above that was boxing --- 7.5x faster and 74x less allocated.
--
-- The gather kernel gains far less than the fold kernel, for a legible
-- reason: `stencilGrid` builds a @[a]@ per cell, so a boxed accumulator was a
-- small part of what it already allocated. `stencilFoldGrid` exists precisely
-- so as not to build that list, and boxing was most of what it had left. What
-- the gather kernel does get is 11% less allocation on both representations,
-- and on the unboxed grid that is all it gets: the time is unmoved, because
-- what is left there is the neighbour list itself.
--
-- The cost @INLINE@ is normally weighed against --- code size at the call
-- sites --- did not materialise. Measured on the three consumers in tree,
-- object code got /smaller/: @Data\/Grid\/Sized\/Stencil.o@ 50,112 -> 48,648
-- bytes, @gameOfLife@'s @Main.o@ 86,824 -> 85,368, @bench\/Main.o@ 432,344 ->
-- 422,400. @ising-example@ and @sudoko@ are byte-identical, because neither
-- calls these two. Inlining a kernel this size does grow a call site, but it
-- also lets the specialised loop drop the boxing, the generic vector
-- dictionary and the dead sentinel arithmetic, and here that is worth more.
--
-- == What it does not change
--
-- The boundary policies. The table is built by asking the same
-- `Data.Grid.Sized.Coord.mooreNeighbours` the hand-written loop asks, one cell
-- at a time, so a `Data.Grid.Sized.Coord.Periodic.Periodic` axis wraps and a
-- `Data.Grid.Sized.Coord.Clamped.Clamped` one drops, exactly and only as
-- before. @Test.Stencil@ states that as a property over mixed-policy axis
-- lists.
module Data.Grid.Sized.Stencil
  ( -- * The table
    Stencil
  , stencilWidth
  , stencilPositions
    -- * Building one
  , stencilFor
  , mooreStencil
  , vonNeumannStencil
    -- * Running one
  , stencilGrid
  , stencilFoldGrid
  , stencilAt
  ) where

import           Data.Grid.Sized.Coord
import           Data.Grid.Sized.Internal.Grid (GridOf, gridVector,
                                                unsafeGridFromVector)

import           Control.Monad                 (forM_)
import           Control.Monad.ST              (ST, runST)
import           Data.Kind                     (Type)
import qualified Data.Vector.Generic           as VG
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM

-- | The neighbourhood of every cell of a @cs@-shaped grid, as vector positions.
--
-- Indexed by the axis list and nothing else --- not by the vector type, not by
-- the element type --- because that is the whole claim: two grids of the same
-- shape have the same neighbour positions whatever they hold.
--
-- Abstract. The two accessors below are for inspecting one; there is no
-- constructor, because a table whose width and length disagree, or whose
-- positions are out of range, would make `stencilGrid` read out of bounds.
data Stencil (cs :: [Type]) = Stencil
    { stencilWidth     :: !Int
      -- ^ The largest number of neighbours any cell has, and so the stride of
      -- 'stencilPositions'. Rows shorter than this are padded with @-1@.
    , stencilPositions :: !(VU.Vector Int)
      -- ^ @'coordSpaceSize' \@cs * 'stencilWidth'@ entries, row-major by cell:
      -- the neighbours of position @i@ occupy
      -- @[i * w, (i + 1) * w)@, ending at the first @-1@ if there is one.
    }

-- | Precompute a neighbourhood given as a function of the coordinate.
--
-- The general constructor: 'mooreStencil' and 'vonNeumannStencil' are this
-- applied to the two neighbourhoods the library already defines, and anything
-- else a caller can write --- a knight's move, one axis only, an asymmetric
-- kernel --- goes through here without needing a new entry point.
--
-- The function is called once per cell, at build time, and its results are
-- taken as given: whatever coordinates it returns become the neighbours, in the
-- order it returned them. It is therefore the /only/ place a boundary policy is
-- consulted, which is what makes 'stencilGrid' agree with the loop it replaces
-- by construction rather than by a second implementation of the same rules.
--
-- The width is discovered rather than derived from the radius, because this
-- constructor has no radius: @neighbourhood@ is an arbitrary function, a
-- knight's move or an asymmetric kernel included, and nothing here knows a
-- bound for one of those. Where a bound /is/ available, as it is for the two
-- neighbourhoods below, deriving one is fine as an /allocation/ size:
-- 'Data.Grid.Sized.Coord.axisSteps' still drops the neighbours a bounded axis
-- refuses and deduplicates the ones a short periodic or reflecting axis
-- reaches twice, so the width that lands in the 'Stencil' is still the one
-- the fill actually observed --- @(2r+1)^d - 1@ only ever over-estimates the
-- buffer to fill it into. See 'stencilBounded', which 'mooreStencil' and
-- 'vonNeumannStencil' use for exactly that.
stencilFor ::
       forall cs. IsCoordList cs
    => (Coord cs -> [Coord cs])
    -> Stencil cs
stencilFor neighbourhood = Stencil w (VU.fromListN (n * w) (concatMap padRow (allCoord @cs)))
  where
    n = coordSpaceSize @cs
    -- Two passes over 'allCoord' rather than one over a list of lists held
    -- live across a width pass (sized-grid-adr.15): 'neighbourhood' runs
    -- twice per cell, but each row dies in the nursery as 'padRow' produces
    -- it instead of 2,500 rows being retained at once to find their maximum.
    -- Measured cheaper: see the module Haddock.
    w = maximum (0 : [length (neighbourhood c) | c <- allCoord @cs])
    padRow c = go w (map coordPosition (neighbourhood c))
      where
        go 0 _      = []
        go k []     = (-1) : go (k - 1) []
        go k (p:ps) = p : go (k - 1) ps
{-# INLINABLE stencilFor #-}

-- | The largest number of neighbours a Moore neighbourhood of radius @r@ can
-- name on @cs@'s axes: the cube around the cell, less the cell itself.
--
-- An over-estimate in general, for exactly the reasons 'stencilFor's Haddock
-- gives: a bounded axis drops the neighbours that fall off it, and an axis
-- shorter than @2r+1@ reaches the same cell twice and gets deduplicated.
-- Used only as an /allocation/ bound for 'stencilBounded' --- the width that
-- ends up in the 'Stencil' is still the one the fill observed, not this one.
--
-- It is also the bound 'vonNeumannStencil' allocates at, which is looser
-- there: von Neumann's true bound is the L1 ball, not the cube. That is a
-- deliberate choice, not an oversight --- see 'stencilBounded' --- because
-- the looseness costs peak residency during the build and nothing else: it
-- is measured, bounded by @n@ times this formula, and freed at the
-- compacting copy the same pass performs regardless.
mooreUpperBound :: forall cs. IsCoordList cs => Int -> Int
mooreUpperBound r = (2 * r + 1) ^ axisCount @cs - 1
{-# INLINE mooreUpperBound #-}

-- | The Moore neighbourhood at the given radius: `Data.Grid.Sized.Coord.mooreNeighbours`
-- precomputed. @mooreStencil 1@ is the game-of-life neighbourhood, diagonals
-- included.
--
-- Built through 'stencilBounded' rather than 'stencilFor': @mooreStencil@
-- knows both the radius and, from @cs@, the dimension, so it can derive
-- 'mooreUpperBound' and run the neighbourhood function once per cell instead
-- of twice. See 'stencilBounded' for the build itself and the module
-- Haddock for what that measures.
mooreStencil :: forall cs. IsCoordList cs => Int -> Stencil cs
mooreStencil r = stencilBounded (mooreUpperBound @cs r) (mooreNeighbours r)
{-# INLINABLE mooreStencil #-}

-- | The von Neumann neighbourhood at the given radius:
-- `Data.Grid.Sized.Coord.vonNeumannNeighbours` precomputed. @vonNeumannStencil 1@
-- is the orthogonally adjacent cells, however many dimensions there are.
--
-- Built through 'stencilBounded' at 'mooreUpperBound', the same allocation
-- bound `mooreStencil` uses --- see there for why it is loose here and
-- deliberately so.
vonNeumannStencil :: forall cs. IsCoordList cs => Int -> Stencil cs
vonNeumannStencil r = stencilBounded (mooreUpperBound @cs r) (vonNeumannNeighbours r)
{-# INLINABLE vonNeumannStencil #-}

-- | Lay a table out at a caller-supplied upper bound on the width, fill it in
-- one pass recording the width actually seen, and compact to that width if
-- the bound was loose.
--
-- Not the one-pass alternative @sized-grid-adr.15@ rejected: that variant
-- retained every row across a width pass to learn the maximum. This one
-- retains nothing --- each row is written directly into its slot of a table
-- already allocated at @ub@ --- so @neighbourhood@ runs once per cell instead
-- of the twice 'stencilFor' pays, at the cost of a compacting copy when the
-- bound turns out loose. Internal: 'mooreStencil' and 'vonNeumannStencil' are
-- the two entry points that know a radius and a dimension to derive a bound
-- from; 'stencilFor' has neither and keeps the two-pass path.
--
-- __Precondition:__ every row @neighbourhood@ produces is at most @ub@ long,
-- unchecked here --- a longer row would write into its neighbour's slot.
-- 'mooreUpperBound' is exact for 'mooreStencil' and an over-estimate for
-- 'vonNeumannStencil', never an under-estimate for either, so both call sites
-- satisfy it.
stencilBounded ::
       forall cs. IsCoordList cs
    => Int
    -> (Coord cs -> [Coord cs])
    -> Stencil cs
stencilBounded ub neighbourhood =
    runST $ do
        buf <- VUM.new (n * ub)
        w <- fillAll buf
        positions <-
            if w == ub
                then VU.unsafeFreeze buf
                else compact buf w
        pure (Stencil w positions)
  where
    n = coordSpaceSize @cs
    -- One pass over every cell, each row written straight into its slot and
    -- padded with the sentinel; the running maximum falls out of the fill
    -- rather than needing a pass of its own.
    fillAll :: forall s. VUM.MVector s Int -> ST s Int
    fillAll buf = go 0 0
      where
        go !i !seen
            | i >= n = pure seen
            | otherwise = do
                len <- fillRow buf (i * ub) (rowOf i)
                go (i + 1) (max seen len)
    rowOf i = map coordPosition (neighbourhood (unsafeCoordFromPosition i))
    fillRow :: forall s. VUM.MVector s Int -> Int -> [Int] -> ST s Int
    fillRow buf base = write base
      where
        end = base + ub
        write !j []     = pad j >> pure (j - base)
        write !j (p:ps) = VUM.unsafeWrite buf j p >> write (j + 1) ps
        pad !j
            | j >= end = pure ()
            | otherwise = VUM.unsafeWrite buf j (-1) >> pad (j + 1)
    -- Narrow a table laid out at stride @ub@ to stride @w@, once the fill
    -- above has found @w@ to be smaller: freed as soon as this returns, so
    -- the two tables are never both reachable at once.
    compact :: forall s. VUM.MVector s Int -> Int -> ST s (VU.Vector Int)
    compact buf w = do
        out <- VUM.new (n * w)
        forM_ [0 .. n - 1] $ \i ->
            VUM.unsafeCopy
                (VUM.unsafeSlice (i * w) w out)
                (VUM.unsafeSlice (i * ub) w buf)
        VU.unsafeFreeze out

-- | Rebuild a grid from each cell and its neighbours.
--
-- @stencilGrid s f g@ is @f here neigh@ at every position, where @neigh@ is
-- what the stencil says the neighbours are, read out of the /old/ grid. So it
-- is the bulk automaton step, and unlike `Control.Comonad.extend` it needs no
-- grid of grids and works on an unboxed grid.
--
-- Note what is missing from the signature: no @IsCoordList cs@. Every use of
-- the axis list happened when the `Stencil` was built, and what is left here is
-- a loop over two vectors. That is the point of the exercise stated as a type.
--
-- The neighbour list is produced lazily, so a rule that stops early --- a
-- @take@, a short-circuiting fold --- does not pay for the rest of it. See
-- 'gatherRow' for why reading it needs no bounds check.
stencilGrid ::
       forall v cs a b. (VG.Vector v a, VG.Vector v b)
    => Stencil cs
    -> (a -> [a] -> b)
    -> GridOf v cs a
    -> GridOf v cs b
stencilGrid (Stencil w tbl) f g =
    unsafeGridFromVector $
    VG.generate n $ \i -> f (VG.unsafeIndex v i) (gatherRow tbl w v i)
  where
    v = gridVector g
    -- The grid's own length rather than 'coordSpaceSize', which are the same
    -- number by the size invariant. Taking it from the vector is what lets this
    -- function go without the 'IsCoordList' constraint that would otherwise be
    -- needed only to recompute a length already in hand.
    n = VG.length v
-- @INLINE@, not @INLINABLE@: the latter only offers GHC an unfolding to
-- specialise, and where it declines the rule stays a lambda-bound variable
-- and every neighbour is boxed through it. See the module Haddock's
-- "Why both kernels are @INLINE@" for what that costs, measured
-- (@sized-grid-v6ye@). Do not weaken it without re-running @bench\/Main.hs@.
{-# INLINE stencilGrid #-}

-- | Rebuild a grid from each cell and its neighbours, folded in place rather
-- than gathered into a list first.
--
-- @stencilFoldGrid s step seed g@ is @foldl' step (seed here) neigh@ at every
-- position, for the same @neigh@ `stencilGrid` would hand its rule as a list
-- --- so @stencilFoldGrid s step seed@ agrees with
-- @stencilGrid s (\\here ns -> foldl' step (seed here) ns)@ everywhere,
-- `Test.Stencil` states that as a property.
--
-- Where `stencilGrid` builds a @[a]@ per cell --- one cons cell and one boxed
-- element per neighbour, thrown away as soon as a fold like a neighbour-sum
-- rule folds it right back down --- this reads each neighbour out of the
-- table and folds it into the accumulator directly, so that list is never
-- built. `gatherRow` is still the one place the row's span and sentinel are
-- decided; `foldRow'` walks the same span the same way, just strictly and
-- without consing.
--
-- Keep `stencilGrid` for rules that do not fold every neighbour every time
-- --- a @take@, a short-circuiting @any@ --- `gatherRow`'s laziness is what
-- lets those stop early, and a strict fold cannot; that is why
-- @gameOfLife@'s @runRule@ stays on `stencilGrid`.
--
-- Measured on the same @50 x 50@ neighbour-sum step as `stencilGrid`'s own
-- Haddock, in @bench\/Main.hs@:
--
-- * one pass, table already built: 133 μs and 2.1 MB for `stencilGrid`
--   against 42.5 μs and 215 KB here --- 3.1x faster and 10x less allocation.
-- * @iterate (stencilFoldGrid s (+) id) g !! 100@: 17.7 ms and 21 MB against
--   31.0 ms and 210 MB for the `stencilGrid` loop it replaces.
-- * the same x100 loop on an unboxed grid: 1.38 ms and 1.9 MB against 12.9 ms
--   and 221 MB. Boxed and unboxed allocate almost the same amount for
--   `stencilGrid` --- 210 MB against 221 MB --- which is the answer to the
--   question this issue was opened to settle: that allocation is almost
--   entirely the neighbour lists, not boxed-@Int@ thunk chains, because a
--   list element is boxed on the way out of an unboxed vector regardless.
--
-- The two fold rows are 11x apart on allocation --- 21 MB boxed against
-- 1.9 MB unboxed --- for the same 2,500 cells and the same hundred
-- generations, which is the `INLINE` result the module Haddock records seen
-- from the other end: unboxed, the specialised loop allocates the result
-- vectors and nothing else; boxed, it still writes a boxed @b@ per cell, and
-- no pragma can remove that.
stencilFoldGrid ::
       forall v cs a b. (VG.Vector v a, VG.Vector v b)
    => Stencil cs
    -> (b -> a -> b)
    -> (a -> b)
    -> GridOf v cs a
    -> GridOf v cs b
stencilFoldGrid (Stencil w tbl) step seed g =
    unsafeGridFromVector $
    VG.generate n $ \i -> foldRow' tbl w v i step (seed (VG.unsafeIndex v i))
  where
    v = gridVector g
    n = VG.length v
-- @INLINE@, not @INLINABLE@, and this is the kernel that shows why: on an
-- unboxed grid the pragma is worth 7.5x in time and 74x in allocation over a
-- hundred generations. Module Haddock, @sized-grid-v6ye@.
{-# INLINE stencilFoldGrid #-}

-- | The neighbours of a single cell, read out of the table rather than
-- enumerated.
--
-- @stencilAt s g c@ is @map (indexGrid g) (neighbourhood c)@ for the
-- neighbourhood @s@ was built from, and it exists because not every consumer
-- rewrites the whole grid. A single-site Metropolis step --- the shape
-- @ising-example@ runs --- picks one cell at random, reads its neighbourhood,
-- and puts back one element; running 'stencilGrid' for that would compute every
-- other cell to throw it away.
--
-- This used to need @IsCoordList cs@, to turn the coordinate into the row of
-- the table it names. sized-grid-adr.16 removed even that: a coordinate /is/
-- its row-major position, so `coordPosition` is a field read and the axis list
-- has no work left to do here at all. What the table saves is what it always
-- saved --- the per-neighbour coordinate arithmetic and the boundary policy
-- behind it.
stencilAt ::
       forall v cs a. VG.Vector v a
    => Stencil cs
    -> GridOf v cs a
    -> Coord cs
    -> [a]
stencilAt (Stencil w tbl) g c = gatherRow tbl w (gridVector g) (coordPosition c)
{-# INLINABLE stencilAt #-}

-- | The half-open span of 'stencilPositions' that holds row @i@ of a stencil
-- of width @w@: @[i * w, (i + 1) * w)@. The one piece of table-layout
-- arithmetic `gatherRow` and `foldRow'` would otherwise each restate.
rowSpan :: Int -> Int -> (Int, Int)
rowSpan w i = (i * w, (i + 1) * w)
{-# INLINE rowSpan #-}

-- | Row @i@ of the table, as the elements it names.
--
-- The sentinels are a suffix of each row, so meeting one ends the row and a
-- full row simply reaches the end of its span first. Lazy in the tail, so a
-- rule that stops early does not read the neighbours it never asked for.
--
-- 'VG.unsafeIndex' throughout, licensed by the same invariant
-- `Data.Grid.Sized.indexGrid` relies on: every entry of the table came from
-- `coordPosition` on a real @'Coord' cs@, so it is in
-- @[0, 'coordSpaceSize' \@cs)@, and the grid has exactly that many elements.
-- Both halves are facts about @cs@, which is why the stencil is indexed by it.
gatherRow :: VG.Vector v a => VU.Vector Int -> Int -> v a -> Int -> [a]
gatherRow tbl w v i = go start
  where
    (start, end) = rowSpan w i
    go !j
        | j >= end = []
        | otherwise =
            case VU.unsafeIndex tbl j of
                -1 -> []
                p  -> VG.unsafeIndex v p : go (j + 1)
{-# INLINE gatherRow #-}

-- | Row @i@ of the table, folded left instead of gathered into a list.
--
-- Same span, same sentinel, same licence for 'VG.unsafeIndex' as `gatherRow`
-- --- see there for why every entry is in range. Strict in the accumulator, so
-- `stencilFoldGrid` never builds a thunk chain across a row; the one it cannot
-- avoid is the outer 'VG.generate', which `stencilGrid` pays too.
foldRow' :: VG.Vector v a => VU.Vector Int -> Int -> v a -> Int -> (b -> a -> b) -> b -> b
foldRow' tbl w v i step = go start
  where
    (start, end) = rowSpan w i
    go !j !acc
        | j >= end = acc
        | otherwise =
            case VU.unsafeIndex tbl j of
                -1 -> acc
                p  -> go (j + 1) (step acc (VG.unsafeIndex v p))
{-# INLINE foldRow' #-}
