-- | Baseline benchmarks for the operations the real workloads hit.
module Main (main) where

import           Data.Grid.Sized
import           Data.Grid.Sized.Unboxed (UGrid)

import           Control.Comonad
import           Control.Comonad.Store   (peek, pos)
import           Control.DeepSeq         (NFData (..))
import           Control.Exception       (evaluate)
import           Control.Lens            (ifoldl', imap, itraverse, toListOf, view)
import           Data.Aeson              (Result (..), fromJSON, toJSON)
import           Data.AffineSpace        ((.+^), (.-.))
import           Data.Functor.Identity   (Identity (..))
import           Data.Functor.Rep        (index, tabulate)
import qualified Data.Vector             as V
import qualified Data.Vector.Generic     as VG
import           Test.Tasty.Bench

-- | 90,000 cells, a real consumer's working size.
type Big = '[Clamped 300, Clamped 300]

-- | 10,000 cells, another real consumer's working size.
type Mid = '[Clamped 100, Clamped 100]

-- | Small enough that the comonadic step stays interactive: 2,500 cells.
type Step = '[Clamped 50, Clamped 50]

-- | Periodic, so walking never saturates the way Clamped's clamp would.
type Walk = '[Periodic 300, Periodic 300]

-- | 'Step', but 'Periodic': for measuring the boundary policy whose
-- 'offsetIsCoord' override is not the bounds-check default 'Clamped' folds
-- to a literal comparison.
type StepPeriodic = '[Periodic 50, Periodic 50]

bigGrid :: Grid Big Int
bigGrid = tabulate coordPosition

-- | 'bigGrid' in the other representation, for the boxed/unboxed pairs.
ubigGrid :: UGrid Big Int
ubigGrid = tabulateGrid coordPosition

midGrid :: Grid Mid Int
midGrid = tabulate coordPosition

stepGrid :: FocusedGrid Step Int
stepGrid = FocusedGrid (tabulate coordPosition) zeroCoord

-- | 'stepGrid' over 'StepPeriodic', so 'neighbourSum' walks Periodic's
-- 'offsetIsCoord' override instead of Clamped's bounds check.
stepGridPeriodic :: FocusedGrid StepPeriodic Int
stepGridPeriodic = FocusedGrid (tabulate coordPosition) zeroCoord

-- | The same two grids without the focus, for the stencil benchmarks. A
-- `Data.Grid.Sized.Stencil.Stencil` needs no focus, so measuring it through one
-- would charge it for machinery it does not use.
plainStepGrid :: Grid Step Int
plainStepGrid = tabulate coordPosition

plainStepGridPeriodic :: Grid StepPeriodic Int
plainStepGridPeriodic = tabulate coordPosition

-- | 'plainStepGrid' in the other representation, the way 'ubigGrid' is to
-- 'bigGrid': splits the iterated stencil step's allocation between the
-- neighbour lists 'stencilStep' builds and the boxed-'Int' thunk chains
-- 'iterate' builds on top of them.
uPlainStepGrid :: UGrid Step Int
uPlainStepGrid = tabulateGrid coordPosition

-- | The tables the iterated stencil benchmarks share. Top level because that is
-- where a real consumer would put them: built once for the grid type, reused
-- for every generation.
stepStencil :: Stencil Step
stepStencil = mooreStencil 1

stepStencilPeriodic :: Stencil StepPeriodic
stepStencilPeriodic = mooreStencil 1

-- | Sum: the actual computed quantity in the benchmarks below that reduce a
-- per-cell rule (a neighbourhood, a corner-read window, an offset sweep) to
-- a single 'Int'. Not a forcing helper -- 'nf' handles forcing now that
-- 'Grid'\/'FocusedGrid' have real 'NFData' instances, so every use left here
-- is one where the sum itself is part of what is being measured.
total :: (Foldable f) => f Int -> Int
total = sum

-- | A game-of-life shaped step: read the Moore neighbourhood and fold it.
--
-- Polymorphic in the axis list (rather than fixed at 'Step') so the same
-- function drives both 'stepGrid' and 'stepGridPeriodic', to exercise the
-- same neighbourhood fold over a different boundary policy.
neighbourSum :: (IsCoordList cs, AllSizedKnown cs) => FocusedGrid cs Int -> Int
neighbourSum fg = total [peek p fg | p <- neighbours (pos fg)]

-- | 'neighbourSum' iterated to depth @n@ through 'extend', the shape a
-- cellular automaton actually runs: hundreds of generations built on the
-- last, rather than the single 'extend' the benchmarks above measure --
-- checking whether 'FocusedGrid'\'s two lazy fields let a thunk chain build
-- up across generations even though 'Grid' underneath is a strict vector.
iterateExtend ::
       (IsCoordList cs, AllSizedKnown cs)
    => Int
    -> FocusedGrid cs Int
    -> FocusedGrid cs Int
iterateExtend n fg = iterate (extend neighbourSum) fg !! n

-- | The same neighbourhood fold as 'neighbourSum', written against a
-- precomputed `Data.Grid.Sized.Stencil.Stencil` rather than against `neighbours`.
--
-- A plain grid, not a 'FocusedGrid': `Data.Grid.Sized.Stencil.stencilGrid` is
-- already the bulk step, so there is no focus to carry and no @extend@ to build
-- one with. That is part of what is being measured --- the comonadic version
-- pays for a focus per cell that the rule never reads.
--
-- Polymorphic in the vector, like 'Data.Grid.Sized.Stencil.stencilGrid'
-- itself, so the same function drives both the boxed and unboxed iterated
-- benchmarks below.
stencilStep :: VG.Vector v Int => Stencil cs -> GridOf v cs Int -> GridOf v cs Int
stencilStep s = stencilGrid s (\_ ns -> total ns)

-- | The loop `Data.Grid.Sized.Stencil.stencilGrid` literally replaces, and what
-- @gameOfLife@\'s @applyRule@ was before this: the neighbourhood enumerated per
-- cell, on a plain `Grid`.
--
-- This, not @extend neighbourSum@, is the like-for-like comparison. The
-- comonadic version also pays for a `FocusedGrid` per cell that the rule never
-- reads, so measuring the stencil against it would credit the table with a
-- saving that is really the focus.
neighbourStep :: IsCoordList cs => Grid cs Int -> Grid cs Int
neighbourStep g = imapGrid (\c _ -> total (map (indexGrid g) (neighbours c))) g

-- | 'neighbourStep' iterated, the counterpart of 'iterateStencil'.
iterateNeighbourStep :: IsCoordList cs => Int -> Grid cs Int -> Grid cs Int
iterateNeighbourStep n g = iterate neighbourStep g !! n

-- | 'stencilStep' iterated, the counterpart of 'iterateExtend'.
--
-- The stencil is built once, outside the loop, which is the entire claim the
-- API makes: the neighbourhood is a fact about the type, so a hundred
-- generations should consult the axis list once rather than a hundred times.
iterateStencil :: VG.Vector v Int => Stencil cs -> Int -> GridOf v cs Int -> GridOf v cs Int
iterateStencil s n g = iterate (stencilStep s) g !! n

-- | 'stencilStep', against `Data.Grid.Sized.Stencil.stencilFoldGrid` instead of
-- `Data.Grid.Sized.Stencil.stencilGrid` (@sized-grid-adr.13@): the same
-- neighbour-sum rule, folded straight out of the table with no list in
-- between.
stencilFoldStep :: VG.Vector v Int => Stencil cs -> GridOf v cs Int -> GridOf v cs Int
stencilFoldStep s = stencilFoldGrid s (+) id

-- | 'stencilFoldStep' iterated, the counterpart of 'iterateStencil'.
iterateStencilFold :: VG.Vector v Int => Stencil cs -> Int -> GridOf v cs Int -> GridOf v cs Int
iterateStencilFold s n g = iterate (stencilFoldStep s) g !! n

-- | The same repeated step without the lazy list created by 'iterate'.
iterateStencilFoldLoop :: VG.Vector v Int => Stencil cs -> Int -> GridOf v cs Int -> GridOf v cs Int
iterateStencilFoldLoop stencil count = go count
  where
    go 0 grid = grid
    go remaining grid = go (remaining - 1) (stencilFoldStep stencil grid)

-- | Repeated coordinate offset. Recursive rather than a fold so the
-- intermediate 'Coord's cannot be fused away.
walk :: Int -> Coord Walk -> Int
walk 0 c = coordPosition c
walk k c = walk (k - 1) (c .+^ (1 :^ 1 :^ NoDelta))

-- | Four corner reads per position at a fixed window size: a summed-area-table
-- solve, 360,000 offsets over the 90,000 cells of 'Big'. Neither benchmark
-- above measures a constant displacement applied to every coordinate of a
-- 'Clamped' grid, which is what a read loop actually does.
cornerReads :: Grid Big Int -> Int
cornerReads g =
    total
        [ index g (c .+^ (0 :^ 0 :^ NoDelta)) +
          index g (c .+^ (3 :^ 3 :^ NoDelta)) -
          index g (c .+^ (0 :^ 3 :^ NoDelta)) -
          index g (c .+^ (3 :^ 0 :^ NoDelta))
        | c <- allCoord @Big
        ]

-- | The same four displacements as 'cornerReads', through the /checked/ offset
-- instead of @('.+^')@: 360,000 'offsetCoord' calls over the 90,000 cells of
-- 'Big'. @('.+^')@ and 'offsetCoord' are different functions over different
-- folds, so a change to one is invisible to a benchmark of the other.
--
-- Sums the position of each coordinate reached, so both branches of the
-- bounds check are forced /and/ the coordinate 'offsetCoord' constructs is
-- actually demanded. That last part is load-bearing as of sized-grid-adr.16:
-- this counted successes with @isJust@ until then, which was enough while a
-- coordinate was a spine of boxes -- the cold branch of 'unsafeOrdinal''s
-- guard sat in the middle of the fold and nothing could be dropped around
-- it. Once adr.16 made the decode unguarded, GHC could see that @isJust@
-- never demands the position and deleted the whole reconstruction, leaving
-- four in-range tests: 3.46 ms and 36.7 MB became 205 us and 24 bytes, and
-- the benchmark had quietly stopped measuring 'offsetCoord' at all.
-- 'coordPosition' is what a caller does with the result and is the cheapest
-- thing that demands it.
--
-- Consumed with 'total' rather than @length@, whose @foldr@-with-accumulator
-- form allocates a closure per element. The window size is an argument, not
-- a literal, so full laziness can't float the displacement list out as a
-- CAF.
--
-- This allocates ~35 MB against 'checkedCornerReadsFlat''s ~60 bytes for the
-- identical 360,000 'offsetCoord' calls -- see that function's note.
-- 'checkedCornerReadsFlat' is the fair reading of what 'offsetCoord' itself
-- costs; this one is kept because the second generator is closer to how a
-- caller iterating a runtime list of offsets would actually write the loop.
checkedCornerReads :: Int -> Int
checkedCornerReads k =
    total
        [ maybe 0 coordPosition (offsetCoord c d)
        | c <- allCoord @Big
        , d <- [ 0 :^ 0 :^ NoDelta
               , k :^ k :^ NoDelta
               , 0 :^ k :^ NoDelta
               , k :^ 0 :^ NoDelta
               ]
        ]

-- | sized-grid-2xv. The same 360,000 'offsetCoord' calls as
-- 'checkedCornerReads', on the same four displacements with @k@ still a
-- runtime argument (so full laziness still can't float anything out as a
-- CAF) -- but written as four flat arithmetic arms, the way 'cornerReads'
-- writes its four corners, instead of a second @d <-@ generator over a
-- list.
--
-- That is the only difference, and it is worth 35 MB: this allocates ~60
-- bytes for the same 360,000 calls 'checkedCornerReads' pays ~35 MB for.
-- @-ddump-simpl@ shows why. 'cornerReads' and this one each compile to a
-- single @joinrec@ -- GHC's zero-allocation loop form -- over 'allCoord',
-- with the four displacements' bounds checks inlined straight into it.
-- 'checkedCornerReads' compiles to a @letrec@-bound (not @join@) worker
-- that takes the next arm as an explicit @(Int -> Int)@ continuation, and
-- building that continuation is a real heap closure, allocated four times
-- per cell -- because the second generator is the fused @concatMap@ over a
-- 4-element runtime list, and GHC's list-fusion rewrite for that shape
-- comes out CPS'd rather than as a join point.
--
-- So the 35 MB this issue (sized-grid-2xv) went looking for was never in
-- 'offsetCoord', 'onBoundary' or 'coordDistance' -- all three fold straight
-- down to the same unrolled comparisons this flat form reaches, confirmed
-- by 'onBoundarySweepFlat' and 'axisDistanceSweepFlat' below reproducing
-- the same ~99% drop. It is what a *caller* pays for driving repeated
-- per-cell offset checks off a runtime list rather than a fixed number of
-- literal arms, which checkedCornerReads/onBoundarySweep/axisDistanceSweep
-- exist to measure honestly rather than hide.
checkedCornerReadsFlat :: Int -> Int
checkedCornerReadsFlat k =
    total
        [ maybe 0 coordPosition (offsetCoord c (0 :^ 0 :^ NoDelta)) +
          maybe 0 coordPosition (offsetCoord c (k :^ k :^ NoDelta)) +
          maybe 0 coordPosition (offsetCoord c (0 :^ k :^ NoDelta)) +
          maybe 0 coordPosition (offsetCoord c (k :^ 0 :^ NoDelta))
        | c <- allCoord @Big
        ]

-- | 'onBoundary' at the same four displacements as 'checkedCornerReads',
-- over 'Big'. Reaches 'axisBoundaryIsCoord''s default,
-- 'axisBoundaryByPosition', which nothing else here exercises. Allocates for
-- the same reason 'checkedCornerReads' does -- see 'checkedCornerReadsFlat'
-- and this function's own flat counterpart, 'onBoundarySweepFlat'.
onBoundarySweep :: Int -> Int
onBoundarySweep k =
    total
        [ if onBoundary (c .+^ d)
              then 1
              else 0
        | c <- allCoord @Big
        , d <- [ 0 :^ 0 :^ NoDelta
               , k :^ k :^ NoDelta
               , 0 :^ k :^ NoDelta
               , k :^ 0 :^ NoDelta
               ]
        ]

-- | sized-grid-2xv. 'onBoundarySweep' rewritten as flat arms, exactly as
-- 'checkedCornerReadsFlat' is to 'checkedCornerReads'. ~35 MB drops to ~60
-- bytes for the same 360,000 'onBoundary' calls, confirming the allocation
-- in 'onBoundarySweep' is the second generator's, not 'onBoundary''s or
-- 'axisBoundaryIsCoord''s.
onBoundarySweepFlat :: Int -> Int
onBoundarySweepFlat k =
    total
        [ (if onBoundary (c .+^ (0 :^ 0 :^ NoDelta)) then 1 else 0) +
          (if onBoundary (c .+^ (k :^ k :^ NoDelta)) then 1 else 0) +
          (if onBoundary (c .+^ (0 :^ k :^ NoDelta)) then 1 else 0) +
          (if onBoundary (c .+^ (k :^ 0 :^ NoDelta)) then 1 else 0)
        | c <- allCoord @Big
        ]

-- | 'coordDistance' at the same four displacements, over 'Big'. Reaches
-- 'axisDistance' and so 'axisDistanceIsCoord''s default. Allocates for the
-- same reason 'checkedCornerReads' does -- see 'checkedCornerReadsFlat' and
-- this function's own flat counterpart, 'axisDistanceSweepFlat'.
axisDistanceSweep :: Int -> Int
axisDistanceSweep k =
    total
        [ coordDistance c (c .+^ d)
        | c <- allCoord @Big
        , d <- [ 0 :^ 0 :^ NoDelta
               , k :^ k :^ NoDelta
               , 0 :^ k :^ NoDelta
               , k :^ 0 :^ NoDelta
               ]
        ]

-- | sized-grid-2xv. 'axisDistanceSweep' rewritten as flat arms, exactly as
-- 'checkedCornerReadsFlat' is to 'checkedCornerReads'. ~35 MB drops to
-- ~100 bytes for the same 360,000 'coordDistance' calls, confirming the
-- allocation in 'axisDistanceSweep' is the second generator's, not
-- 'coordDistance''s or 'axisDistanceIsCoord''s.
axisDistanceSweepFlat :: Int -> Int
axisDistanceSweepFlat k =
    total
        [ coordDistance c (c .+^ (0 :^ 0 :^ NoDelta)) +
          coordDistance c (c .+^ (k :^ k :^ NoDelta)) +
          coordDistance c (c .+^ (0 :^ k :^ NoDelta)) +
          coordDistance c (c .+^ (k :^ 0 :^ NoDelta))
        | c <- allCoord @Big
        ]

-- | The same 360,000 offsets as 'cornerReads', on a bare axis instead of a
-- 'Coord', read against it to separate the per-axis arithmetic cost from
-- the cost of the fold over the axis list. Keep both: improving the axis
-- arithmetic shows up here, improving the fold shows up in 'cornerReads'.
axisOffsets :: Int -> Int
axisOffsets k =
    total
        [ ordinalToInt (view asOrdinal (c .+^ d))
        | c <- allCoordLike @300 @Clamped
        , d <- [1 .. k]
        ]

--------------------------------------------------------------------------------
-- Boxed against unboxed.
--
-- Each pair below differs only in the vector type: 'GridOf' takes its vector
-- as a parameter, so 'tabulateGrid', 'mapGrid', 'foldlGrid'', 'scanl1Grid',
-- 'mapLowerDim', 'transposeGrid' and 'scanAxis' below are the same code at
-- @v ~ V.Vector@ and @v ~ U.Vector@.
--
-- Keep the 'indexGrid' pair even though it reports no difference -- it is
-- the row that says where the win is /not/, and "Data.Grid.Sized.Unboxed"
-- tells callers not to reach for unboxing to speed up indexed reads. Delete
-- the benchmark and that claim stops being checked.
--------------------------------------------------------------------------------

-- | Prefix-sum each row independently: @scanl1Grid@ lifted through the
-- outermost axis, which is how the consumer builds a summed-area table.
rowPrefix :: VG.Vector v Int => GridOf v Big Int -> GridOf v Big Int
rowPrefix = runIdentity . mapLowerDim (Identity . scanl1Grid (+))

-- | Cell values for the summed-area benchmark below, matching a real
-- consumer's table-building workload.
power :: Int -> Coord Big -> Int
power serial ((fromEnum -> y) :| (fromEnum -> x) :| _) =
    (rack * y + serial) * rack `div` 100 `mod` 10 - 5
  where
    rack = x + 10


-- | The hand-rolled equivalent of @'scanAxis' 0@: rotate the grid so the
-- axis is outermost, prefix each row, rotate back. This is what a caller
-- writes when the library does not reach the axis for them, and what
-- 'scanAxis' has to beat rather than merely match (sized-grid-adr.5).
rotatedPrefix :: VG.Vector v Int => GridOf v Big Int -> GridOf v Big Int
rotatedPrefix = transposeGrid . rowPrefix . transposeGrid

-- | Summed-area table: prefix along the rows, transpose, prefix again,
-- transpose back. Four whole-grid passes, which is the shape unboxing helps.
satBuild :: VG.Vector v Int => Int -> GridOf v Big Int
satBuild serial =
    transposeGrid . rowPrefix . transposeGrid . rowPrefix . tabulateGrid $
    power serial

-- | The same table, named by axis instead of built from the transpose
-- trick. Kept alongside 'satBuild' so the two are measured against each
-- other rather than assumed equal: @scanAxis 0@ genuinely transposes and
-- measured ~25% slower boxed than the hand-fused pipeline, trading that for
-- a function that reaches any axis of a grid of any dimension.
satBuildAxis :: VG.Vector v Int => Int -> GridOf v Big Int
satBuildAxis serial =
    scanAxis 0 (+) . scanAxis 1 (+) . tabulateGrid $ power serial

-- There is no standalone @allCoord@ benchmark: at a fixed type it is a CAF,
-- so the obvious benchmark just measures a pointer, and @-fno-full-laziness@
-- would be needed to defeat that -- which would make every other benchmark
-- here stop resembling how the library is actually compiled. The cost is
-- measured where it is real instead, in the instances that receive
-- @IsCoordList@ at runtime.
--
-- The "indexed vs unindexed" group below is named for what it measures, not
-- allCoord overhead: 'coordPosition' costs far more per call than producing
-- the coordinate itself does, so the gap between a pair like @tabulate@\/@pure@
-- is mostly 'coordPosition''s cost, not the coordinate list's.

main :: IO ()
main = do
    -- Force the shared inputs once, so their construction is not charged to
    -- whichever benchmark happens to run first.
    _ <- evaluate (rnf bigGrid)
    _ <- evaluate (rnf midGrid)
    _ <- evaluate (rnf stepGrid)
    _ <- evaluate (rnf stepGridPeriodic)
    _ <- evaluate (rnf uPlainStepGrid)
    defaultMain
        [ bgroup
              "coord arithmetic"
              [ bench "(.+^) x10000, Periodic 300x300" $
                whnf (`walk` zeroCoord) 10000
              , -- sized-grid-hb4. 'nf f x' passes x as an argument, but that alone
                -- does not stop GHC floating 'f x' out of the benchmark loop when
                -- x is itself a statically-known value (a literal, or a top-level
                -- CAF like 'bigGrid' below): full laziness only needs x to be
                -- loop-invariant, not written as a literal. It floated here for one
                -- library build and not another, changing nothing in this file --
                -- see the note on tasty-bench's own 'funcToBench'. 'env' pushes x
                -- through 'withResource'/'unsafePerformIO', which GHC does not see
                -- through, so the benchmark keeps measuring real work regardless of
                -- how much the library's cross-module inlining shifts underneath it.
                env (pure zeroCoord) $ \o ->
                bench "(.-.) x10000, Clamped 100x100 (coord list shared)" $
                nf
                    (\o' -> [view deltaHead (c .-. o') | c <- allCoord @Mid])
                    o
              , bench "(.+^) x360000, four corner reads over Clamped 300x300" $
                whnf cornerReads bigGrid
                -- sized-grid-adr.16: these six take their @k@ through 'env'
                -- for the same reason '(.-.) x10000' above does, and they did
                -- not need to before. While a coordinate was a spine, the cold
                -- branch of 'unsafeOrdinal''s guard sat in the middle of every
                -- one of these loops and was on its own enough to stop GHC
                -- folding them. adr.16 removed that guard from the /decode/
                -- path (see 'unsafeOrdinalUnchecked'), and with nothing left
                -- to block it GHC evaluated 'checkedCornerReads 3' at compile
                -- time: 3.46 ms and 36.7 MB became 205 us and __24 bytes__,
                -- which is not a 17x speedup but a benchmark that had stopped
                -- doing the work. 'env' pushes @k@ through
                -- 'withResource'/'unsafePerformIO', which GHC does not see
                -- through, so what is measured is 360,000 real calls again.
              , env (pure 3) $ \k ->
                bench "offsetCoord x360000, checked, over Clamped 300x300" $
                whnf checkedCornerReads k
              , env (pure 3) $ \k ->
                bench "offsetCoord x360000, checked, flat arms (sized-grid-2xv)" $
                whnf checkedCornerReadsFlat k
              , env (pure 3) $ \k ->
                bench "onBoundary x360000, over Clamped 300x300" $
                whnf onBoundarySweep k
              , env (pure 3) $ \k ->
                bench "onBoundary x360000, flat arms (sized-grid-2xv)" $
                whnf onBoundarySweepFlat k
              , env (pure 3) $ \k ->
                bench "coordDistance x360000, over Clamped 300x300" $
                whnf axisDistanceSweep k
              , env (pure 3) $ \k ->
                bench "coordDistance x360000, flat arms (sized-grid-2xv)" $
                whnf axisDistanceSweepFlat k
              , bench "(.+^) x360000, one Clamped 300 axis (no Coord)" $
                whnf axisOffsets 1200
              , -- sized-grid-hb4: same floating hazard as '(.-.) x10000' above.
                env (pure 299) $ \n ->
                bench "toEnum/fromEnum x300, Clamped 300" $
                nf
                    (\n' -> [fromEnum (toEnum i :: Clamped 300) | i <- [0 .. n']])
                    n
              ]
        , bgroup
              "indexed vs unindexed (the gap is coordPosition, not allCoord)"
              [ bench "tabulate 300x300  [coordPosition per cell]" $
                nf (\f -> tabulate f :: Grid Big Int) coordPosition
                -- sized-grid-adr.16: the worst case for a coordinate that is
                -- a position rather than a spine, and the one adr.8 called
                -- out. Every other tabulate here passes its coordinate
                -- straight to 'coordPosition', which is now free; this one
                -- takes the coordinate apart per cell, so both @(':|')@
                -- matches are a 'quotRem' where they used to be field reads.
                -- adr.8 predicted it still comes out ahead, because producing
                -- a coordinate costs more than decoding one, and the suite had
                -- no benchmark that would have caught it if that were wrong.
              , bench "tabulate 300x300  [rule destructures the coord]" $
                nf (\s -> tabulate (power s) :: Grid Big Int) 42
              , bench "pure 300x300      [no coord at all]" $
                nf (\x -> pure x :: Grid Big Int) 1
              , bench "imap 300x300      [coordPosition per cell]" $
                nf (imap (\c x -> coordPosition c + x)) bigGrid
              , -- sized-grid-hb4: same floating hazard as '(.-.) x10000' above.
                env (pure bigGrid) $ \g ->
                bench "fmap 300x300      [no coord at all]" $
                nf (fmap (+ 1)) g
              ]
        , bgroup
              "grid access"
              [ -- sized-grid-hb4: same floating hazard as '(.-.) x10000' above.
                env (pure bigGrid) $ \g ->
                bench "index x90000, 300x300 (coord list shared)" $
                nf (\g' -> map (index g') (allCoord @Big)) g
              ]
        , bgroup
              "indexed traversals"
              [ bench "ifoldl' 300x300" $
                whnf (ifoldl' (\c acc x -> acc + coordPosition c + x) 0) bigGrid
              , bench "itraverse 100x100" $
                nf (itraverse (const Just)) midGrid
              ]
        , bgroup
              "comonad"
              [ bench "extract 50x50" $ whnf extract stepGrid
              , bench "extend neighbourSum 50x50" $
                nf (extend neighbourSum) stepGrid
              , bench "extend neighbourSum 50x50, Periodic" $
                nf (extend neighbourSum) stepGridPeriodic
              , bench "iterate (extend neighbourSum) x100, 50x50" $
                nf (iterateExtend 100) stepGrid
              , bench "iterate (extend neighbourSum) x100, 50x50, Periodic" $
                nf (iterateExtend 100) stepGridPeriodic
              ]
        , -- The same workload as the four above, through a precomputed
          -- neighbourhood, split into the three costs that make up the trade.
          --
          -- Building the table is the case the stencil /loses/: it is one full
          -- pass of the neighbourhood computation it replaces, and rather more
          -- than one 'extend' because it also lays out a vector. Running an
          -- already-built table is the case it wins, and by how much. A caller
          -- taking @n@ passes pays the first once and the second @n@ times, so
          -- the two together say where the crossover is.
          --
          -- The radius is the 'whnf' argument in the two build benchmarks and
          -- not a literal in the expression, because as a literal GHC floats
          -- @mooreStencil 1@ out of the benchmarked function --- it depends on
          -- nothing else --- and what gets measured is the step alone under a
          -- label claiming otherwise. That happened, and these numbers are the
          -- ones after it was caught.
          bgroup
              "stencil (the same neighbourhood, precomputed)"
              [ bench "imapGrid over neighbours 50x50 (what it replaces)" $
                nf neighbourStep plainStepGrid
              , bench "imapGrid over neighbours 50x50, Periodic" $
                nf neighbourStep plainStepGridPeriodic
              , bench "imapGrid over neighbours x100, 50x50" $
                nf (iterateNeighbourStep 100) plainStepGrid
              , bench "mooreStencil r, 50x50 (building the table)" $
                whnf (VG.length . stencilPositions . mooreStencil @Step) 1
              , bench "mooreStencil r, 50x50, Periodic" $
                whnf (VG.length . stencilPositions . mooreStencil @StepPeriodic) 1
              , bench "stencilStep 50x50, table already built" $
                nf (stencilStep stepStencil) plainStepGrid
              , bench "stencilStep 50x50, Periodic, table already built" $
                nf (stencilStep stepStencilPeriodic) plainStepGridPeriodic
              , bench "stencilStep 50x50, table built for this one pass" $
                nf (\r -> stencilStep (mooreStencil r) plainStepGrid) 1
              , bench "stencilStep x100, 50x50" $
                nf (iterateStencil stepStencil 100) plainStepGrid
              , bench "stencilStep x100, 50x50, Periodic" $
                nf (iterateStencil stepStencilPeriodic 100) plainStepGridPeriodic
              , -- sized-grid-adr.13: splits the boxed figure above between the
                -- neighbour lists 'stencilGrid' builds per pass and the boxed
                -- 'Int' thunk chains 'iterate' builds across passes, deciding
                -- whether a fold-shaped 'stencilFoldGrid' is worth writing.
                bench "stencilStep x100, 50x50, unboxed" $
                nf (iterateStencil stepStencil 100) uPlainStepGrid
              , -- The fold-shaped counterparts of the four 'stencilStep'
                -- benchmarks just above, same grids and same rule, against
                -- 'stencilFoldGrid' instead of 'stencilGrid' (sized-grid-adr.13).
                bench "stencilFoldStep 50x50, table already built" $
                nf (stencilFoldStep stepStencil) plainStepGrid
              , bench "stencilFoldStep 50x50, Periodic, table already built" $
                nf (stencilFoldStep stepStencilPeriodic) plainStepGridPeriodic
              , bench "stencilFoldStep x100, 50x50" $
                nf (iterateStencilFold stepStencil 100) plainStepGrid
              , bench "stencilFoldStep x100, 50x50, Periodic" $
                nf (iterateStencilFold stepStencilPeriodic 100) plainStepGridPeriodic
              , bench "stencilFoldStep x100, 50x50, unboxed" $
                nf (iterateStencilFold stepStencil 100) uPlainStepGrid
              , bench "stencilFoldStep x100, 50x50, unboxed, recursive" $
                nf (iterateStencilFoldLoop stepStencil 100) uPlainStepGrid
              ]
        , bgroup
              "collapse round trip"
              [ bench "collapseGrid 100x100" $ nf collapseGrid midGrid
              , bench "gridFromList . collapseGrid 100x100" $
                nf
                    (\g -> gridFromList (collapseGrid g) :: Maybe (Grid Mid Int))
                    midGrid
              ]
        , bgroup
              "boxed vs unboxed (same code, different vector)"
              [ bench "tabulateGrid 300x300      boxed" $
                nf (\f -> tabulateGrid f :: Grid Big Int) coordPosition
              , bench "tabulateGrid 300x300    unboxed" $
                nf (\f -> tabulateGrid f :: UGrid Big Int) coordPosition
              , -- sized-grid-hb4: same floating hazard as '(.-.) x10000' above.
                -- This pair is the clearest evidence for it: the unboxed
                -- benchmark's time was identical whether floated or not (an
                -- already-realised unboxed vector is nearly free to re-force),
                -- so only its allocation column exposed the sharing.
                env (pure bigGrid) $ \g ->
                bench "mapGrid 300x300              boxed" $
                nf (mapGrid (+ 1)) g
              , env (pure ubigGrid) $ \g ->
                bench "mapGrid 300x300            unboxed" $
                nf (mapGrid (+ 1)) g
              , env (pure bigGrid) $ \g ->
                bench "imapGrid 300x300            boxed" $
                nf (imapGrid (\_ x -> x + 1)) g
              , env (pure ubigGrid) $ \g ->
                bench "imapGrid 300x300          unboxed" $
                nf (imapGrid (\_ x -> x + 1)) g
              , bench "foldlGrid' 300x300         boxed" $
                whnf (foldlGrid' (+) 0) bigGrid
              , bench "foldlGrid' 300x300       unboxed" $
                whnf (foldlGrid' (+) 0) ubigGrid
              , bench "transposeGrid 300x300      boxed" $ nf transposeGrid bigGrid
              , bench "transposeGrid 300x300    unboxed" $ nf transposeGrid ubigGrid
              , bench "summed-area build 300x300  boxed" $
                nf (\s -> satBuild s :: Grid Big Int) 18
              , bench "summed-area build 300x300 unboxed" $
                nf (\s -> satBuild s :: UGrid Big Int) 18
              , bench "summed-area build (scanAxis) 300x300  boxed" $
                nf (\s -> satBuildAxis s :: Grid Big Int) 18
              , bench "summed-area build (scanAxis) 300x300 unboxed" $
                nf (\s -> satBuildAxis s :: UGrid Big Int) 18
                -- sized-grid-adr.5. The summed-area pair above measures
                -- 'scanAxis' with 'tabulateGrid' in front of it, which is
                -- most of the time; these four measure the axis walk on a
                -- grid that already exists. Axis 1 is the innermost, whose
                -- fibres are contiguous; axis 0 is the strided one, and the
                -- 'rotatedPrefix' rows below are what a caller writes
                -- instead when the combinator is not worth reaching for.
              , env (pure bigGrid) $ \g ->
                bench "scanAxis 0 300x300           boxed" $ nf (scanAxis 0 (+)) g
              , env (pure ubigGrid) $ \g ->
                bench "scanAxis 0 300x300         unboxed" $ nf (scanAxis 0 (+)) g
              , env (pure bigGrid) $ \g ->
                bench "transpose-prefix-transpose   boxed" $ nf rotatedPrefix g
              , env (pure ubigGrid) $ \g ->
                bench "transpose-prefix-transpose unboxed" $ nf rotatedPrefix g
              , env (pure bigGrid) $ \g ->
                bench "scanAxis 1 300x300           boxed" $ nf (scanAxis 1 (+)) g
              , env (pure ubigGrid) $ \g ->
                bench "scanAxis 1 300x300         unboxed" $ nf (scanAxis 1 (+)) g
              , env (pure bigGrid) $ \g ->
                bench "mapAxis 0 300x300            boxed" $
                nf (mapAxis 0 (mapGrid (+ 1))) g
              , env (pure ubigGrid) $ \g ->
                bench "mapAxis 0 300x300          unboxed" $
                nf (mapAxis 0 (mapGrid (+ 1))) g
              , env (pure bigGrid) $ \g ->
                bench "axisFold 0 300x300         boxed" $
                nf (sum . map (foldlGrid' (+) 0) . toListOf (axisFold 0)) g
              , env (pure ubigGrid) $ \g ->
                bench "axisFold 0 300x300       unboxed" $
                nf (sum . map (foldlGrid' (+) 0) . toListOf (axisFold 0)) g
                -- The pair that reports no difference, deliberately kept.
              , bench "indexGrid x90000           boxed" $
                whnf (\g -> sum (map (indexGrid g) (allCoord @Big))) bigGrid
              , bench "indexGrid x90000         unboxed" $
                whnf (\g -> sum (map (indexGrid g) (allCoord @Big))) ubigGrid
              ]
        , bgroup
              "json"
              [ bench "toJSON 100x100" $ nf toJSON midGrid
              , bench "fromJSON . toJSON 100x100" $
                whnf (roundTrips . toJSON) midGrid
              , env (pure (toJSON midGrid)) $ \v ->
                bench "fromJSON 100x100, pre-serialised" $ whnf roundTrips v
              , env (pure (toJSON midGrid)) $ \v ->
                bench "TEMP aeson floor [[Int]]" $ whnf floorList v
              , env (pure (toJSON midGrid)) $ \v ->
                bench "TEMP aeson floor Vector" $ whnf floorVec v
              ]
        ]
  where
    -- 'Result' has no 'NFData' instance, so this stays a manual force rather
    -- than 'nf'; 'rnf' on the decoded 'Grid' is still the direct deep force,
    -- not a 'total'-style Foldable sum.
    roundTrips v =
        case fromJSON v :: Result (Grid Mid Int) of
            Success g -> rnf g `seq` (1 :: Int)
            Error _   -> -1
    floorList v =
        case fromJSON v :: Result [[Int]] of
            Success g -> rnf g `seq` (1 :: Int)
            Error _   -> -1
    floorVec v =
        case fromJSON v :: Result (V.Vector (V.Vector Int)) of
            Success g -> rnf g `seq` (1 :: Int)
            Error _   -> -1
