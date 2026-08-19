{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}


-- | Baseline benchmarks for the operations the real workloads hit.
module Main (main) where

import           Data.Grid.Sized
import           Data.Grid.Sized.Unboxed (UGrid)

import           Control.Comonad
import           Control.Comonad.Store   (peek, pos)
import           Control.DeepSeq         (NFData (..))
import           Control.Exception       (evaluate)
import           Control.Lens            (ifoldl', imap, itraverse, view)
import           Data.Aeson              (Result (..), fromJSON, toJSON)
import           Data.AffineSpace        ((.+^), (.-.))
import           Data.Functor.Identity   (Identity (..))
import           Data.Functor.Rep        (index, tabulate)
import           Data.Maybe              (isJust)
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

-- | Repeated coordinate offset. Recursive rather than a fold so the
-- intermediate 'Coord's cannot be fused away.
walk :: Int -> Coord Walk -> Int
walk 0 c = coordPosition c
walk k c = walk (k - 1) (c .+^ (1 :| 1 :| EmptyCoord))

-- | Four corner reads per position at a fixed window size: a summed-area-table
-- solve, 360,000 offsets over the 90,000 cells of 'Big'. Neither benchmark
-- above measures a constant displacement applied to every coordinate of a
-- 'Clamped' grid, which is what a read loop actually does.
cornerReads :: Grid Big Int -> Int
cornerReads g =
    total
        [ index g (c .+^ (0 :| 0 :| EmptyCoord)) +
          index g (c .+^ (3 :| 3 :| EmptyCoord)) -
          index g (c .+^ (0 :| 3 :| EmptyCoord)) -
          index g (c .+^ (3 :| 0 :| EmptyCoord))
        | c <- allCoord @Big
        ]

-- | The same four displacements as 'cornerReads', through the /checked/ offset
-- instead of @('.+^')@: 360,000 'offsetCoord' calls over the 90,000 cells of
-- 'Big'. @('.+^')@ and 'offsetCoord' are different functions over different
-- folds, so a change to one is invisible to a benchmark of the other.
--
-- Counts successes rather than summing positions, so both branches of the
-- bounds check are forced and measured, and consumed with 'total' rather
-- than @length@, whose @foldr@-with-accumulator form allocates a closure
-- per element. The window size is an argument, not a literal, so full
-- laziness can't float the displacement list out as a CAF.
checkedCornerReads :: Int -> Int
checkedCornerReads k =
    total
        [ if isJust (offsetCoord c d)
              then 1
              else 0
        | c <- allCoord @Big
        , d <- [ 0 :| 0 :| EmptyCoord
               , k :| k :| EmptyCoord
               , 0 :| k :| EmptyCoord
               , k :| 0 :| EmptyCoord
               ]
        ]

-- | 'onBoundary' at the same four displacements as 'checkedCornerReads',
-- over 'Big'. Reaches 'axisBoundaryIsCoord''s default,
-- 'axisBoundaryByPosition', which nothing else here exercises.
onBoundarySweep :: Int -> Int
onBoundarySweep k =
    total
        [ if onBoundary (c .+^ d)
              then 1
              else 0
        | c <- allCoord @Big
        , d <- [ 0 :| 0 :| EmptyCoord
               , k :| k :| EmptyCoord
               , 0 :| k :| EmptyCoord
               , k :| 0 :| EmptyCoord
               ]
        ]

-- | 'coordDistance' at the same four displacements, over 'Big'. Reaches
-- 'axisDistance' and so 'axisDistanceIsCoord''s default.
axisDistanceSweep :: Int -> Int
axisDistanceSweep k =
    total
        [ coordDistance c (c .+^ d)
        | c <- allCoord @Big
        , d <- [ 0 :| 0 :| EmptyCoord
               , k :| k :| EmptyCoord
               , 0 :| k :| EmptyCoord
               , k :| 0 :| EmptyCoord
               ]
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
    ((rack * y + serial) * rack `div` 100) `mod` 10 - 5
  where
    rack = x + 10

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
              , bench "(.-.) x10000, Clamped 100x100 (coord list shared)" $
                nf
                    (\o -> [view coordHead (c .-. o) | c <- allCoord @Mid])
                    zeroCoord
              , bench "(.+^) x360000, four corner reads over Clamped 300x300" $
                whnf cornerReads bigGrid
              , bench "offsetCoord x360000, checked, over Clamped 300x300" $
                whnf checkedCornerReads 3
              , bench "onBoundary x360000, over Clamped 300x300" $
                whnf onBoundarySweep 3
              , bench "coordDistance x360000, over Clamped 300x300" $
                whnf axisDistanceSweep 3
              , bench "(.+^) x360000, one Clamped 300 axis (no Coord)" $
                whnf axisOffsets 1200
              , bench "toEnum/fromEnum x300, Clamped 300" $
                nf
                    (\n -> [fromEnum (toEnum i :: Clamped 300) | i <- [0 .. n]])
                    299
              ]
        , bgroup
              "indexed vs unindexed (the gap is coordPosition, not allCoord)"
              [ bench "tabulate 300x300  [coordPosition per cell]" $
                nf (\f -> tabulate f :: Grid Big Int) coordPosition
              , bench "pure 300x300      [no coord at all]" $
                nf (\x -> pure x :: Grid Big Int) 1
              , bench "imap 300x300      [coordPosition per cell]" $
                nf (imap (\c x -> coordPosition c + x)) bigGrid
              , bench "fmap 300x300      [no coord at all]" $
                nf (fmap (+ 1)) bigGrid
              ]
        , bgroup
              "grid access"
              [ bench "index x90000, 300x300 (coord list shared)" $
                nf (\g -> map (index g) (allCoord @Big)) bigGrid
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
                whnf (\r -> VG.length (stencilPositions (mooreStencil @Step r))) 1
              , bench "mooreStencil r, 50x50, Periodic" $
                whnf (\r -> VG.length (stencilPositions (mooreStencil @StepPeriodic r))) 1
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
              , bench "mapGrid then sum 300x300   boxed" $
                nf (mapGrid (+ 1)) bigGrid
              , bench "mapGrid then sum 300x300 unboxed" $
                nf (mapGrid (+ 1)) ubigGrid
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
