{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Execution-path benchmarks for @sized-grid-49hi@: what running a stencil
-- over a grid costs, sequentially and on several cores.
--
-- Only the running. Building the table is @sized-grid-kb38@, answered in
-- @spike\/kb38-parstencil@; every stencil here is a top-level CAF built once
-- and shared by every benchmark that names it, which is what a consumer
-- does and what makes the numbers below about the fill alone.
--
-- Two kernels, because the library has two and they are not the same
-- problem. 'stencilGrid' builds a @[a]@ per cell and hands it to the rule;
-- 'stencilFoldGrid' folds the row in place and never builds the list. The
-- first is allocation-bound and the second is not, and kb38 found that
-- allocation-bound work stops scaling at about four cores --- so the
-- interesting question is not whether the run parallelises but whether the
-- two kernels parallelise the same, and they do not.
--
-- Every variant is checked before anything is measured. 'checks' compares
-- each one against the library's own answer on both vector representations,
-- both boundary policies and an order-sensitive rule, and exits non-zero on
-- the first disagreement, so a variant that is fast because it scrambled a
-- row fails rather than reports.
module Main (main) where

import           Data.Grid.Sized
import           Data.Grid.Sized.Unboxed (UGrid)

import           Control.Concurrent      (getNumCapabilities)
import           Control.DeepSeq         (NFData, force)
import           Control.Exception       (evaluate)
import           Control.Monad           (forM_, unless, void)
import           Data.Kind               (Type)
import qualified Data.Vector.Generic     as VG
import           ParExec
import           System.Exit             (exitFailure)
import           Test.Tasty.Bench

-- * The shapes
--
-- Names and sizes taken from @bench\/Main.hs@ in the library, so a number
-- here can be read next to one there. 50x50 and 300x300 are the two the
-- issue asks for; the four smaller shapes are there to find the crossover,
-- which is the only way "when overhead dominates" gets an answer rather than
-- an assertion.

-- | 12 cells. Below any plausible threshold: here the fork is the whole cost.
type Tiny = '[Clamped 3, Clamped 4]

-- | 400 and 1,024 cells, either side of where kb38 put the build's crossover.
type Small20 = '[Clamped 20, Clamped 20]

type Small32 = '[Clamped 32, Clamped 32]

-- | 2,500 cells: the automaton step the library's stencil Haddock quotes.
type Step = '[Clamped 50, Clamped 50]

-- | 10,000 cells.
type Mid = '[Clamped 100, Clamped 100]

-- | 40,000 cells. Between 'Mid' and 'Big' because the fold kernel's
-- crossover turned out to be in there somewhere and the first pass could not
-- say where: it loses at 10,000 and wins at 90,000.
type Wide = '[Clamped 200, Clamped 200]

-- | 90,000 cells, a real consumer's working size.
type Big = '[Clamped 300, Clamped 300]

-- | 90,000 cells with no short rows: every cell has the full eight
-- neighbours, so 'gatherRow' never meets a sentinel and each cell is the
-- same amount of work. The clamped shapes have a cheaper boundary, which is
-- exactly the imbalance a static chunk split has to survive.
type BigPeriodic = '[Periodic 300, Periodic 300]

-- * The grids and the tables

boxed :: forall (cs :: [Type]). (IsCoordList cs, AllSizedKnown cs) => Grid cs Int
boxed = tabulateGrid coordPosition

unboxed ::
       forall (cs :: [Type]). (IsCoordList cs, AllSizedKnown cs)
    => UGrid cs Int
unboxed = tabulateGrid coordPosition

-- | One Moore table per shape, built once. Top level because that is where a
-- real consumer would put them, and because a table rebuilt inside a
-- benchmark would be measuring kb38's question instead of this one.
tinyS :: Stencil Tiny
tinyS = mooreStencil 1

small20S :: Stencil Small20
small20S = mooreStencil 1

small32S :: Stencil Small32
small32S = mooreStencil 1

stepS :: Stencil Step
stepS = mooreStencil 1

midS :: Stencil Mid
midS = mooreStencil 1

wideS :: Stencil Wide
wideS = mooreStencil 1

bigS :: Stencil Big
bigS = mooreStencil 1

bigPeriodicS :: Stencil BigPeriodic
bigPeriodicS = mooreStencil 1

-- * The rules
--
-- Two, and the second is not decoration. A neighbour sum is commutative and
-- associative, so it cannot tell a variant that reordered a row from one
-- that did not; the Horner rule can. Both are cheap, which is the case that
-- makes parallelism hardest to win: a rule expensive enough to dominate the
-- gather would make every variant here look good.

-- | The neighbour sum every automaton benchmark in the library runs.
sumRule :: Int -> [Int] -> Int
sumRule _ = sum

-- | Order-sensitive, for the checks: @a*3 + x@ left-folded over the row.
hornerRule :: Int -> [Int] -> Int
hornerRule _ = foldl (\a x -> a * 3 + x) 1

-- | 'hornerRule' as a fold step, so the fold kernel gets the same check.
hornerStep :: Int -> Int -> Int
hornerStep a x = a * 3 + x

-- * Checking

-- | Every variant against the library's own answer, for one grid.
--
-- Both kernels, both rules. The comparison is on the result vector rather
-- than the grid so that a mismatch prints as two lists of 'Int' rather than
-- needing an 'Eq' instance the library does not owe anyone.
verify ::
       forall v (cs :: [Type]). (VG.Vector v Int)
    => String
    -> Stencil cs
    -> GridOf v cs Int
    -> IO Bool
verify label s g = do
    let results =
            [ ("gridSeq/sum", out (gridSeq s sumRule g) == wantSum)
            , ("gridSeqStrict/sum", out (gridSeqStrict s sumRule g) == wantSum)
            , ("gridPar/sum", out (gridPar 2 s sumRule g) == wantSum)
            , ("gridParSpark/sum", out (gridParSpark 2 s sumRule g) == wantSum)
            , ("gridSeqInline/sum", out (gridSeqInline s sumRule g) == wantSum)
            , ("foldSeqInline/sum", out (foldSeqInline s (+) id g) == wantFoldSum)
            , ( "gridSeqInline/horner"
              , out (gridSeqInline s hornerRule g) == wantHorner)
            , ( "foldSeqInline/horner"
              , out (foldSeqInline s hornerStep (const 1) g) == wantFoldHorner)
            , ("gridSeq/horner", out (gridSeq s hornerRule g) == wantHorner)
            , ( "gridSeqStrict/horner"
              , out (gridSeqStrict s hornerRule g) == wantHorner)
            , ("gridPar/horner", out (gridPar 2 s hornerRule g) == wantHorner)
            , ( "gridParSpark/horner"
              , out (gridParSpark 2 s hornerRule g) == wantHorner)
            , ("foldSeq/sum", out (foldSeq s (+) id g) == wantFoldSum)
            , ("foldSeqStrict/sum", out (foldSeqStrict s (+) id g) == wantFoldSum)
            , ("foldPar/sum", out (foldPar 2 s (+) id g) == wantFoldSum)
            , ("foldParSpark/sum", out (foldParSpark 2 s (+) id g) == wantFoldSum)
            , ( "foldSeq/horner"
              , out (foldSeq s hornerStep (const 1) g) == wantFoldHorner)
            , ( "foldSeqStrict/horner"
              , out (foldSeqStrict s hornerStep (const 1) g) == wantFoldHorner)
            , ( "foldPar/horner"
              , out (foldPar 2 s hornerStep (const 1) g) == wantFoldHorner)
            , ( "foldParSpark/horner"
              , out (foldParSpark 2 s hornerStep (const 1) g) == wantFoldHorner)
              -- The chunk multiplier is a parameter, so check the ends of the
              -- range it is measured over: 1 chunk per capability, and 8.
            , ("gridPar mult=1", out (gridPar 1 s hornerRule g) == wantHorner)
            , ("gridPar mult=8", out (gridPar 8 s hornerRule g) == wantHorner)
            , ( "foldPar mult=1"
              , out (foldPar 1 s hornerStep (const 1) g) == wantFoldHorner)
            , ( "foldPar mult=8"
              , out (foldPar 8 s hornerStep (const 1) g) == wantFoldHorner)
              -- 'stencilFoldGrid' is documented to agree with 'stencilGrid'
              -- under a left fold; the parallel fold has to agree with it too,
              -- which is a different claim from agreeing with itself.
            , ( "foldPar vs stencilGrid"
              , out (foldPar 2 s hornerStep (const 1) g) == wantHorner)
            ]
    forM_ results $ \(name, ok) ->
        unless ok $ putStrLn ("MISMATCH: " ++ label ++ " / " ++ name)
    pure (all snd results)
  where
    out = VG.toList . gridVector
    wantSum = out (stencilGrid s sumRule g)
    wantHorner = out (stencilGrid s hornerRule g)
    wantFoldSum = out (stencilFoldGrid s (+) id g)
    wantFoldHorner = out (stencilFoldGrid s hornerStep (const 1) g)

-- | The checks, over both representations, both boundary policies, and sizes
-- from fewer cells than there are cores to more cells than there are chunks.
checks :: IO Bool
checks =
    fmap and . sequence $
    [ verify "Tiny boxed" tinyS (boxed @Tiny)
    , verify "Tiny unboxed" tinyS (unboxed @Tiny)
    , verify "Small20 boxed" small20S (boxed @Small20)
    , verify "Small32 unboxed" small32S (unboxed @Small32)
    , verify "Step boxed" stepS (boxed @Step)
    , verify "Step unboxed" stepS (unboxed @Step)
    , verify "Mid boxed" midS (boxed @Mid)
    , verify "Wide unboxed" wideS (unboxed @Wide)
    , verify "BigPeriodic unboxed" bigPeriodicS (unboxed @BigPeriodic)
    ]

-- * Measuring

-- | The full variant set for one grid, at the chunk multiplier the run was
-- given.
--
-- Five arms per kernel, because there turned out to be three axes and not
-- two. @Seq@ is the library restated; @SeqInline@ changes only its pragma;
-- @SeqStrict@ adds the strict fill; @Par@ adds the cores; @ParSpark@ trades
-- the 'unsafePerformIO' for a copy. Read left to right, each arm adds exactly
-- one thing to the one before it.
--
-- 'nf' rather than 'whnf' throughout, and that is the whole reason
-- @SeqStrict@ exists as a separate arm: on a boxed grid the library's
-- 'VG.generate' writes a thunk per cell, so under 'whnf' the sequential
-- variants would appear to do no work at all and the parallel ones --- which
-- force in the worker --- would appear to do all of it. 'nf' charges every
-- variant for the same finished grid.
runGroup ::
       forall v (cs :: [Type]). (VG.Vector v Int, NFData (GridOf v cs Int))
    => String
    -> Int
    -> Stencil cs
    -> GridOf v cs Int
    -> Benchmark
runGroup label mult s g =
    bgroup
        label
        [ bench "stencilGrid (library)" $ nf (stencilGrid s sumRule) g
        , bench "gridSeq" $ nf (gridSeq s sumRule) g
        , bench "gridSeqInline" $ nf (gridSeqInline s sumRule) g
        , bench "gridSeqStrict" $ nf (gridSeqStrict s sumRule) g
        , bench "gridPar" $ nf (gridPar mult s sumRule) g
        , bench "gridParSpark" $ nf (gridParSpark mult s sumRule) g
        , bench "stencilFoldGrid (library)" $ nf (stencilFoldGrid s (+) id) g
        , bench "foldSeq" $ nf (foldSeq s (+) id) g
        , bench "foldSeqInline" $ nf (foldSeqInline s (+) id) g
        , bench "foldSeqStrict" $ nf (foldSeqStrict s (+) id) g
        , bench "foldPar" $ nf (foldPar mult s (+) id) g
        , bench "foldParSpark" $ nf (foldParSpark mult s (+) id) g
        ]
-- Inlined at each call site, and that is load-bearing rather than tidy: this
-- function is polymorphic in the vector, so left out of line every variant it
-- names is called through a dictionary and none of them specialise. That is
-- not what a consumer's code looks like -- a consumer writes
-- @stencilFoldGrid s (+) id g@ at one concrete grid type -- and it costs the
-- fold kernel about 144 bytes per cell of boxed accumulator, which is enough
-- to move the answer.
{-# INLINE runGroup #-}

-- | The same fill at three chunk counts, to check kb38's finding that a
-- static split's granularity did not matter for the build.
--
-- It is a separate group rather than a third axis of 'runGroup' because it
-- only needs asking at one size: if 1 and 8 chunks per capability measure
-- the same on 90,000 cells, they measure the same on 2,500 for less reason.
chunkGroup ::
       forall v (cs :: [Type]). (VG.Vector v Int, NFData (GridOf v cs Int))
    => String
    -> Stencil cs
    -> GridOf v cs Int
    -> Benchmark
chunkGroup label s g =
    bgroup label $
    [bench ("gridPar mult=" ++ show m) (nf (gridPar m s sumRule) g) | m <- mults] ++
    [bench ("foldPar mult=" ++ show m) (nf (foldPar m s (+) id) g) | m <- mults]
  where
    mults = [1, 2, 8 :: Int]
{-# INLINE chunkGroup #-}

-- | The shape a cellular automaton actually runs: a generation built on the
-- last, hundreds of times, rather than the single pass 'runGroup' measures.
--
-- This is where the strictness axis pays or does not: on a boxed grid the
-- library's lazy fill lets a thunk chain build across generations, and a
-- variant that forces each cell as it writes it never builds one. Measured
-- against the same loop the library's own bench file runs.
iterGroup ::
       forall v (cs :: [Type]). (VG.Vector v Int, NFData (GridOf v cs Int))
    => String
    -> Int
    -> Int
    -> Stencil cs
    -> GridOf v cs Int
    -> Benchmark
iterGroup label mult reps s g =
    bgroup
        label
        [ bench "stencilGrid (library)" $ nf (iterN reps (stencilGrid s sumRule)) g
        , bench "gridSeqInline" $ nf (iterN reps (gridSeqInline s sumRule)) g
        , bench "gridSeqStrict" $ nf (iterN reps (gridSeqStrict s sumRule)) g
        , bench "gridPar" $ nf (iterN reps (gridPar mult s sumRule)) g
        , bench "stencilFoldGrid (library)" $
          nf (iterN reps (stencilFoldGrid s (+) id)) g
        , bench "foldSeqInline" $ nf (iterN reps (foldSeqInline s (+) id)) g
        , bench "foldSeqStrict" $ nf (iterN reps (foldSeqStrict s (+) id)) g
        , bench "foldPar" $ nf (iterN reps (foldPar mult s (+) id)) g
        ]
{-# INLINE iterGroup #-}

iterN :: Int -> (a -> a) -> a -> a
iterN n f = go n
  where
    go 0 x = x
    go k x = go (k - 1) (f x)

-- | Force every grid and every table before a single benchmark runs.
--
-- Without this the first arm of each group pays to materialise its input, and
-- the first arm of each group is the library control. A boxed grid built by
-- 'tabulateGrid' is a vector of thunks until something forces it, and the
-- first benchmark to 'nf' a result over it is what does. That is a real cost
-- charged to whichever benchmark happens to be listed first, which is not a
-- property of the variant --- it measured the library 1.5x slower than its own
-- transliteration on 400 boxed cells, at identical allocation, purely for
-- being at the top of the list.
warm :: IO ()
warm = do
    grid (boxed @Tiny)
    grid (unboxed @Tiny)
    grid (boxed @Small20)
    grid (boxed @Small32)
    grid (unboxed @Small32)
    grid (boxed @Step)
    grid (unboxed @Step)
    grid (boxed @Mid)
    grid (unboxed @Mid)
    grid (unboxed @Wide)
    grid (boxed @Big)
    grid (unboxed @Big)
    grid (unboxed @BigPeriodic)
    table tinyS
    table small20S
    table small32S
    table stepS
    table midS
    table wideS
    table bigS
    table bigPeriodicS
  where
    grid :: NFData a => a -> IO ()
    grid = void . evaluate . force
    -- 'Stencil' is abstract and has no 'NFData' instance; its two fields are
    -- strict and its vector is unboxed, so touching the width and the length
    -- is the whole table.
    table :: Stencil cs -> IO ()
    table s = void (evaluate (stencilWidth s + VG.length (stencilPositions s)))

main :: IO ()
main = do
    ok <- checks
    unless ok $ do
        putStrLn "variants disagree with the library; not measuring"
        exitFailure
    warm
    caps <- getNumCapabilities
    putStrLn ("capabilities: " ++ show caps)
    let mult = 2
    defaultMain
        [ runGroup "00,012 cells (Tiny), boxed" mult tinyS (boxed @Tiny)
        , runGroup "00,400 cells (Small20), boxed" mult small20S (boxed @Small20)
        , runGroup "01,024 cells (Small32), boxed" mult small32S (boxed @Small32)
        , runGroup "02,500 cells (Step), boxed" mult stepS (boxed @Step)
        , runGroup "02,500 cells (Step), unboxed" mult stepS (unboxed @Step)
        , runGroup "10,000 cells (Mid), boxed" mult midS (boxed @Mid)
        , runGroup "10,000 cells (Mid), unboxed" mult midS (unboxed @Mid)
        , runGroup "40,000 cells (Wide), unboxed" mult wideS (unboxed @Wide)
        , runGroup "90,000 cells (Big), boxed" mult bigS (boxed @Big)
        , runGroup "90,000 cells (Big), unboxed" mult bigS (unboxed @Big)
        , runGroup
              "90,000 cells (BigPeriodic), unboxed"
              mult
              bigPeriodicS
              (unboxed @BigPeriodic)
        , chunkGroup "chunks per capability, 90,000 unboxed" bigS (unboxed @Big)
        , iterGroup "x100, 02,500 cells (Step), boxed" mult 100 stepS (boxed @Step)
        , iterGroup
              "x100, 02,500 cells (Step), unboxed"
              mult
              100
              stepS
              (unboxed @Step)
        , iterGroup "x10, 90,000 cells (Big), unboxed" mult 10 bigS (unboxed @Big)
        ]
