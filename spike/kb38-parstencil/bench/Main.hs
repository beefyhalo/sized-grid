{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Build-path benchmarks for @sized-grid-kb38@: what a stencil table costs
-- to lay out, sequentially and on several cores.
--
-- Only the build. Running a table is @sized-grid-49hi@ and nothing here
-- touches it; every benchmark below forces a freshly built 'Table' (or the
-- library's own 'Stencil') to WHNF, which for a strict pair of an @Int@ and
-- an unboxed vector is the whole table.
--
-- The variants are checked before they are measured. 'verify' compares every
-- one of them against the library's 'stencilFor' on mixed-policy axis lists
-- and several neighbourhoods, and exits non-zero on the first disagreement,
-- so a variant that is fast because it is wrong fails rather than reports.
module Main (main) where

import           Data.Grid.Sized.Coord
import           Data.Grid.Sized.Coord.Clamped    (Clamped)
import           Data.Grid.Sized.Coord.Periodic   (Periodic)
import           Data.Grid.Sized.Coord.Reflective (Reflective)
import           Data.Grid.Sized.Stencil

import           Control.Concurrent      (getNumCapabilities)
import           Control.Monad           (forM_, unless, when)
import           Data.Kind               (Type)
import           ParStencil
import           System.Environment      (lookupEnv)
import           System.Exit             (exitFailure)
import           Test.Tasty.Bench

-- * The shapes
--
-- Names and sizes taken from @bench/Main.hs@ in the library, so a number here
-- can be read next to one there.

-- | 12 cells. Below any plausible threshold: here the fork is the whole cost.
type Tiny = '[Periodic 3, Clamped 4]

-- | 100, 400 and 1,024 cells: the sizes the parallel/sequential crossover
-- turned out to sit among.
type Small10 = '[Clamped 10, Clamped 10]

type Small20 = '[Clamped 20, Clamped 20]

type Small32 = '[Clamped 32, Clamped 32]

-- | 2,500 cells: the automaton step the library's stencil Haddock quotes.
type Step = '[Clamped 50, Clamped 50]

-- | 'Step' with the boundary policy that gives every cell a full row, so the
-- discovered width is the Moore bound and the bounded variants never compact.
type StepPeriodic = '[Periodic 50, Periodic 50]

-- | 10,000 cells.
type Mid = '[Clamped 100, Clamped 100]

-- | 90,000 cells, a real consumer's working size.
type Big = '[Clamped 300, Clamped 300]

-- | 90,000 cells again, periodic.
type BigPeriodic = '[Periodic 300, Periodic 300]

-- | Mixed policies on three axes: 64,000 cells whose rows are neither all
-- full nor all short. Three axes also makes the Moore bound 26 rather than 8,
-- so the fill writes rather more per cell.
type Mixed3 = '[Clamped 40, Periodic 40, Reflective 40]

-- * Checking

-- | The library's answer, as the pair the variants produce.
control :: forall (cs :: [Type]). IsCoordList cs => (Coord cs -> [Coord cs]) -> Table
control f = Table (stencilWidth s) (stencilPositions s)
  where
    s = stencilFor f

-- | Every variant against 'control', for one shape and one neighbourhood.
--
-- @ub@ must be an upper bound on the row length for the bounded variants;
-- 'mooreBound' at the shape's dimension is one for both neighbourhoods the
-- library defines, tight for Moore and loose for von Neumann, which is what
-- exercises the compacting branch.
verify ::
       forall (cs :: [Type]). IsCoordList cs
    => String
    -> Int
    -> (Coord cs -> [Coord cs])
    -> IO Bool
verify label ub f = do
    results <-
        mapM
            (\(name, tbl) -> pure (name, tbl == want))
            [ ("tableSeq", tableSeq f)
            , ("tableSeqBounded", tableSeqBounded ub f)
            , ("tableParWidth", tableParWidth 2 f)
            , ("tableParFill", tableParFill 2 f)
            , ("tableParBoth", tableParBoth 2 f)
            , ("tableParBounded", tableParBounded 2 ub f)
            , ("tableParChunked", tableParChunked 2 f)
            ]
    forM_ results $ \(name, ok) ->
        unless ok $ putStrLn ("MISMATCH: " ++ label ++ " / " ++ name)
    pure (all snd results)
  where
    want = control f

-- | The checks, over shapes whose policies differ from each other and
-- neighbourhoods whose rows are full, short and deduplicated.
checks :: IO Bool
checks =
    fmap and . sequence $
    [ verify @Step "Step moore 1" (mooreBound 1 2) (mooreNeighbours 1)
    , verify @Step "Step moore 2" (mooreBound 2 2) (mooreNeighbours 2)
    , verify @Step "Step vonNeumann 2" (mooreBound 2 2) (vonNeumannNeighbours 2)
    , verify @StepPeriodic "StepPeriodic moore 1" (mooreBound 1 2) (mooreNeighbours 1)
    , verify @StepPeriodic "StepPeriodic vonNeumann 1" (mooreBound 1 2) (vonNeumannNeighbours 1)
    , verify @Mixed3 "Mixed3 moore 1" (mooreBound 1 3) (mooreNeighbours 1)
    , verify @Mixed3 "Mixed3 vonNeumann 1" (mooreBound 1 3) (vonNeumannNeighbours 1)
    , verify @Tiny "Tiny moore 1" (mooreBound 1 2) (mooreNeighbours 1)
    , verify @Tiny "Tiny moore 3" (mooreBound 3 2) (mooreNeighbours 3)
      -- An asymmetric kernel nothing in the library defines, because
      -- 'stencilFor' takes any function and the variants must too.
    , verify @Step "Step east-only" (mooreBound 1 2) eastOnly
      -- The empty neighbourhood: width zero, table of no entries, the
      -- degenerate case every layout calculation here has to survive.
    , verify @Step "Step empty" (mooreBound 1 2) (const [])
    ]

-- | Every neighbour whose position is greater than the cell's own: not a
-- neighbourhood anyone wants, but a function 'stencilFor' accepts, with rows
-- that shorten monotonically across the grid and so a width the very first
-- chunk decides.
eastOnly :: IsCoordList cs => Coord cs -> [Coord cs]
eastOnly c = filter ((> coordPosition c) . coordPosition) (mooreNeighbours 1 c)

-- * Measuring

-- | Which neighbourhood a group builds a table for.
--
-- Both, because the allocation bound the bounded variants take is
-- 'mooreBound' either way and it is only /tight/ for the Moore one. Von
-- Neumann is how the compacting branch gets measured: at radius 2 on two axes
-- the bound is 24 and the width is 12, so the table is laid out at twice the
-- size it ends up in and copied down.
data Kind
    = Moore
    | VonNeumann

neighbourhoodOf :: IsCoordList cs => Kind -> Int -> Coord cs -> [Coord cs]
neighbourhoodOf Moore      = mooreNeighbours
neighbourhoodOf VonNeumann = vonNeumannNeighbours

-- | The full variant set for one shape, at the chunk multiplier the run was
-- given.
--
-- 'whnf' on a function of the radius rather than 'nf' on a value, and the
-- radius threaded through, so that nothing here is a CAF the first
-- measurement builds and the rest read back. WHNF is enough: 'Table' is
-- strict in both fields and an unboxed vector has no interior thunks.
buildGroup ::
       forall (cs :: [Type]). IsCoordList cs
    => String
    -> Kind
    -> Int
    -> Int
    -> Int
    -> Benchmark
buildGroup label kind dim mult r =
    bgroup
        label
        [ bench "stencilFor (library)" $
          whnf (\k -> stencilWidth (stencilFor @cs (nbh k))) r
        , bench "tableSeq" $ whnf (\k -> tblWidth (tableSeq @cs (nbh k))) r
        , bench "tableSeqBounded" $
          whnf (\k -> tblWidth (tableSeqBounded @cs (mooreBound k dim) (nbh k))) r
        , bench "tableParWidth" $
          whnf (\k -> tblWidth (tableParWidth @cs mult (nbh k))) r
        , bench "tableParFill" $
          whnf (\k -> tblWidth (tableParFill @cs mult (nbh k))) r
        , bench "tableParBoth" $
          whnf (\k -> tblWidth (tableParBoth @cs mult (nbh k))) r
        , bench "tableParBounded" $
          whnf (\k -> tblWidth (tableParBounded @cs mult (mooreBound k dim) (nbh k))) r
        , bench "tableParChunked" $
          whnf (\k -> tblWidth (tableParChunked @cs mult (nbh k))) r
        ]
  where
    nbh = neighbourhoodOf @cs kind

-- | The checks build a table for every shape they cover, which is fine when
-- the run is measuring time and ruinous when it is measuring residency:
-- @+RTS -s@ reports the high-water mark of the whole process, and the
-- @Mixed3@ check alone is a 13 MB table. Set @KB38_SKIP_CHECKS=1@ to measure
-- one benchmark's peak in isolation. Do not set it for a timing run.
skipChecks :: IO Bool
skipChecks = maybe False (/= "0") <$> lookupEnv "KB38_SKIP_CHECKS"

main :: IO ()
main = do
    skip <- skipChecks
    when skip $ putStrLn "KB38_SKIP_CHECKS set: not checking the variants"
    ok <- if skip then pure True else checks
    unless ok $ do
        putStrLn "variants disagree with the library; not measuring"
        exitFailure
    caps <- getNumCapabilities
    putStrLn ("capabilities: " ++ show caps)
    let mult = 2
    defaultMain
        [ buildGroup @Tiny "00,012 cells (Tiny), moore 1" Moore 2 mult 1
        , buildGroup @Small10 "00,100 cells (Small10), moore 1" Moore 2 mult 1
        , buildGroup @Small20 "00,400 cells (Small20), moore 1" Moore 2 mult 1
        , buildGroup @Small32 "01,024 cells (Small32), moore 1" Moore 2 mult 1
        , buildGroup @Step "02,500 cells (Step), moore 1" Moore 2 mult 1
        , buildGroup @StepPeriodic "02,500 cells (StepPeriodic), moore 1" Moore 2 mult 1
        , buildGroup @Mid "10,000 cells (Mid), moore 1" Moore 2 mult 1
        , buildGroup @Big "90,000 cells (Big), moore 1" Moore 2 mult 1
        , buildGroup @BigPeriodic "90,000 cells (BigPeriodic), moore 1" Moore 2 mult 1
        , buildGroup @Big "90,000 cells (Big), moore 3" Moore 2 mult 3
        , buildGroup @Mixed3 "64,000 cells (Mixed3), moore 1" Moore 3 mult 1
          -- The loose bound: 'mooreBound' 2 2 is 24 and the von Neumann width
          -- is 12, so these two are the only groups where the bounded
          -- variants pay for a compacting copy.
        , buildGroup @Step "02,500 cells (Step), vonNeumann 2" VonNeumann 2 mult 2
        , buildGroup @Big "90,000 cells (Big), vonNeumann 2" VonNeumann 2 mult 2
        ]
