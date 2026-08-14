{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- |
-- Baseline benchmarks for the operations the real workloads hit.
--
-- Shapes are taken from the consumer rather than invented: 'Big' is the
-- @300 x 300@ grid of ../aoc/src/2018/11.hs and 'Mid' the @100 x 100@ of
-- ../aoc/src/2015/18.hs and ../aoc/src/2018/18.hs.
--
-- Every benchmark reduces to an 'Int' or a 'Bool' so that 'whnf' forces the
-- whole computation. That avoids an orphan 'NFData' instance for 'Grid', and it
-- avoids the trap of timing a 'whnf' that stops at the outermost constructor:
-- @Grid@ is a newtype over a boxed vector, so WHNF of a @tabulate@ would
-- measure almost nothing.
--
-- == Do not compare against the recorded CSV across sessions
--
-- @bench\/baseline-ghc9.12.3-aarch64-darwin.csv@ holds the times of the code at
-- the commit that recorded it, on the machine state of that moment, and that
-- second half does not keep. Re-running the /same/ code weeks later measured
-- every benchmark 1.9x slower, uniformly --- including @pure@ and @fmap@, which
-- touch none of the machinery being worked on. Read against the stale CSV, a
-- change that made coordinate arithmetic twice as fast looked like a
-- regression.
--
-- So the allocation columns are the ones that transfer between sessions; they
-- are deterministic. To compare times, measure both sides now:
--
-- > git archive HEAD | tar -x -C /tmp/before
-- > (cd /tmp/before && cabal run bench:benchmarks -- --csv /tmp/before.csv)
-- > cabal run bench:benchmarks -- --baseline /tmp/before.csv
--
-- tasty-bench then prints each result as a percentage of the run it can
-- actually be compared with.
module Main (main) where

import           Data.Grid.Sized

import           Control.Comonad
import           Control.Comonad.Store  (peek, pos)
import           Control.DeepSeq        (NFData (..))
import           Control.Exception      (evaluate)
import           Control.Lens           (ifoldl', imap, itraverse, view)
import           Data.Aeson             (Result (..), fromJSON, toJSON)
import           Data.AffineSpace       ((.+^), (.-.))
import           Data.Functor.Rep       (index, tabulate)
import           Data.Maybe             (isJust)
import           Test.Tasty.Bench

-- | ../aoc/src/2018/11.hs works at this size: 90,000 cells.
type Big = '[Clamped 300, Clamped 300]

-- | ../aoc/src/2015/18.hs and ../aoc/src/2018/18.hs: 10,000 cells.
type Mid = '[Clamped 100, Clamped 100]

-- | Small enough that the comonadic step stays interactive: 2,500 cells.
type Step = '[Clamped 50, Clamped 50]

-- | Periodic, so walking never saturates the way Clamped's clamp would.
type Walk = '[Periodic 300, Periodic 300]

bigGrid :: Grid Big Int
bigGrid = tabulate coordPosition

midGrid :: Grid Mid Int
midGrid = tabulate coordPosition

stepGrid :: FocusedGrid Step Int
stepGrid = FocusedGrid (tabulate coordPosition) zeroCoord

-- | Total a grid, forcing every element on the way.
total :: (Foldable f) => f Int -> Int
total = foldl' (+) 0

-- | A game-of-life shaped step: read the Moore neighbourhood and fold it.
-- Mirrors the 'experiment'/'peek' pattern in the consumer's rules.
--
-- This reads eight cells rather than the nine 'moorePoints' used to return:
-- that one included the centre, and every caller filtered it back out.
neighbourSum :: FocusedGrid Step Int -> Int
neighbourSum fg = total [peek p fg | p <- neighbours (pos fg)]

-- | Repeated coordinate offset. Recursive rather than a fold so the
-- intermediate 'Coord's cannot be fused away.
walk :: Int -> Coord Walk -> Int
walk 0 c = coordPosition c
walk k c = walk (k - 1) (c .+^ (1 :| 1 :| EmptyCoord))

-- | Four corner reads per position at a fixed window size: the summed-area-table
-- solve of ../aoc/src/2018/11.hs, which is 360,000 offsets over the 90,000 cells
-- of 'Big'.
--
-- This is the workload sized-grid-0tj was found on, and it is here because the
-- two benchmarks above did not catch it. @walk@ offsets a 'Periodic' coord it
-- carries from step to step, and the @(.-.)@ one does no offsetting at all;
-- neither measures a /constant/ displacement applied to every coordinate of a
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
-- 'Big'.
--
-- It exists because 'cornerReads' does not reach 'offsetCoord' and neither does
-- anything else here. @('.+^')@ and 'offsetCoord' are different functions over
-- different folds --- 'Data.Grid.Sized.Coord.AffineCoordList' and
-- 'Data.Grid.Sized.Coord.Class.IsCoordList' respectively --- so a change to one is
-- invisible to a benchmark of the other. sized-grid-135 was filed against the
-- neighbourhood benchmark on the assumption that neighbourhoods offset; they do
-- not, they enumerate ('stepsWithin'), and so the suite had no measurement of
-- the checked offset at all.
--
-- Counts successes rather than summing positions, so that the 'Maybe' is forced
-- without an index into the grid on top. Most offsets succeed and the last @k@
-- rows and columns refuse, so both branches of the bounds check are measured
-- rather than only the one.
--
-- Consumed with 'total' and not with @length@, so that it is 'cornerReads' with
-- the offset swapped and nothing else changed. @length@ over the same
-- comprehension is @foldr@ with a function accumulator: it allocates a closure
-- per element, and at 360,000 elements that buried the operation being measured
-- under 130 MB of its own.
--
-- The window size is an argument for the reason given in the note on
-- @allCoord@ below: with the displacements written as literals the whole list
-- is a CAF, GHC floats it past the lambda, and the benchmark reports 3.36 ns
-- for 360,000 offsets. Taking @k@ from the caller is what 'axisOffsets' does
-- and it is what makes the list depend on the argument.
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

-- | The same 360,000 offsets as 'cornerReads', on a bare axis instead of a
-- 'Coord', and it exists to be read against it.
--
-- The pair separates two costs that sized-grid-0tj ran together, and has to,
-- because the issue's own decomposition could not: it compared a 'Coord' loop
-- against an 'Int' loop, which removes the per-axis arithmetic and the 'NP'
-- rebuild in one step and so cannot say which was paying.
--
-- Measured apart, across the @Diff Integer -> Diff Int@ change: this benchmark
-- went from 8.41 ms and 22 MB to 2.20 ms and 94 KB, while 'cornerReads' above
-- went from 32.4 ms and 143 MB to only 28.6 ms and 126 MB. The per-axis
-- operation was fixed outright and the loop barely moved, which places the
-- remaining 126 MB in the fold over the axis list rather than in the arithmetic.
--
-- Keep both. Improving the axis arithmetic shows up here, improving the fold
-- shows up in 'cornerReads', and only the gap between them says which is worth
-- doing.
axisOffsets :: Int -> Int
axisOffsets k =
    total
        [ ordinalToInt (view asOrdinal (c .+^ d))
        | c <- allCoordLike @300 @Clamped
        , d <- [1 .. k]
        ]

-- Note on why there is no standalone @allCoord@ benchmark.
--
-- 'allCoord' takes no value arguments, so at a fixed type it is a CAF: the
-- obvious benchmark builds the list once and then measures a pointer.
-- Empirically it reported 1.96 ns for 90,000 coordinates. Hiding it behind a
-- NOINLINE function with a 'Proxy' argument does not help either -- GHC's full
-- laziness floats the list out past the 'Proxy' lambda, and it still reported
-- 1.72 ns.
--
-- Only @-fno-full-laziness@ would defeat that, and turning it on would make
-- every other benchmark here stop resembling how the library is actually
-- compiled. So the cost is measured where it is real instead, in the library's
-- own instances, which receive @IsCoordList@ at runtime.
--
-- == The second group is named for what it actually measures
--
-- It used to be called "allCoord overhead", paired so that each member differs
-- from its neighbour by whether the operation walks the coordinate list. But the two members of a pair also differ in what they do
-- per cell -- @tabulate@ and @imap@ are given 'coordPosition' as their function
-- and @pure@ and @fmap@ are not -- and 'coordPosition' turned out to cost far
-- more than the coordinate it is handed. Reading the gap as allCoord's cost is
-- what sent sized-grid-uvd looking in the wrong place.
--
-- The honest figures, from the @index@ benchmark below (which shares its
-- coordinate list, so it is 90,000 'coordPosition' calls and nothing else)
-- against @imap@ over the same 90,000 cells: 'coordPosition' was ~800 bytes a
-- call, and the coordinate list about 67 bytes a cell.
--
-- 'coordPosition' is no longer the expensive half. Moving its fold into
-- 'Data.Grid.Sized.Coord.Class.IsCoordList' as a method let it unroll at a concrete
-- axis list, and @index x90000@ went from 27 MB to 38 bytes total --- the whole
-- benchmark, not per call. What the second group now measures is much closer to
-- the coordinate list it was originally named for. The list is also not
-- rebuilt in the sense the issue meant -- @V.zipWith@ fuses with the
-- @V.fromList@ that feeds it, so the coordinates are produced and consumed one
-- at a time and never all exist at once. Do not "fix" that; see the note on the
-- instances in Data.Grid.Sized.Internal.Grid.

main :: IO ()
main = do
    -- Force the shared inputs once, so their construction is not charged to
    -- whichever benchmark happens to run first.
    _ <- evaluate (total bigGrid)
    _ <- evaluate (total midGrid)
    _ <- evaluate (total (focusedGrid stepGrid))
    defaultMain
        [ bgroup
              "coord arithmetic"
              [ bench "(.+^) x10000, Periodic 300x300" $
                whnf (\n -> walk n zeroCoord) 10000
              , bench "(.-.) x10000, Clamped 100x100 (coord list shared)" $
                whnf
                    -- No 'fromIntegral': the displacement is an 'Int' as of
                    -- sized-grid-0tj, so the conversion this used to need is
                    -- now the identity and warns under -Widentities.
                    (\o -> total [view coordHead (c .-. o) | c <- allCoord @Mid])
                    zeroCoord
              , bench "(.+^) x360000, four corner reads over Clamped 300x300" $
                whnf cornerReads bigGrid
              , bench "offsetCoord x360000, checked, over Clamped 300x300" $
                whnf checkedCornerReads 3
              , bench "(.+^) x360000, one Clamped 300 axis (no Coord)" $
                whnf axisOffsets 1200
              , bench "toEnum/fromEnum x300, Clamped 300" $
                whnf
                    (\n -> total [fromEnum (toEnum i :: Clamped 300) | i <- [0 .. n]])
                    299
              ]
        , bgroup
              "indexed vs unindexed (the gap is coordPosition, not allCoord)"
              [ bench "tabulate 300x300  [coordPosition per cell]" $
                whnf (\f -> total (tabulate f :: Grid Big Int)) coordPosition
              , bench "pure 300x300      [no coord at all]" $
                whnf (\x -> total (pure x :: Grid Big Int)) 1
              , bench "imap 300x300      [coordPosition per cell]" $
                whnf (\g -> total (imap (\c x -> coordPosition c + x) g)) bigGrid
              , bench "fmap 300x300      [no coord at all]" $
                whnf (\g -> total (fmap (+ 1) g)) bigGrid
              ]
        , bgroup
              "grid access"
              [ bench "index x90000, 300x300 (coord list shared)" $
                whnf (\g -> total (map (index g) (allCoord @Big))) bigGrid
              ]
        , bgroup
              "indexed traversals"
              [ bench "ifoldl' 300x300" $
                whnf (ifoldl' (\c acc x -> acc + coordPosition c + x) 0) bigGrid
              , bench "itraverse 100x100" $
                whnf
                    (\g -> maybe 0 total (itraverse (\_ x -> Just x) g))
                    midGrid
              ]
        , bgroup
              "comonad"
              [ bench "extract 50x50" $ whnf extract stepGrid
              , bench "extend neighbourSum 50x50" $
                whnf (total . focusedGrid . extend neighbourSum) stepGrid
              ]
        , bgroup
              "collapse round trip"
              [ bench "collapseGrid 100x100" $
                whnf (total . map total . collapseGrid) midGrid
              , bench "gridFromList . collapseGrid 100x100" $
                whnf
                    (\g -> maybe 0 total (gridFromList (collapseGrid g) :: Maybe (Grid Mid Int)))
                    midGrid
              ]
        , bgroup
              "json"
              [ bench "toJSON 100x100" $ whnf (jsonSize . toJSON) midGrid
              , bench "fromJSON . toJSON 100x100" $
                whnf (roundTrips . toJSON) midGrid
              ]
        ]
  where
    jsonSize v = rnf v `seq` (1 :: Int)
    roundTrips v =
        case fromJSON v :: Result (Grid Mid Int) of
            Success g -> total g
            Error _   -> -1
