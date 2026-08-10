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
module Main (main) where

import           SizedGrid

import           Control.Comonad
import           Control.Comonad.Store  (peek, pos)
import           Control.DeepSeq        (NFData (..))
import           Control.Exception      (evaluate)
import           Control.Lens           (ifoldl', imap, itraverse)
import           Data.Aeson             (Result (..), fromJSON, toJSON)
import           Data.AffineSpace       ((.+^), (.-.))
import           Data.Functor.Rep       (index, tabulate)
import           Test.Tasty.Bench

-- | ../aoc/src/2018/11.hs works at this size: 90,000 cells.
type Big = '[HardWrap 300, HardWrap 300]

-- | ../aoc/src/2015/18.hs and ../aoc/src/2018/18.hs: 10,000 cells.
type Mid = '[HardWrap 100, HardWrap 100]

-- | Small enough that the comonadic step stays interactive: 2,500 cells.
type Step = '[HardWrap 50, HardWrap 50]

-- | Periodic, so walking never saturates the way HardWrap's clamp would.
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
neighbourSum :: FocusedGrid Step Int -> Int
neighbourSum fg =
    total [peek p fg | p <- moorePoints (1 :: Integer) (pos fg)]

-- | Repeated coordinate offset. Recursive rather than a fold so the
-- intermediate 'Coord's cannot be fused away.
walk :: Int -> Coord Walk -> Int
walk 0 c = coordPosition c
walk k c = walk (k - 1) (c .+^ (1, 1))

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
-- compiled. So the cost is measured where it is real instead: the library's own
-- instances receive @All IsCoordLifted@ at runtime, so they cannot share the
-- list, and the "allCoord overhead" group below pairs each allCoord-using
-- operation with the closest one that does not use it. The difference is the
-- number sized-grid-uvd is about.

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
              , bench "(.-.) x10000, HardWrap 100x100 (coord list shared)" $
                whnf
                    (\o -> total [fromIntegral (fst (c .-. o)) | c <- allCoord @Mid])
                    zeroCoord
              , bench "toEnum/fromEnum x300, HardWrap 300" $
                whnf
                    (\n -> total [fromEnum (toEnum i :: HardWrap 300) | i <- [0 .. n]])
                    299
              ]
        , bgroup
              "allCoord overhead (each pair differs only by whether allCoord is rebuilt)"
              [ bench "tabulate 300x300  [rebuilds allCoord]" $
                whnf (\f -> total (tabulate f :: Grid Big Int)) coordPosition
              , bench "pure 300x300      [does not]" $
                whnf (\x -> total (pure x :: Grid Big Int)) 1
              , bench "imap 300x300      [rebuilds allCoord]" $
                whnf (\g -> total (imap (\c x -> coordPosition c + x) g)) bigGrid
              , bench "fmap 300x300      [does not]" $
                whnf (\g -> total (fmap (+ 1) g)) bigGrid
              ]
        , bgroup
              "grid access"
              [ bench "index x90000, 300x300 (coord list shared)" $
                whnf (\g -> total (map (index g) (allCoord @Big))) bigGrid
              ]
        , bgroup
              "indexed traversals (these rebuild allCoord per call)"
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
