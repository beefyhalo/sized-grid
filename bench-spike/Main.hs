{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- |
-- THROWAWAY SPIKE for sized-grid-up6. Not part of the library's benchmark
-- story; delete once the decision it exists to inform has been made.
--
-- The question: now that sized-grid-upo has made 'Ordinal' a newtype over
-- 'Int', how much is left for an unboxed 'Grid' to win? The 93 MB figure in
-- up6's description was measured before upo and was mostly 'Coord' boxing, so
-- it cannot be used to justify the work any more.
--
-- The method: a minimal unboxed grid ('UGrid') with just enough API to run the
-- same workloads as the real boxed 'Grid', benchmarked side by side at the
-- consumer's own shape. Each pair of benchmarks differs in the vector type and
-- nothing else -- in particular both sides go through 'coordPosition' and
-- 'allCoord', so the coordinate machinery cancels and what remains is the
-- representation.
--
-- The row-prefix pair is deliberately NOT boxed-through-'mapLowerDim' against
-- unboxed-through-a-hand-rolled-loop: that would measure 'mapLowerDim', not the
-- vector. Both sides use the same slice-and-scan shape.
module Main (main) where

import           SizedGrid
import           SizedGrid.Grid.Unsafe  (unsafeGridFromVector)

import           Data.AffineSpace       ((.+^))
import           Data.Functor.Rep       (index, tabulate)
import           Data.Kind              (Type)
import qualified Data.Vector            as V
import qualified Data.Vector.Unboxed    as U
import           Test.Tasty.Bench

-- | ../aoc/src/2018/11.hs works at this size: 90,000 cells.
type Big = '[Clamped 300, Clamped 300]

side :: Int
side = 300

--------------------------------------------------------------------------------
-- The minimal unboxed grid.
--
-- No Functor/Applicative/Representable: an unboxed vector cannot have them.
-- What it can have is exactly what the numeric workloads use, which is the
-- point of option 2 in the issue.
--------------------------------------------------------------------------------

newtype UGrid (cs :: [Type]) a = UGrid (U.Vector a)

utabulate :: forall cs a. (U.Unbox a, IsCoordList cs) => (Coord cs -> a) -> UGrid cs a
utabulate f = UGrid $ U.fromListN (coordSpaceSize @cs) $ map f allCoord

uindex :: (U.Unbox a, IsCoordList cs) => UGrid cs a -> Coord cs -> a
uindex (UGrid v) c = v `U.unsafeIndex` coordPosition c

umap :: (U.Unbox a, U.Unbox b) => (a -> b) -> UGrid cs a -> UGrid cs b
umap f (UGrid v) = UGrid (U.map f v)

utotal :: (U.Unbox a, Num a) => UGrid cs a -> a
utotal (UGrid v) = U.foldl' (+) 0 v

-- | Prefix-sum each row independently, the operation
-- @mapLowerDim (Identity . scanl1Grid (+))@ performs on the boxed side.
urowPrefix :: U.Unbox a => (a -> a -> a) -> UGrid Big a -> UGrid Big a
urowPrefix f (UGrid v) =
    UGrid $ U.concat [U.scanl1' f (U.slice (i * side) side v) | i <- [0 .. side - 1]]

utranspose :: (U.Unbox a) => UGrid Big a -> UGrid Big a
utranspose g = utabulate (\i -> uindex g (tranposeCoord i))

--------------------------------------------------------------------------------
-- The boxed side, written to the same shape so the comparison is honest.
--------------------------------------------------------------------------------

total :: Foldable f => f Int -> Int
total = foldl' (+) 0

browPrefix :: (a -> a -> a) -> Grid Big a -> Grid Big a
browPrefix f (gridVector -> v) =
    unsafeGridFromVector $
    V.concat [V.scanl1' f (V.slice (i * side) side v) | i <- [0 .. side - 1]]

btranspose :: Grid Big Int -> Grid Big Int
btranspose g = tabulate (\i -> index g (tranposeCoord i))

--------------------------------------------------------------------------------
-- The real workload: ../aoc/src/2018/11.hs.
--------------------------------------------------------------------------------

power :: Int -> Coord Big -> Int
power serial ((fromEnum -> y) :| (fromEnum -> x) :| _) =
    ((rack * y + serial) * rack `div` 100) `mod` 10 - 5
  where
    rack = x + 10

-- | Summed-area table, built exactly as the consumer builds it: prefix along
-- the rows, transpose, prefix along the rows again, transpose back.
bSAT :: Int -> Grid Big Int
bSAT = btranspose . browPrefix (+) . btranspose . browPrefix (+) . tabulate . power

uSAT :: Int -> UGrid Big Int
uSAT = utranspose . urowPrefix (+) . utranspose . urowPrefix (+) . utabulate . power

-- | The hot loop: four corner reads per candidate square, over every position.
bSolve :: Int -> Grid Big Int -> Int
bSolve sz sat = maximum [squareSum p | p <- allCoord @Big]
  where
    d = fromIntegral (sz - 1)
    corners =
        [ (coordFromTuple (d, d), 1)
        , (coordFromTuple (-1, d), -1)
        , (coordFromTuple (d, -1), -1)
        , (coordFromTuple (-1, -1), 1)
        ]
    squareSum p = sum [s * index sat (p .+^ off) | (off, s) <- corners]

uSolve :: Int -> UGrid Big Int -> Int
uSolve sz sat = maximum [squareSum p | p <- allCoord @Big]
  where
    d = fromIntegral (sz - 1)
    corners =
        [ (coordFromTuple (d, d), 1)
        , (coordFromTuple (-1, d), -1)
        , (coordFromTuple (d, -1), -1)
        , (coordFromTuple (-1, -1), 1)
        ]
    squareSum p = sum [s * uindex sat (p .+^ off) | (off, s) <- corners]

--------------------------------------------------------------------------------
-- Sizing sized-grid-0tj: how much of the solve is recoverable at all?
--
-- Three variants of the same loop, each removing one layer of coordinate
-- machinery, so the gaps between them attribute the cost:
--
--   uSolve          Coord throughout                  (what the library gives you)
--   uSolveRawOffset allCoord to enumerate, Int arithmetic for the corners
--                                                     (gap from uSolve = cost of (.+^))
--   uSolveRawAll    Int loops throughout              (gap = cost of allCoord)
--
-- The clamping is written out by hand to match what Clamped's (.+^) does, so
-- all three compute the same answer -- checked in main.
--------------------------------------------------------------------------------

clampIx :: Int -> Int
clampIx x = max 0 (min (side - 1) x)
{-# INLINE clampIx #-}

-- | Read the unboxed grid at a raw (row, col), clamping as Clamped would.
uat :: UGrid Big Int -> Int -> Int -> Int
uat (UGrid v) r c = v `U.unsafeIndex` (clampIx r * side + clampIx c)
{-# INLINE uat #-}

uSolveRawOffset :: Int -> UGrid Big Int -> Int
uSolveRawOffset sz sat = maximum [squareSum p | p <- allCoord @Big]
  where
    d = sz - 1
    squareSum ((fromEnum -> r) :| (fromEnum -> c) :| _) =
        uat sat (r + d) (c + d) - uat sat (r - 1) (c + d) - uat sat (r + d) (c - 1) +
        uat sat (r - 1) (c - 1)

uSolveRawAll :: Int -> UGrid Big Int -> Int
uSolveRawAll sz sat = maximum [squareSum r c | r <- [0 .. side - 1], c <- [0 .. side - 1]]
  where
    d = sz - 1
    squareSum r c =
        uat sat (r + d) (c + d) - uat sat (r - 1) (c + d) - uat sat (r + d) (c - 1) +
        uat sat (r - 1) (c - 1)

--------------------------------------------------------------------------------

bigGrid :: Grid Big Int
bigGrid = tabulate coordPosition

ubigGrid :: UGrid Big Int
ubigGrid = utabulate coordPosition

-- | The decomposition is only meaningful if the three variants compute the same
-- thing, and the hand-written clamping is exactly where that could silently go
-- wrong. Check it before measuring rather than trusting it.
checkAgreement :: IO ()
checkAgreement = do
    let sat = uSAT 18
        results = [uSolve 3 sat, uSolveRawOffset 3 sat, uSolveRawAll 3 sat]
        boxedResult = bSolve 3 (bSAT 18)
    putStrLn $ "solve variants (must all agree): " ++ show (boxedResult : results)
    if all (== boxedResult) results
        then putStrLn "OK: all four solve variants agree\n"
        else error "solve variants disagree -- the decomposition below is meaningless"

main :: IO ()
main = do
    checkAgreement
    defaultMain
        [ bgroup
              "build 300x300"
              [ bench "boxed   tabulate" $ whnf (\f -> total (tabulate f :: Grid Big Int)) coordPosition
              , bench "unboxed tabulate" $ whnf (\f -> utotal (utabulate f :: UGrid Big Int)) coordPosition
              ]
        , bgroup
              "read x90000"
              [ bench "boxed   index" $ whnf (\g -> total (map (index g) (allCoord @Big))) bigGrid
              , bench "unboxed index" $ whnf (\g -> total (map (uindex g) (allCoord @Big))) ubigGrid
              ]
        , bgroup
              "map then sum 300x300"
              [ bench "boxed   fmap" $ whnf (\g -> total (fmap (+ 1) g)) bigGrid
              , bench "unboxed umap" $ whnf (\g -> utotal (umap (+ 1) g)) ubigGrid
              ]
        , bgroup
              "fold 300x300"
              [ bench "boxed   foldl'" $ whnf total bigGrid
              , bench "unboxed foldl'" $ whnf utotal ubigGrid
              ]
        , bgroup
              "row prefix sums 300x300"
              [ bench "boxed   scanl1" $ whnf (total . browPrefix (+)) bigGrid
              , bench "unboxed scanl1" $ whnf (utotal . urowPrefix (+)) ubigGrid
              ]
        , bgroup
              "transpose 300x300"
              [ bench "boxed  " $ whnf (total . btranspose) bigGrid
              , bench "unboxed" $ whnf (utotal . utranspose) ubigGrid
              ]
        , bgroup
              "SAT build (2018/11)"
              [ bench "boxed  " $ whnf (total . bSAT) 18
              , bench "unboxed" $ whnf (utotal . uSAT) 18
              ]
        , bgroup
              "SAT solve size 3 (360,000 reads)"
              [ bench "boxed  " $ whnf (bSolve 3) (bSAT 18)
              , bench "unboxed" $ whnf (uSolve 3) (uSAT 18)
              ]
          -- Sizing sized-grid-0tj. Each step removes one layer of coordinate
          -- machinery from the same loop over the same unboxed grid.
        , bgroup
              "SAT solve, coordinate cost decomposition"
              [ bench "Coord throughout        " $ whnf (uSolve 3) (uSAT 18)
              , bench "allCoord + Int corners  " $ whnf (uSolveRawOffset 3) (uSAT 18)
              , bench "Int loops throughout    " $ whnf (uSolveRawAll 3) (uSAT 18)
              ]
        ]
