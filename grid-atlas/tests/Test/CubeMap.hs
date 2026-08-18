{-# LANGUAGE DataKinds #-}

-- | Tests for "Data.Grid.Atlas.CubeMap".
module Test.CubeMap
  ( cubeMapTests
  ) where

import           Data.Atlas.Topology.Seam (HalfEdge, seamViolations)
import           Data.Grid.Atlas.CubeMap
import           Data.Grid.Sized

import           Test.Tasty
import           Test.Tasty.HUnit

allFaces :: [Face]
allFaces = [minBound .. maxBound]

allAxes :: [Axis]
allAxes = [minBound .. maxBound]

allExtrema :: [Extremum]
allExtrema = [minBound .. maxBound]

-- | Spelled out rather than derived from a class because a boundary label is
-- a product type, which base gives 'Bounded' but not 'Enum'.
cubeHalfEdges :: [HalfEdge Face (Axis, Extremum)]
cubeHalfEdges =
    [(f, (a, e)) | f <- allFaces, a <- allAxes, e <- allExtrema]

cubeSeamPairsUp :: TestTree
cubeSeamPairsUp =
    testCase "cubeSeam pairs every half-edge with one that points back" $
    assertEqual
        "half-edges whose partner does not point back"
        []
        (seamViolations cubeSeam cubeHalfEdges)

-- | Smallest size with an interior cell on every axis.
type N = 5

cubeStepBeltCloses :: TestTree
cubeStepBeltCloses =
    testCase
        "cubeStep, followed for 4n steps from any start, returns to that exact start" $
    mapM_
        (\(face, axis, side, u, v) ->
             let start = ((faceIndex face, u :| v :| EmptyCoord), Heading axis side)
                 n = ordinalSize @N
                 got = iterate (uncurry (cubeStep @N)) start !! (4 * n)
             in assertEqual (show (face, axis, side, u, v)) start got)
        [ (face, axis, side, u, v)
        | face <- allFaces
        , axis <- allAxes
        , side <- allExtrema
        , u <- [minBound .. maxBound] :: [Ordinal N]
        , v <- [minBound .. maxBound] :: [Ordinal N]
        ]

cubeMapTests :: TestTree
cubeMapTests =
    testGroup "Data.Grid.Atlas.CubeMap" [cubeSeamPairsUp, cubeStepBeltCloses]
