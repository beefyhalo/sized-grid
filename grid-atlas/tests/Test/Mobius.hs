{-# LANGUAGE DataKinds #-}

module Test.Mobius
  ( mobiusTests
  ) where

import           Data.Atlas.Topology.Seam (HalfEdge, seamViolations)
import           Data.Grid.Atlas.Mobius
import           Data.Grid.Sized

import           Test.Tasty
import           Test.Tasty.HUnit

allAxes :: [Axis]
allAxes = [minBound .. maxBound]

allExtrema :: [Extremum]
allExtrema = [minBound .. maxBound]

mobiusHalfEdges :: [HalfEdge () (Axis, Extremum)]
mobiusHalfEdges = [((), (a, e)) | a <- allAxes, e <- allExtrema]

mobiusSeamPairsUp :: TestTree
mobiusSeamPairsUp =
    testCase "mobiusSeam pairs every half-edge with one that points back" $
    assertEqual
        "half-edges whose partner does not point back"
        []
        (seamViolations mobiusSeam mobiusHalfEdges)

type W = 5
type H = 3

mobiusStepBeltCloses :: TestTree
mobiusStepBeltCloses =
    testCase
        "mobiusStep, heading right, mirrors the row after one lap and closes after two" $
    mapM_
        (\(u, v) ->
             let w = ordinalSize @W
                 start = ((minBound, Clamped u :| Clamped v :| EmptyCoord), Heading Wrapped AtMax)
                 step s@(c, hd) =
                     case mobiusStep c hd of
                         Just (c', hd', _) -> (c', hd')
                         Nothing ->
                             error
                                 ("mobiusStepBeltCloses: unexpected Straight-axis edge at " ++
                                  show s)
                 afterOneLap = iterate step start !! w
                 afterTwoLaps = iterate step start !! (2 * w)
                 h = ordinalSize @H
                 vi = ordinalToInt v
                 mirroredRow = ((minBound, Clamped u :| Clamped (unsafeOrdinal (h - 1 - vi)) :| EmptyCoord), Heading Wrapped AtMax)
             in do
                    assertEqual (show (u, v) ++ ": after one lap") mirroredRow afterOneLap
                    assertEqual (show (u, v) ++ ": after two laps") start afterTwoLaps)
        [ (u, v)
        | u <- [minBound .. maxBound] :: [Ordinal W]
        , v <- [minBound .. maxBound] :: [Ordinal H]
        ]

mobiusTests :: TestTree
mobiusTests =
    testGroup "Data.Grid.Atlas.Mobius" [mobiusSeamPairsUp, mobiusStepBeltCloses]
