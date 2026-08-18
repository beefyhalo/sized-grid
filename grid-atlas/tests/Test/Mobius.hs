{-# LANGUAGE DataKinds #-}

-- | Tests for the Mobius strip ("Data.Grid.Atlas.Mobius", sized-grid-00v).
--
-- Two checks, the same shape @Test.CubeMap@ uses to verify 'cubeSeam' and
-- 'cubeStep':
--
--   * every half-edge of the chart names a destination that names it back
--     ('mobiusSeamPairsUp'), 'seamViolations' (sized-grid-b15) applied to
--     'mobiusSeam'\'s own half-edges;
--   * a walker who never turns, always heading towards 'Wrapped'\'s
--     @'AtMax'@ end, lands on the mirrored row after crossing the seam once
--     (an odd number of times) and back on its exact starting row after
--     crossing it twice (an even number, the two flips cancelling) ---
--     the concrete, checkable statement of \"a Mobius strip has one edge,
--     not two\" ('mobiusStepBeltCloses').
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

-- | Every half-edge of the strip's one chart: the enumeration
-- 'seamViolations' checks the table over.
mobiusHalfEdges :: [HalfEdge () (Axis, Extremum)]
mobiusHalfEdges = [((), (a, e)) | a <- allAxes, e <- allExtrema]

mobiusSeamPairsUp :: TestTree
mobiusSeamPairsUp =
    testCase "mobiusSeam pairs every half-edge with one that points back" $
    assertEqual
        "half-edges whose partner does not point back"
        []
        (seamViolations mobiusSeam mobiusHalfEdges)

-- | The strip's width, fixed at the smallest value with an interior cell
-- (@> 2@), so a belt walk genuinely crosses several cells before it meets
-- the seam.
type W = 5

-- | The strip's height, deliberately different from 'W' --- nothing about a
-- self-gluing on one axis requires the other axis to be the same size, and
-- a shared value would hide a transposed-axis bug that a mismatched one
-- cannot.
type H = 3

mobiusStepBeltCloses :: TestTree
mobiusStepBeltCloses =
    testCase
        "mobiusStep, heading right, mirrors the row after one lap and closes after two" $
    mapM_
        (\(u, v) ->
             let w = ordinalSize @W
                 start = ((minBound, Clamped u :| Clamped v :| EmptyCoord), Heading Wrapped AtMax)
                 step s =
                     case uncurry mobiusStep s of
                         Just s' -> s'
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
