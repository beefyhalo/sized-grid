{-# LANGUAGE DataKinds #-}

-- | Tests for the cube map ("Data.Grid.Atlas.CubeMap", sized-grid-68j).
--
-- Neither check here is a spot-check against a handful of hand-picked
-- values. Both are the two properties the module haddock cites as how
-- 'cubeSeam' was actually verified while deriving it, made permanent so a
-- future edit to the table cannot silently break either one:
--
--   * every one of the 24 half-edges names a destination that names it back
--     ('cubeSeamPairsUp'), the exhaustive check that the table really is 12
--     edges, not 24 independent (and possibly inconsistent) guesses. The law
--     itself is @atlas-topology@'s 'seamViolations' (sized-grid-b15) --- it
--     says nothing about squares or cells, so it belongs to seam tables in
--     general, and this suite only supplies the cube's own half-edges to
--     check it over;
--   * a walker following a fixed heading returns to its exact starting
--     chart, coordinate, and heading after exactly @4n@ steps, for every one
--     of the 600 possible (face, axis, side, position) starting points on an
--     n=5 cube ('cubeStepBeltCloses') --- the property an incorrect
--     transition or a sign error in the frame transform is very unlikely to
--     satisfy by accident.
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

-- | Every half-edge of the cube: the enumeration 'seamViolations' checks the
-- table over. Spelled out here rather than derived from a class because a
-- boundary label is a product type, which base gives 'Bounded' but not
-- 'Enum'.
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

-- | A face size fixed at the smallest value that still has an interior cell
-- on every axis (@> 2@), so a belt walk genuinely crosses several cells of
-- each face rather than only ever landing on an edge.
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
