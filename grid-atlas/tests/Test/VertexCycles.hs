-- | The vertex-cycle law against the seam tables that ship, rather than
-- against tables written to exercise it.
--
-- "Data.Atlas.Topology.Seam" tests the same law on hand-written squares. The
-- point of repeating it here is that these tables were written for
-- 'Data.Grid.Atlas.Rect.rectStep' and not for this, so they are the check
-- that the corner-enumeration contract in 'Corner' describes the gluings the
-- library actually has --- including a cube, whose seams turn a corner
-- between axes where a single-chart surface's never do.
module Test.VertexCycles
  ( vertexCycleTests
  ) where

import           Data.Atlas.Topology.Seam   (Corner, vertexCycleLengths,
                                             vertexCycleViolations)
import           Data.Grid.Atlas.CubeMap    (Face, cubeSeam)
import           Data.Grid.Atlas.Klein      (kleinSeam)
import           Data.Grid.Atlas.Projective (projectiveSeam)
import           Data.Grid.Atlas.Rect       (Axis (..))
import           Data.Grid.Sized            (Extremum (..))

import           Test.Tasty
import           Test.Tasty.HUnit

-- | One rectangular chart's four corners in row-major order: @(0,0)@,
-- @(max,0)@, @(0,max)@, @(max,max)@.
--
-- That order is what 'Corner' asks for. A @U@-axis boundary's along-edge
-- coordinate is @v@ and a @V@-axis boundary's is @u@, so each of the four
-- boundaries has its @0@ end listed before its @max@ end --- which is the
-- direction 'rectStep' measures its own orientation bit along.
rectCorners :: chart -> [Corner chart (Axis, Extremum)]
rectCorners chart =
    [ ((chart, (U, AtMin)), (chart, (V, AtMin)))
    , ((chart, (U, AtMax)), (chart, (V, AtMin)))
    , ((chart, (U, AtMin)), (chart, (V, AtMax)))
    , ((chart, (U, AtMax)), (chart, (V, AtMax)))
    ]

cubeCorners :: [Corner Face (Axis, Extremum)]
cubeCorners = concatMap rectCorners [minBound .. maxBound]

squareCorners :: [Corner () (Axis, Extremum)]
squareCorners = rectCorners ()

-- | A cube's 8 vertices each have 3 faces meeting at them, so every cycle is
-- 3 and none is 4. The surface is a sphere, and a sphere cannot be flat: the
-- angle missing at each vertex is the curvature, and the eight of them are
-- where a cube keeps all of it.
cubeHasEightConePoints :: TestTree
cubeHasEightConePoints =
    testGroup
        "cube map"
        [ testCase "every vertex joins three faces" $
          assertEqual "" [3] (vertexCycleLengths cubeSeam cubeCorners)
        , testCase "all 24 corners are accounted for by 8 cycles of 3" $
          assertEqual "" 24 (length cubeCorners)
        , testCase "a cube is not flat" $
          assertEqual "" [3] (vertexCycleViolations cubeSeam cubeCorners)
        ]

-- | The two single-chart surfaces are told apart by their orientation bits
-- alone: same chart, same four half-edges, same pairing, and the projective
-- plane's antipodal identification still splits the four corners into two
-- vertices where the Klein bottle's leaves one.
singleChartSurfaces :: TestTree
singleChartSurfaces =
    testGroup
        "single-chart surfaces"
        [ testCase "a Klein bottle has one four-corner vertex" $
          assertEqual "" [4] (vertexCycleLengths kleinSeam squareCorners)
        , testCase "a Klein bottle is flat" $
          assertEqual "" [] (vertexCycleViolations kleinSeam squareCorners)
        , testCase "a projective plane has two two-corner vertices" $
          assertEqual "" [2] (vertexCycleLengths projectiveSeam squareCorners)
        , testCase "a projective plane is not flat" $
          assertEqual "" [2] (vertexCycleViolations projectiveSeam squareCorners)
        ]

vertexCycleTests :: TestTree
vertexCycleTests =
    testGroup
        "Data.Atlas.Topology.Seam vertex cycles, on the shipped atlases"
        [cubeHasEightConePoints, singleChartSurfaces]
