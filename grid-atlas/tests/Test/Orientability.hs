{-# LANGUAGE DataKinds #-}

module Test.Orientability
  ( orientabilityTests,
  )
where

import Data.Atlas.Topology.Seam (HalfEdge)
import Data.Grid.Atlas.CubeMap (Face, cubeSeam)
import Data.Grid.Atlas.Klein (kleinSeam)
import Data.Grid.Atlas.Klein qualified as Klein
import Data.Grid.Atlas.Mobius (mobiusSeam)
import Data.Grid.Atlas.Projective (projectiveSeam)
import Data.Grid.Atlas.Projective qualified as Projective
import Data.Grid.Atlas.Rect (Axis (..), mirroringHalfEdges, orientable)
import Data.Grid.Sized (Extremum (..), ordinalSize)
import Test.Tasty
import Test.Tasty.HUnit

allAxes :: [Axis]
allAxes = [minBound .. maxBound]

allExtrema :: [Extremum]
allExtrema = [minBound .. maxBound]

squareHalfEdges :: [HalfEdge () (Axis, Extremum)]
squareHalfEdges = [((), (axis, side)) | axis <- allAxes, side <- allExtrema]

cubeHalfEdges :: [HalfEdge Face (Axis, Extremum)]
cubeHalfEdges =
  [ (face, (axis, side))
  | face <- [minBound .. maxBound],
    axis <- allAxes,
    side <- allExtrema
  ]

singleChartOrientability :: TestTree
singleChartOrientability =
  testGroup
    "single-chart surfaces"
    [ testCase "a Mobius strip is not orientable" $
        assertBool
          "expected a mirrored seam constraint"
          (not (orientable (const (ordinalSize @5)) mobiusSeam squareHalfEdges)),
      testCase "a Klein bottle is not orientable" $
        assertBool
          "expected the twisted seam to make the constraints inconsistent"
          (not (orientable kleinAxisSize kleinSeam squareHalfEdges)),
      testCase "a projective plane is not orientable" $
        assertBool
          "expected both projective seams to reverse orientation"
          (not (orientable projectiveAxisSize projectiveSeam squareHalfEdges)),
      testCase "the mirrored incidences are visible separately" $
        assertEqual
          ""
          [((), (Klein.Twisted, AtMin)), ((), (Klein.Twisted, AtMax))]
          (mirroringHalfEdges kleinAxisSize kleinSeam squareHalfEdges)
    ]
  where
    kleinAxisSize Klein.Twisted = ordinalSize @5
    kleinAxisSize Klein.Rolled = ordinalSize @3
    projectiveAxisSize Projective.Horizontal = ordinalSize @5
    projectiveAxisSize Projective.Vertical = ordinalSize @3

cubeOrientability :: TestTree
cubeOrientability =
  testCase "a cube map is orientable" $
    assertBool
      "cube seams should admit a consistent orientation"
      (orientable (const (ordinalSize @5)) cubeSeam cubeHalfEdges)

orientabilityTests :: TestTree
orientabilityTests =
  testGroup
    "Data.Grid.Atlas.Rect orientability"
    [cubeOrientability, singleChartOrientability]
