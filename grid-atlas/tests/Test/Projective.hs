{-# LANGUAGE DataKinds #-}

module Test.Projective
  ( projectiveTests,
  )
where

import Data.Atlas.Topology.Seam (HalfEdge, seamViolations)
import Data.Grid.Atlas (AtlasCoord)
import Data.Grid.Atlas.Projective
import Data.Grid.Sized
import Test.Tasty
import Test.Tasty.HUnit

allAxes :: [Axis]
allAxes = [minBound .. maxBound]

allExtrema :: [Extremum]
allExtrema = [minBound .. maxBound]

projectiveHalfEdges :: [HalfEdge () (Axis, Extremum)]
projectiveHalfEdges =
  [((), (edgeAxis, side)) | edgeAxis <- allAxes, side <- allExtrema]

projectiveSeamPairsUp :: TestTree
projectiveSeamPairsUp =
  testCase "projectiveSeam pairs every half-edge with one that points back" $
    assertEqual
      "half-edges whose partner does not point back"
      []
      (seamViolations projectiveSeam projectiveHalfEdges)

type W = 5

type H = 3

w :: Int
w = ordinalSize @W

h :: Int
h = ordinalSize @H

at :: Int -> Int -> AtlasCoord '[Clamped W, Clamped H] 1
at horizontal vertical =
  ( minBound,
    Clamped (unsafeOrdinal horizontal)
      :| Clamped (unsafeOrdinal vertical)
      :| EmptyCoord
  )

-- | 'projectiveStep' with its 'Crossing' dropped; see Test.Frames for the
-- tests that are about the crossing.
step ::
  (AtlasCoord '[Clamped W, Clamped H] 1, Heading) ->
  (AtlasCoord '[Clamped W, Clamped H] 1, Heading)
step (c, heading) =
  let (c', heading', _) = projectiveStep c heading
   in (c', heading')

lap :: Int -> Heading -> (Int, Int) -> (AtlasCoord '[Clamped W, Clamped H] 1, Heading)
lap count heading (horizontal, vertical) =
  iterate step (at horizontal vertical, heading) !! count

projectiveHorizontalLapMirrors :: TestTree
projectiveHorizontalLapMirrors =
  testCase "projectiveStep crossing Horizontal mirrors the vertical coordinate" $
    mapM_
      ( \(horizontal, vertical) ->
          let heading = Heading Horizontal AtMax
           in do
                assertEqual
                  (show (horizontal, vertical) ++ ": after one lap")
                  (at horizontal (h - 1 - vertical), heading)
                  (lap w heading (horizontal, vertical))
                assertEqual
                  (show (horizontal, vertical) ++ ": after two laps")
                  (at horizontal vertical, heading)
                  (lap (2 * w) heading (horizontal, vertical))
      )
      [(horizontal, vertical) | horizontal <- [0 .. w - 1], vertical <- [0 .. h - 1]]

projectiveVerticalLapMirrors :: TestTree
projectiveVerticalLapMirrors =
  testCase "projectiveStep crossing Vertical mirrors the horizontal coordinate" $
    mapM_
      ( \(horizontal, vertical) ->
          let heading = Heading Vertical AtMax
           in do
                assertEqual
                  (show (horizontal, vertical) ++ ": after one lap")
                  (at (w - 1 - horizontal) vertical, heading)
                  (lap h heading (horizontal, vertical))
                assertEqual
                  (show (horizontal, vertical) ++ ": after two laps")
                  (at horizontal vertical, heading)
                  (lap (2 * h) heading (horizontal, vertical))
      )
      [(horizontal, vertical) | horizontal <- [0 .. w - 1], vertical <- [0 .. h - 1]]

projectiveTests :: TestTree
projectiveTests =
  testGroup
    "Data.Grid.Atlas.Projective"
    [ projectiveSeamPairsUp,
      projectiveHorizontalLapMirrors,
      projectiveVerticalLapMirrors
    ]
