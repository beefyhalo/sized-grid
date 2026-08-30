{-# LANGUAGE DataKinds #-}

-- | What a step does to a walker's /frame/ --- which way its left hand points
-- --- which is the part neither the landing coordinate nor the new heading
-- says, and which 'Data.Grid.Atlas.Rect.rectStep' now hands back as a
-- 'Crossing'.
--
-- One module rather than a paragraph in each surface's tests, because the
-- interesting statements are comparisons /between/ the surfaces. All four are
-- built from the same 'rectStep' and the same shape of table, and the only
-- thing that separates them is which seams reverse and which do not. That
-- difference is precisely what these tests read out:
--
--   * a cube map is orientable, so no crossing anywhere on it mirrors a
--     walker, and a walk that closes brings the walker back as it was;
--   * a Mobius strip is not, and the middle row of an odd-height strip is the
--     shortest proof: a lap along it closes --- same cell, same heading ---
--     with exactly one mirroring;
--   * a Klein bottle has one seam of each kind, which is what makes it a
--     bottle and not a torus;
--   * a projective plane has two, which is what makes it not a bottle.
--
-- None of that is visible in a position or a heading. Before
-- sized-grid-lopy.5 none of it was visible at all: the bit was computed in
-- 'rectStep' and dropped by every layer above it.
module Test.Frames
  ( frameTests,
  )
where

import Data.Grid.Atlas (AtlasCoord)
import Data.Grid.Atlas.CubeMap qualified as Cube
import Data.Grid.Atlas.Klein qualified as Klein
import Data.Grid.Atlas.Mobius qualified as Mobius
import Data.Grid.Atlas.Projective qualified as Proj
import Data.Grid.Atlas.Rect
  ( Crossing (..),
    Heading (..),
    crossedSeam,
    reversedFrame,
  )
import Data.Grid.Sized
import Test.Tasty
import Test.Tasty.HUnit

-- * A cube map: orientable, and every crossing says so

type N = 5

cubeCell :: Int -> Int -> Cube.Face -> AtlasCoord '[Ordinal N, Ordinal N] 6
cubeCell u v face =
  (Cube.faceIndex face, unsafeOrdinal u :| unsafeOrdinal v :| EmptyCoord)

cubeCrossing :: Cube.Face -> Int -> Int -> Heading -> Crossing
cubeCrossing face u v heading =
  let (_, _, crossing) = Cube.cubeStep (cubeCell u v face) heading
   in crossing

-- | The cell sitting against the given edge of a face, @free@ cells along it.
onEdge :: Heading -> Int -> (Int, Int)
onEdge (Heading ax side) free =
  case ax of
    Cube.U -> (fixed, free)
    Cube.V -> (free, fixed)
  where
    fixed =
      case side of
        AtMin -> 0
        AtMax -> ordinalSize @N - 1

cubeEdges :: [(Cube.Face, Heading, Int)]
cubeEdges =
  [ (face, Heading ax side, free)
  | face <- [minBound .. maxBound],
    ax <- [minBound .. maxBound],
    side <- [minBound .. maxBound],
    free <- [0 .. ordinalSize @N - 1]
  ]

cubeNeverMirrors :: TestTree
cubeNeverMirrors =
  testGroup
    "a cube map is orientable"
    [ testCase "no crossing on a cube reverses a walker's frame" $
        assertEqual
          "crossings that mirrored"
          []
          [ (face, heading, free)
          | (face, heading, free) <- cubeEdges,
            let (u, v) = onEdge heading free,
            reversedFrame (cubeCrossing face u v heading)
          ],
      testCase "and every one of them is still a crossing" $
        assertEqual
          "steps off an edge that did not report a seam"
          []
          [ (face, heading, free)
          | (face, heading, free) <- cubeEdges,
            let (u, v) = onEdge heading free,
            not (crossedSeam (cubeCrossing face u v heading))
          ],
      testCase "a step that stays on its face reports Interior" $
        assertEqual
          "interior steps that claimed a seam"
          []
          [ (face, u, v, heading)
          | face <- [minBound .. maxBound],
            u <- [1 .. ordinalSize @N - 2],
            v <- [1 .. ordinalSize @N - 2],
            ax <- [minBound .. maxBound],
            side <- [minBound .. maxBound],
            let heading = Heading ax side,
            cubeCrossing face u v heading /= Interior
          ],
      testCase "and a walk that closes brings the walker back as it was" $
        assertEqual
          "closed walks that mirrored the walker"
          []
          [ (face, u, v, heading)
          | face <- [minBound .. maxBound],
            u <- [0 .. ordinalSize @N - 1],
            v <- [0 .. ordinalSize @N - 1],
            ax <- [minBound .. maxBound],
            side <- [minBound .. maxBound],
            let heading = Heading ax side,
            let walk = cubeWalk (cubeCell u v face) heading (4 * ordinalSize @N),
            odd (length (filter reversedFrame walk))
          ]
    ]

cubeWalk ::
  AtlasCoord '[Ordinal N, Ordinal N] 6 -> Heading -> Int -> [Crossing]
cubeWalk _ _ 0 = []
cubeWalk c heading n =
  let (c', heading', crossing) = Cube.cubeStep c heading
   in crossing : cubeWalk c' heading' (n - 1)

-- * A Mobius strip: not orientable, and the middle row is the proof

-- | Odd height, so that the middle row is the one the seam's mirror fixes ---
-- the shortest closed walk on the surface that comes back reversed.
type W = 5

type H = 3

stripCell :: Int -> Int -> AtlasCoord '[Clamped W, Clamped H] 1
stripCell u v =
  ( minBound,
    Clamped (unsafeOrdinal u) :| Clamped (unsafeOrdinal v) :| EmptyCoord
  )

mobiusWalk ::
  AtlasCoord '[Clamped W, Clamped H] 1 ->
  Heading ->
  Int ->
  Maybe [Crossing]
mobiusWalk _ _ 0 = Just []
mobiusWalk c heading n = do
  (c', heading', crossing) <- Mobius.mobiusStep c heading
  (crossing :) <$> mobiusWalk c' heading' (n - 1)

mobiusMirrors :: TestTree
mobiusMirrors =
  testGroup
    "a Mobius strip is not orientable"
    [ testCase "every crossing of the wrapped seam reverses the frame" $
        assertEqual
          "wrapped crossings that did not mirror"
          []
          [ (v, side)
          | v <- [0 .. ordinalSize @H - 1],
            side <- [minBound .. maxBound],
            let u =
                  case side of
                    AtMin -> 0
                    AtMax -> ordinalSize @W - 1,
            let heading = Heading Mobius.Wrapped side,
            Just (_, _, crossing) <- [Mobius.mobiusStep (stripCell u v) heading],
            not (reversedFrame crossing)
          ],
      testCase "a step that stays on the chart reports Interior" $
        assertEqual
          "interior steps that claimed a seam"
          []
          [ (u, v, heading)
          | u <- [1 .. ordinalSize @W - 2],
            v <- [1 .. ordinalSize @H - 2],
            ax <- [minBound .. maxBound],
            side <- [minBound .. maxBound],
            let heading = Heading ax side,
            Just (_, _, crossing) <- [Mobius.mobiusStep (stripCell u v) heading],
            crossing /= Interior
          ],
      testCase "one lap of the middle row closes, and mirrors exactly once" $ do
        let mid = (ordinalSize @H - 1) `div` 2
            heading = Heading Mobius.Wrapped AtMax
            start = (stripCell 0 mid, heading)
            stepped (c, hd) =
              case Mobius.mobiusStep c hd of
                Just (c', hd', _) -> (c', hd')
                Nothing -> error "the wrapped axis has no edge"
        assertEqual
          "the lap closes"
          start
          (iterate stepped start !! ordinalSize @W)
        assertEqual
          "mirrorings in one lap"
          (Just 1)
          ( length . filter reversedFrame
              <$> mobiusWalk (stripCell 0 mid) heading (ordinalSize @W)
          )
        assertEqual
          "and in two"
          (Just 2)
          ( length . filter reversedFrame
              <$> mobiusWalk (stripCell 0 mid) heading (2 * ordinalSize @W)
          )
    ]

-- * A Klein bottle: one seam of each kind

kleinCrossing :: Int -> Int -> Heading -> Crossing
kleinCrossing u v heading =
  let (_, _, crossing) = Klein.kleinStep (stripCell u v) heading
   in crossing

kleinHasOneOfEach :: TestTree
kleinHasOneOfEach =
  testGroup
    "a Klein bottle glues one axis with a twist and one without"
    [ testCase "crossing Twisted reverses the frame" $
        assertEqual
          "twisted crossings that did not mirror"
          []
          [ (v, side)
          | v <- [0 .. ordinalSize @H - 1],
            side <- [minBound .. maxBound],
            let u = endOf side (ordinalSize @W),
            not (reversedFrame (kleinCrossing u v (Heading Klein.Twisted side)))
          ],
      testCase "crossing Rolled does not" $
        assertEqual
          "rolled crossings that mirrored"
          []
          [ (u, side)
          | u <- [0 .. ordinalSize @W - 1],
            side <- [minBound .. maxBound],
            let v = endOf side (ordinalSize @H),
            reversedFrame (kleinCrossing u v (Heading Klein.Rolled side))
          ],
      testCase "but both are crossings" $
        assertEqual
          "edge steps that reported Interior"
          []
          [ (u, v, ax, side)
          | (ax, u, v, side) <-
              [ (Klein.Twisted, endOf side (ordinalSize @W), v, side)
              | v <- [0 .. ordinalSize @H - 1],
                side <- [minBound .. maxBound]
              ]
                ++ [ (Klein.Rolled, u, endOf side (ordinalSize @H), side)
                   | u <- [0 .. ordinalSize @W - 1],
                     side <- [minBound .. maxBound]
                   ],
            not (crossedSeam (kleinCrossing u v (Heading ax side)))
          ]
    ]

-- * A projective plane: two of each

projectiveCrossing :: Int -> Int -> Heading -> Crossing
projectiveCrossing u v heading =
  let (_, _, crossing) = Proj.projectiveStep (stripCell u v) heading
   in crossing

projectiveMirrorsBoth :: TestTree
projectiveMirrorsBoth =
  testCase "every crossing of a projective plane reverses the frame" $
    assertEqual
      "crossings that did not mirror"
      []
      ( [ (v, side, "horizontal")
        | v <- [0 .. ordinalSize @H - 1],
          side <- [minBound .. maxBound],
          let u = endOf side (ordinalSize @W),
          not (reversedFrame (projectiveCrossing u v (Heading Proj.Horizontal side)))
        ]
          ++ [ (u, side, "vertical")
             | u <- [0 .. ordinalSize @W - 1],
               side <- [minBound .. maxBound],
               let v = endOf side (ordinalSize @H),
               not (reversedFrame (projectiveCrossing u v (Heading Proj.Vertical side)))
             ]
      )

endOf :: Extremum -> Int -> Int
endOf AtMin _ = 0
endOf AtMax size = size - 1

frameTests :: TestTree
frameTests =
  testGroup
    "what a crossing does to a walker's frame"
    [cubeNeverMirrors, mobiusMirrors, kleinHasOneOfEach, projectiveMirrorsBoth]
