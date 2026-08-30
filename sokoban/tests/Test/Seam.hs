{-# LANGUAGE DataKinds #-}

-- | What the surface does, before any Sokoban is played on it.
--
-- These are the facts the rules are entitled to assume, and they are asserted
-- against arithmetic written out here rather than against the library, so a
-- change in @Data.Grid.Atlas.Mobius@ that changed the shape of the strip would
-- fail here instead of quietly changing every level.
module Test.Seam
  ( seamTests,
  )
where

import Data.Grid.Atlas.Mobius
import Data.Grid.Sized
import Data.Maybe (fromJust, isNothing)
import Sokoban.Board
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

type W = 6

type H = 5

spot :: Int -> Int -> Spot W H
spot x y = fromJust (spotAt x y)

-- | Walk, taking each step's landing heading into the next step, which is how
-- "Sokoban.Rules" walks.
walk :: Int -> Spot W H -> Heading -> (Spot W H, Heading)
walk 0 s hd = (s, hd)
walk n s hd =
  case stepSpot s hd of
    Nothing -> error ("walk: off the strip at " ++ show (spotXY s))
    Just (s', hd', _) -> walk (n - 1) s' hd'

oneLapMirrors :: TestTree
oneLapMirrors =
  testProperty "one lap around lands in the mirrored row, two laps land home" $
    forAll ((,) <$> choose (0, 5) <*> choose (0, 4)) $ \(x, y) ->
      let w = ordinalSize @W
          h = ordinalSize @H
          start = spot x y
          right = Heading Wrapped AtMax
          (afterOne, _) = walk w start right
          (afterTwo, _) = walk (2 * w) start right
       in spotXY afterOne === (x, h - 1 - y) .&&. spotXY afterTwo === (x, y)

middleRowClosesInOne :: TestTree
middleRowClosesInOne =
  testCase "the middle row of an odd strip is the one lap that closes" $
    let mid = (ordinalSize @H - 1) `div` 2
        (landed, _) = walk (ordinalSize @W) (spot 0 mid) (Heading Wrapped AtMax)
     in assertEqual "back where it started" (0, mid) (spotXY landed)

headingSurvivesTheSeam :: TestTree
headingSurvivesTheSeam =
  testCase "crossing the seam does not change the heading that crossed it" $
    assertEqual
      "still heading the way it was"
      (Just (Heading Wrapped AtMax))
      ((\(_, hd, _) -> hd) <$> stepSpot (spot 5 1) (Heading Wrapped AtMax))

straightAxisHasARealEdge :: TestTree
straightAxisHasARealEdge =
  testGroup
    "the straight axis is a genuine edge"
    [ testCase "off the top" $
        assertBool "should be Nothing" $
          isNothing (stepSpot (spot 2 4) (Heading Straight AtMax)),
      testCase "off the bottom" $
        assertBool "should be Nothing" $
          isNothing (stepSpot (spot 2 0) (Heading Straight AtMin))
    ]

-- | The bit the surface hands back, and what it means.
--
-- Every sideways step off an end of the strip is a crossing that reverses the
-- walker's frame; every step that stays inside the chart is 'Interior'; and a
-- step across the strip never reaches the seam at all, which is why only
-- sideways moves change the player's parity.
--
-- Before sized-grid-lopy.5 this module reconstructed all of that from a bounds
-- test against the strip's width. It agreed with the surface, which was the
-- trouble: it agreed by accident, because on a Mobius strip every crossing
-- happens to mirror, and the same reconstruction on a cube map would have been
-- wrong in every case.
crossingsSayWhatTheyDid :: TestTree
crossingsSayWhatTheyDid =
  testGroup
    "what a step reports"
    [ testCase "a sideways step off either end mirrors the walker" $
        assertEqual
          "sideways edge steps that did not mirror"
          []
          [ (x, y, side)
          | y <- [0 .. 4],
            side <- [AtMin, AtMax],
            let x =
                  case side of
                    AtMin -> 0
                    AtMax -> 5,
            Just (_, _, crossing) <- [stepSpot (spot x y) (Heading Wrapped side)],
            not (reversedFrame crossing)
          ],
      testCase "a step that stays on the chart is Interior" $
        assertEqual
          "interior steps that claimed a crossing"
          []
          [ (x, y, ax, side)
          | x <- [1 .. 4],
            y <- [1 .. 3],
            ax <- [Wrapped, Straight],
            side <- [AtMin, AtMax],
            Just (_, _, crossing) <- [stepSpot (spot x y) (Heading ax side)],
            crossing /= Interior
          ],
      testProperty "only a sideways step can reach the seam" $
        forAll ((,) <$> choose (0, 5) <*> choose (0, 4)) $ \(x, y) ->
          conjoin
            [ counterexample (show (x, y, side)) $
                case stepSpot (spot x y) (Heading Straight side) of
                  -- Off the top or the bottom: the strip's real edge.
                  Nothing -> property True
                  Just (_, _, crossing) -> crossing === Interior
            | side <- [AtMin, AtMax]
            ],
      testCase "a mirrored crossing is still a crossing" $
        assertBool "crossedSeam should hold wherever reversedFrame does" $
          and
            [ crossedSeam c
            | c <- [minBound .. maxBound],
              reversedFrame c
            ]
    ]

seamTests :: TestTree
seamTests =
  testGroup
    "the surface"
    [ oneLapMirrors,
      middleRowClosesInOne,
      headingSurvivesTheSeam,
      straightAxisHasARealEdge,
      crossingsSayWhatTheyDid
    ]
