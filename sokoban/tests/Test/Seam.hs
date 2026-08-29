{-# LANGUAGE DataKinds #-}

-- | What the surface does, before any Sokoban is played on it.
--
-- These are the facts the rules are entitled to assume, and they are asserted
-- against arithmetic written out here rather than against the library, so a
-- change in @Data.Grid.Atlas.Mobius@ that changed the shape of the strip would
-- fail here instead of quietly changing every level.
module Test.Seam
  ( seamTests
  ) where

import           Sokoban.Board

import           Data.Grid.Atlas.Mobius
import           Data.Grid.Sized

import           Data.Maybe             (fromJust, isNothing)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck  as QC

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
        Nothing        -> error ("walk: off the strip at " ++ show (spotXY s))
        Just (s', hd') -> walk (n - 1) s' hd'

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
        (snd <$> stepSpot (spot 5 1) (Heading Wrapped AtMax))

straightAxisHasARealEdge :: TestTree
straightAxisHasARealEdge =
    testGroup
        "the straight axis is a genuine edge"
        [ testCase "off the top" $
          assertBool "should be Nothing" $
          isNothing (stepSpot (spot 2 4) (Heading Straight AtMax))
        , testCase "off the bottom" $
          assertBool "should be Nothing" $
          isNothing (stepSpot (spot 2 0) (Heading Straight AtMin))
        ]

crossesSeamAgrees :: TestTree
crossesSeamAgrees =
    testProperty "crossesSeam agrees with what the step actually did" $
    forAll ((,) <$> choose (0, 5) <*> choose (0, 4)) $ \(x, y) ->
        conjoin
            [ counterexample (show (x, y, side)) $
            let here = spot x y
                hd = Heading Wrapped side
                -- A crossing is the step whose landing row is not the row it
                -- left, or, on the row the mirror fixes, the step whose column
                -- did not move by one.
                reallyCrossed there =
                    snd (spotXY there) /= y ||
                    abs (fst (spotXY there) - x) /= 1
            in case stepSpot here hd of
                   Nothing -> counterexample "a sideways step is never off the strip" False
                   Just (there, _) -> crossesSeam here hd === reallyCrossed there
            | side <- [AtMin, AtMax]
            ]

-- | A step along the straight axis never touches the seam, which is why the
-- player's parity only ever changes on a sideways move.
straightNeverCrosses :: TestTree
straightNeverCrosses =
    testCase "a step across the strip never crosses the seam" $
    assertEqual
        "cells reporting a crossing"
        []
        [ (x, y, side)
        | x <- [0 .. 5]
        , y <- [0 .. 4]
        , side <- [AtMin, AtMax]
        , crossesSeam (spot x y) (Heading Straight side)
        ]

seamTests :: TestTree
seamTests =
    testGroup
        "the surface"
        [ oneLapMirrors
        , middleRowClosesInOne
        , headingSurvivesTheSeam
        , straightAxisHasARealEdge
        , crossesSeamAgrees
        , straightNeverCrosses
        ]
