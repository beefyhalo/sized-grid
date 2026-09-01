{-# LANGUAGE DataKinds #-}

-- | That the picture is of the surface the game is played on.
--
-- "Sokoban.Band" draws a Mobius strip from a formula and "Sokoban.Board"
-- walks one through @Data.Grid.Atlas.Mobius@, and there is nothing in either
-- that makes them agree. If they drift, the game stays correct and the picture
-- starts lying --- which is the one failure this view cannot survive, since
-- its whole claim is that the surface on screen is the surface being played
-- on.
--
-- So the assertions below are not about the formula. Each one walks the board
-- with the game's own step and asks the drawing where that cell is.
module Test.Band
  ( bandTests,
  )
where

import Data.Grid.Sized (ordinalSize)
import Data.Maybe (fromJust)
import Sokoban.Band
import Sokoban.Board
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

type W = 6

type H = 5

around, across :: Int
around = ordinalSize @W
across = ordinalSize @H

band :: Band
band = defaultBand

-- | Where the cell at @(x, y)@ is drawn: the middle of its quad.
centre :: (Int, Int) -> (Float, Float, Float)
centre (x, y) =
  bandPoint band around across (fromIntegral x + 0.5) (fromIntegral y + 0.5)

-- | Cell coordinates are the units the drawing counts in, so a lap is @w@ of
-- them, and this is the same cell drawn one lap further round.
lapOn :: (Int, Int) -> (Float, Float, Float)
lapOn (x, y) =
  bandPoint band around across (fromIntegral x + 0.5 + fromIntegral around) (fromIntegral y + 0.5)

-- | Where the /game/ says a lap from here lands. On a Mobius strip, since
-- that is the surface this parametrisation is a picture of --- 'surfaceIsBand'
-- is 'False' for the other two, and there is no band to check them against.
lapTo :: (Int, Int) -> (Int, Int)
lapTo (x, y) =
  case walkFrom mobius ChartFrame (fromJust (spotAt @W @H x y), identityFrame) (replicate around DirRight) of
    Nothing -> error ("lapTo: a lap left the strip at " ++ show (x, y))
    Just (s, _) -> spotXY s

near :: (Float, Float, Float) -> (Float, Float, Float) -> Bool
near (a, b, c) (x, y, z) = abs (a - x) < eps && abs (b - y) < eps && abs (c - z) < eps
  where
    eps = 1e-4

everyCell :: Gen (Int, Int)
everyCell = (,) <$> choose (0, around - 1) <*> choose (0, across - 1)

-- | The one that matters. Carrying the drawing one lap on arrives at the cell
-- the game's own walker arrives at, for every cell of the board.
--
-- This is what makes it the surface rather than a picture of one. If
-- 'Data.Grid.Atlas.Mobius.mobiusStep' were to stop mirroring, or mirror the
-- other axis, this fails and the parametrisation has to be changed with it.
lapAgreesWithTheWalker :: TestTree
lapAgreesWithTheWalker =
  testProperty "a lap of the drawing lands where a lap of the game lands" $
    forAll everyCell $ \cell ->
      near (lapOn cell) (centre (lapTo cell))

-- | And it is a lap of the /surface/ and not of the picture: the cell it
-- arrives at is a different cell, on the far side of the middle.
oneLapIsNotHome :: TestTree
oneLapIsNotHome =
  testProperty "a single lap only comes home in the middle row" $
    forAll everyCell $ \cell@(_, y) ->
      (lapTo cell == cell) === (y == across - 1 - y)

-- | The strip's one edge is one curve: twice round the ring and back to where
-- it started, having passed through the far side of the picture on the way.
--
-- Asserted at the two ends of the second lap rather than the first, because
-- the first lap is where the other tests live: what is new here is that the
-- second one closes.
edgeIsOneCurve :: TestTree
edgeIsOneCurve =
  testGroup
    "the strip has one edge"
    [ testCase "one lap along it arrives at the other edge" $
        assertBool "should have crossed to the far side" $
          near
            (bandPoint band around across (fromIntegral around) 0)
            (bandPoint band around across 0 (fromIntegral across)),
      testCase "two laps along it arrive back at the start" $
        assertBool "should have closed" $
          near
            (bandPoint band around across (2 * fromIntegral around) 0)
            (bandPoint band around across 0 0),
      testCase "one lap along it has not closed" $
        assertBool "should not have closed" $
          not
            ( near
                (bandPoint band around across (fromIntegral around) 0)
                (bandPoint band around across 0 0)
            )
    ]

-- | The band does not pass through its own middle at the proportions it is
-- drawn at. A ring whose radius is under half the strip's width has an inner
-- edge on the far side of the centre, which is a picture of a surface nobody
-- can read.
bandDoesNotSelfIntersect :: TestTree
bandDoesNotSelfIntersect =
  testCase "the ring is wider than the band" $
    assertBool "radius should clear half the width" $
      bandRadius band around across > fromIntegral across / 2

-- | Something is drawn, at a size a window can be asked to fit.
extentIsPositive :: TestTree
extentIsPositive =
  testProperty "every strip has a positive extent" $
    forAll ((,) <$> choose (1, 20) <*> choose (1, 12)) $ \(w, h) ->
      let (dx, dy) = bandExtent band w h
       in dx > 0 .&&. dy > 0

bandTests :: TestTree
bandTests =
  testGroup
    "the band is the surface"
    [ lapAgreesWithTheWalker,
      oneLapIsNotHome,
      edgeIsOneCurve,
      bandDoesNotSelfIntersect,
      extentIsPositive
    ]
