{-# LANGUAGE DataKinds #-}

-- | What the three surfaces do, and what they do differently.
--
-- "Test.Seam" asserts the Mobius strip's own facts against arithmetic written
-- out by hand. These are the ones that only exist because there is more than
-- one surface (sized-grid-lopy.7): the claims the game makes about all of them
-- at once, and the places where one of them is not like the others.
module Test.Surface
  ( surfaceTests,
  )
where

import Data.Grid.Sized (ordinalSize)
import Data.Maybe (fromJust, isJust, isNothing)
import Sokoban.Board
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

type W = 6

type H = 5

around, across :: Int
around = ordinalSize @W
across = ordinalSize @H

spot :: (Int, Int) -> Spot W H
spot (x, y) = fromJust (spotAt x y)

-- | Walk from a cell, in the chart's frame, and say where it got to and how it
-- is standing.
from :: Surface -> (Int, Int) -> [Dir] -> ((Int, Int), Turn)
from surface start dirs =
  case walkFrom surface ChartFrame (spot start, square) dirs of
    Nothing -> error ("left the surface walking on a " ++ surfaceName surface)
    Just (s, t) -> (spotXY s, t)

everyCell :: Gen (Int, Int)
everyCell = (,) <$> choose (0, around - 1) <*> choose (0, across - 1)

named :: (Surface -> TestTree) -> [TestTree]
named f = map f surfaces

-- | Only one of the three has anywhere to fall off, and it is the one that
-- says so.
--
-- The Klein bottle is the interesting half: with no partiality anywhere,
-- 'Sokoban.Rules.OffTheEdge' stops being reachable, and the rules that
-- distinguish it from a wall become dead code rather than wrong code. That is
-- what the abstraction had to survive.
edgesAreWhereTheSurfaceSaysTheyAre :: TestTree
edgesAreWhereTheSurfaceSaysTheyAre =
  testGroup
    "a step is refused exactly where the surface has an edge"
    ( named $ \surface ->
        testProperty (surfaceName surface) $
          forAll ((,) <$> everyCell <*> elements allDirs) $ \(cell, dir) ->
            let stepped =
                  walkFrom surface ChartFrame (spot cell, square) [dir]
                offTheTop = dir == DirUp && snd cell == across - 1
                offTheBottom = dir == DirDown && snd cell == 0
                leaves = surfaceEdged surface && (offTheTop || offTheBottom)
             in isNothing stepped === leaves
    )

-- | Two laps close, on every surface and along every axis that is glued at
-- all. One lap does not, wherever the seam it crosses has a turn in it, which
-- is what 'oneLapTellsThemApart' is about.
twoLapsComeHome :: TestTree
twoLapsComeHome =
  testGroup
    "two laps come home"
    ( named
        ( \surface ->
            testProperty ("sideways, on a " ++ surfaceName surface) $
              forAll everyCell $ \cell ->
                fst (from surface cell (replicate (2 * around) DirRight)) === cell
        )
        ++ named
          ( \surface ->
              testProperty ("upwards, on a " ++ surfaceName surface) $
                -- An edged surface has no lap to take this way: the assertion
                -- is that walking off it is refused, which
                -- 'edgesAreWhereTheSurfaceSaysTheyAre' makes, and there is
                -- nothing left for this one to say.
                surfaceEdged surface
                  QC..||. forAll
                    everyCell
                    ( \cell ->
                        fst (from surface cell (replicate (2 * across) DirUp)) === cell
                    )
          )
    )

-- | And what a single lap does is the difference between the three.
--
-- Written out as the three answers rather than derived, because deriving them
-- from the seam tables is what "Sokoban.Board" already does: this is the
-- statement of what those tables were chosen to mean.
oneLapTellsThemApart :: TestTree
oneLapTellsThemApart =
  testGroup
    "one lap is what tells the surfaces apart"
    [ testCase "a Mobius strip mirrors the row it wraps into" $
        assertEqual "mirrored row" (2, across - 1 - 1) (lapRight mobius (2, 1)),
      testCase "so does a Klein bottle" $
        assertEqual "mirrored row" (2, across - 1 - 1) (lapRight klein (2, 1)),
      testCase "and so does a projective plane" $
        assertEqual "mirrored row" (2, across - 1 - 1) (lapRight projective (2, 1)),
      testCase "a Klein bottle's other seam glues straight through" $
        assertEqual "same column" (2, 1) (lapUp klein (2, 1)),
      testCase "a projective plane's mirrors the column" $
        assertEqual "mirrored column" (around - 1 - 2, 1) (lapUp projective (2, 1)),
      testCase "a Mobius strip has no other seam to go through" $
        assertBool "should run out" $
          isNothing (walkFrom mobius ChartFrame (spot (2, 1), square) (replicate across DirUp))
    ]
  where
    lapRight surface cell = fst (from surface cell (replicate around DirRight))
    lapUp surface cell = fst (from surface cell (replicate across DirUp))

-- | The reason the walker's frame is two bits and not one.
--
-- A single parity --- @reversedFrame@ accumulated with @xor@, which is what
-- this game carried until sized-grid-lopy.7 --- says that the walker has been
-- turned over. It cannot say which way round, and on a projective plane the
-- two ways round are different: a lap sideways swaps the player's up with
-- their down, and a lap upwards swaps their left with their right. Both are
-- one mirrored crossing, so both set the same bit.
--
-- Do both and the player is back to an even number of reflections, so the
-- parity is clear again --- and they are standing rotated by a half turn, not
-- standing the way they started. Only the pair says so.
theFrameNeedsBothBits :: TestTree
theFrameNeedsBothBits =
  testGroup
    "the walker's frame is two bits, and a projective plane uses both"
    [ testCase "a lap sideways swaps up with down" $
        assertEqual "only turnV" (Turn False True) (turnAfterLap around DirRight),
      testCase "a lap upwards swaps left with right" $
        assertEqual "only turnU" (Turn True False) (turnAfterLap across DirUp),
      testCase "both laps leave the player turned right around" $
        assertEqual
          "both bits"
          (Turn True True)
          ( snd
              ( from
                  projective
                  (2, 1)
                  (replicate around DirRight ++ replicate across DirUp)
              )
          ),
      testCase "and a single parity bit could not have said which" $
        assertBool "the two laps differ only in the pair" $
          turnAfterLap around DirRight /= turnAfterLap across DirUp
    ]
  where
    turnAfterLap n dir = snd (from projective (2, 1) (replicate n dir))

-- | The claim in 'cellAround''s own comment, which nothing asserted until now:
-- in the /walker's/ frame it does not matter whether you go up first or along
-- first, because a seam that mirrors an axis mirrors the walker with it.
--
-- Only true in 'PlayerFrame'. In the chart's it is false, which is why
-- 'spotBeyond' refuses to name a corner.
neighbourhoodsDoNotCareAboutOrder :: TestTree
neighbourhoodsDoNotCareAboutOrder =
  testGroup
    "in the player's own frame, up-then-along is along-then-up"
    ( named $ \surface ->
        testProperty (surfaceName surface) $
          forAll ((,,) <$> everyCell <*> choose (-7, 7) <*> choose (-7, 7)) $
            \(cell, dx, dy) ->
              let start = (spot cell, square)
                  along = replicate (abs dx) (if dx >= 0 then DirRight else DirLeft)
                  up = replicate (abs dy) (if dy >= 0 then DirUp else DirDown)
                  one = walkFrom surface PlayerFrame start (up ++ along)
                  other = walkFrom surface PlayerFrame start (along ++ up)
               in fmap (spotXY . fst) one === fmap (spotXY . fst) other
    )

-- | 'dirOf' undoes 'headingFor', in every frame, for every way a player can be
-- standing. The two are one involution read from opposite ends and this is the
-- assertion that keeps them so.
readingKeysBothWaysAgrees :: TestTree
readingKeysBothWaysAgrees =
  testProperty "dirOf undoes headingFor" $
    forAll ((,,) <$> elements [ChartFrame, PlayerFrame] <*> anyTurn <*> elements allDirs) $
      \(frame, t, dir) -> dirOf frame t (headingFor frame t dir) === dir
  where
    anyTurn = Turn <$> arbitrary <*> arbitrary

-- | Where a view draws the picture carrying on past an edge, and where it
-- refuses to.
--
-- The corner is the point. Two walks reach it and on a surface with a turn in
-- it they disagree, so there is no cell to draw and 'spotBeyond' says so.
theContinuationIsWhereTheSurfaceIs :: TestTree
theContinuationIsWhereTheSurfaceIs =
  testGroup
    "the picture carries on exactly where the surface does"
    [ testCase "past a Mobius strip's sides, but not its top" $ do
        assertBool "sideways continues" (isJust (spotBeyond @W @H mobius (around, 2)))
        assertBool "upwards does not" (isNothing (spotBeyond @W @H mobius (2, across))),
      testCase "past every side of a Klein bottle" $
        assertBool "all four" $
          all
            (isJust . spotBeyond @W @H klein)
            [(around, 2), (-1, 2), (2, across), (2, -1)],
      testCase "and never past a corner" $
        assertBool "no cell to name" $
          all
            (isNothing . spotBeyond @W @H projective)
            [(around, across), (-1, -1), (around, -1), (-1, across)],
      testCase "inside the chart it is just that cell" $
        assertEqual "unchanged" (Just (3, 2)) (spotXY <$> spotBeyond @W @H klein (3, 2))
    ]

surfaceTests :: TestTree
surfaceTests =
  testGroup
    "the surfaces, and what a game written for all of them may assume"
    [ edgesAreWhereTheSurfaceSaysTheyAre,
      twoLapsComeHome,
      oneLapTellsThemApart,
      theFrameNeedsBothBits,
      neighbourhoodsDoNotCareAboutOrder,
      readingKeysBothWaysAgrees,
      theContinuationIsWhereTheSurfaceIs
    ]
