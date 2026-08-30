{-# LANGUAGE DataKinds #-}

-- | The rules, and in particular the push, which is the only place the
-- surface's twist can be got wrong.
module Test.Rules
  ( ruleTests,
  )
where

import Data.Maybe (fromMaybe, isNothing)
import Data.Set qualified as Set
import Sokoban.Board
import Sokoban.Level
import Sokoban.Rules
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

-- | A board from a picture, so a test states its case as the case rather than
-- as grid-building code. Fixed at the one size every test here uses; a picture
-- of the wrong shape fails now rather than somewhere confusing later.
level :: [String] -> Level 6 5
level rows =
  case parseLevelAt (unlines rows) of
    Left err -> error ("test level: " ++ err)
    Right l -> l

-- | A crate set as columns and rows, which is what a test can read.
crateCells :: Game 6 5 -> [(Int, Int)]
crateCells g = [spotXY (minBound, c) | c <- Set.toList (playCrates (gamePlay g))]

crateGoesAround :: TestTree
crateGoesAround =
  testCase "a crate pushed a full lap arrives in the mirrored row" $ do
    let lvl =
          level
            [ "------",
              "------",
              "-@$--.",
              "------",
              "------"
            ]
        w = 6
        (afterLap, outs) = replay ChartFrame (replicate w DirRight) (newGame lvl)
        (afterTwo, _) =
          replay ChartFrame (replicate (2 * w) DirRight) (newGame lvl)
    assertEqual "every key press pushed" (replicate w Pushed) outs
    -- The crate started at column 2 of the middle row of a five-row strip,
    -- which is the row the mirror fixes, so a lap brings it back to
    -- exactly where it was.
    assertEqual
      "crate is back at its own cell"
      (playCrates (levelStart lvl))
      (playCrates (gamePlay afterLap))
    assertEqual
      "and so is the player"
      (playPlayer (levelStart lvl))
      (playPlayer (gamePlay afterLap))
    assertBool "one lap leaves the player upside down" $
      playFlipped (gamePlay afterLap)
    assertBool "two laps put the player back up" $
      not (playFlipped (gamePlay afterTwo))

crateOffTheMiddleRow :: TestTree
crateOffTheMiddleRow =
  testCase "a lap off the middle row lands in the row the mirror sends it to" $ do
    let lvl =
          level
            [ "------",
              "-@$---",
              "------",
              "------",
              "-----."
            ]
        (afterLap, _) = replay ChartFrame (replicate 6 DirRight) (newGame lvl)
        (afterTwo, _) = replay ChartFrame (replicate 12 DirRight) (newGame lvl)
    -- The crate starts at (2, 3): row 3 of a five-row strip, which the
    -- mirror sends to row 1.
    assertEqual "crate is in the mirrored row" [(2, 1)] (crateCells afterLap)
    assertEqual "and home again after two" [(2, 3)] (crateCells afterTwo)

-- | The case the whole game turns on: the player and the crate are on
-- opposite sides of the seam, so their rows are mirror images and no
-- displacement added to the player's cell names the crate's.
straddlingTheSeam :: TestTree
straddlingTheSeam =
  testCase "a push with the pair straddling the seam moves both" $ do
    let lvl =
          level
            [ "------",
              "-@---$",
              "------",
              "------",
              ".-----"
            ]
        -- Walk the player right to column 4, then push.
        (g, _) = replay ChartFrame (replicate 3 DirRight) (newGame lvl)
        (g', o) = move ChartFrame DirRight g
        play = gamePlay g'
    assertEqual "the crate went through the seam" Pushed o
    assertEqual
      "player stepped up to where the crate was"
      (5, 3)
      (spotXY (playPlayer play))
    assertEqual
      "crate came out on the other side, in the mirrored row"
      [(0, 1)]
      (crateCells g')
    assertBool "the player has not crossed yet" $ not (playFlipped play)
    -- And the next push moves the player through, onto the cell the crate
    -- has just left.
    let (g'', o') = move ChartFrame DirRight g'
        play' = gamePlay g''
    assertEqual "still pushing" Pushed o'
    assertEqual "player is through" (0, 1) (spotXY (playPlayer play'))
    assertEqual
      "crate is one further along the mirrored row"
      [(1, 1)]
      (crateCells g'')
    assertBool "and now the player is upside down" (playFlipped play')

refusals :: TestTree
refusals =
  testGroup
    "the three ways a move is refused"
    [ testCase "stepping off the straight edge" $
        assertEqual
          "should be the edge of the strip"
          OffTheStrip
          (snd (move ChartFrame DirDown (newGame lvl))),
      testCase "walking into a wall" $
        assertEqual
          "should be a wall"
          BlockedByWall
          (snd (move ChartFrame DirUp (newGame lvl))),
      testCase "pushing a crate into a wall" $
        assertEqual
          "should be a crate with no room"
          BlockedByCrate
          (snd (move ChartFrame DirRight (newGame lvl))),
      testCase "pushing a crate off the straight edge" $
        assertEqual
          "should be a crate with no room"
          BlockedByCrate
          (snd (move ChartFrame DirDown (newGame edgeLvl))),
      testCase "a refused move changes nothing" $
        assertEqual
          "the play is untouched"
          (levelStart lvl)
          (gamePlay (fst (move ChartFrame DirUp (newGame lvl))))
    ]
  where
    lvl =
      level
        [ "------",
          "------",
          "------",
          "#-----",
          "@$#--."
        ]
    edgeLvl =
      level
        [ "------",
          "------",
          "-----.",
          "-@----",
          "-$----"
        ]

undoRestores :: TestTree
undoRestores =
  testGroup
    "undo"
    [ testCase "at the start there is nothing to take back" $
        assertBool "should be Nothing" (isNothing (undo (newGame lvl))),
      testProperty "any run of moves, undone, is the start again" $
        forAll (listOf (elements allDirs)) $ \dirs ->
          let g0 = newGame lvl
              (g, _) = replay ChartFrame dirs g0
              back = undoAll g
           in playOf back === playOf g0
    ]
  where
    lvl =
      level
        [ "------",
          "--..--",
          "-@$$--",
          "------",
          "------"
        ]
    playOf = gamePlay
    undoAll g = maybe g undoAll (undo g)

winning :: TestTree
winning =
  testCase "every goal covered is a win, and one short is not" $ do
    let lvl =
          level
            [ "------",
              "------",
              "-@$.--",
              "------",
              "------"
            ]
        g0 = newGame lvl
        (g1, _) = move ChartFrame DirRight g0
    assertBool "not solved to start with" (not (solved g0))
    assertEqual "one goal outstanding" 1 (goalsLeft g0)
    assertBool "solved after the push" (solved g1)
    assertEqual "none outstanding" 0 (goalsLeft g1)

frames :: TestTree
frames =
  testGroup
    "the two frames"
    [ testCase "chart frame ignores the player's parity" $
        assertEqual
          "up is up"
          [headingFor ChartFrame False d | d <- allDirs]
          [headingFor ChartFrame True d | d <- allDirs],
      testCase "player frame swaps up and down once flipped, and nothing else" $ do
        assertEqual
          "left and right are untouched"
          (map (headingFor PlayerFrame False) [DirLeft, DirRight])
          (map (headingFor PlayerFrame True) [DirLeft, DirRight])
        assertEqual
          "up flipped is down"
          (headingFor PlayerFrame False DirDown)
          (headingFor PlayerFrame True DirUp),
      testCase "a lap turns the player over, so the same key walks the other way" $ do
        let lvl =
              level
                [ "------",
                  "------",
                  "-@---.",
                  "--$---",
                  "------"
                ]
            g0 = newGame lvl
            -- Six rights is a lap; the middle row is fixed by the
            -- mirror, so the player is back where it started and
            -- upside down.
            (lapped, _) = replay PlayerFrame (replicate 6 DirRight) g0
        assertBool "upside down" (playFlipped (gamePlay lapped))
        assertEqual
          "same cell as the start"
          (spotXY (playPlayer (gamePlay g0)))
          (spotXY (playPlayer (gamePlay lapped)))
        let (up, _) = move PlayerFrame DirUp lapped
            (down, _) = move PlayerFrame DirUp g0
        assertEqual
          "'up' now walks the way 'up' used to walk from the other side"
          (snd (spotXY (playPlayer (gamePlay up))))
          (2 - 1)
        assertEqual
          "which is not where it went before the lap"
          (snd (spotXY (playPlayer (gamePlay down))))
          (2 + 1)
    ]

-- | Moves and pushes are counted the way a player counts them.
counting :: TestTree
counting =
  testCase "moves and pushes are counted, and undo takes them back" $ do
    let lvl =
          level
            [ "------",
              "------",
              "-@$--.",
              "------",
              "------"
            ]
        (g, _) = replay ChartFrame [DirRight, DirRight, DirDown] (newGame lvl)
        play = gamePlay g
    assertEqual "three moves" 3 (playMoves play)
    assertEqual "two of them pushes" 2 (playPushes play)
    let back = foldl' (\x _ -> fromMaybe x (undo x)) g [1 :: Int .. 3]
    assertEqual "and back to nothing" (levelStart lvl) (gamePlay back)

ruleTests :: TestTree
ruleTests =
  testGroup
    "the rules"
    [ crateGoesAround,
      crateOffTheMiddleRow,
      straddlingTheSeam,
      refusals,
      undoRestores,
      winning,
      frames,
      counting
    ]
