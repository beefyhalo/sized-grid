{-# LANGUAGE DataKinds #-}

-- | The program around the board: which screen a key leads to, and what
-- happens at the moment a level is finished.
--
-- Worth asserting because none of it is visible from a type. 'onEvent' is
-- total and pure precisely so that this file can exist --- the window's only
-- IO is the way out, and everything else about a key press is a function on
-- the state.
module Test.Shell
  ( shellTests,
  )
where

import Data.Set qualified as Set
import Graphics.Gloss.Interface.IO.Game
import Sokoban.Board
import Sokoban.Level
import Sokoban.Rules
import Sokoban.Shell
import Sokoban.Solve
import Test.Tasty
import Test.Tasty.HUnit

opened :: App
opened = newApp defaultWindow builtinLevels

press :: Key -> App -> App
press k = onEvent (EventKey k Down (Modifiers Up Up Up) (0, 0))

typed :: String -> App -> App
typed cs app = foldl (flip (press . Char)) app cs

go, esc :: App -> App
go = press (SpecialKey KeySpace)
esc = press (SpecialKey KeyEsc)

-- | Every screen there is, to press a key at.
everyScreen :: [(String, App)]
everyScreen =
  [ ("the title", opened),
    ("a level", go opened),
    ("the level screen", esc (go opened)),
    ("a finished level", finished)
  ]

-- | The first level, solved, by playing the solver's answer through the real
-- key handler. What the game does when a level is finished is only worth
-- asserting on a level that was actually finished.
finished :: App
finished = typed (map keyOf (solutionMoves answer)) (go opened)
  where
    answer :: Solution
    answer =
      case builtinLevels of
        (SomeLevel lvl : _) ->
          case solveLevel 500000 lvl of
            Just s -> s
            Nothing -> error "the first level has no solution"
        [] -> error "no levels"
    keyOf DirLeft = 'a'
    keyOf DirRight = 'd'
    keyOf DirUp = 'w'
    keyOf DirDown = 's'

opensOnTheTitle :: TestTree
opensOnTheTitle =
  testGroup
    "the window opens on the way in and not on level one"
    [ testCase "the title is what is up" $
        assertEqual "at the title" Title (appStage opened),
      testCase "and space is the way past it" $
        assertEqual "playing" Playing (appStage (go opened))
    ]

thereIsAWayOut :: TestTree
thereIsAWayOut =
  testGroup
    "q leaves, from wherever the player is"
    [ testCase name $
        assertBool "should have asked to quit" (appQuit (typed "q" app))
    | (name, app) <- everyScreen
    ]

escapeGoesBack :: TestTree
escapeGoesBack =
  testGroup
    "escape is one screen back"
    [ testCase "from a level, the level screen, with the cursor on that level" $
        let app = esc (typed "n" (go opened))
         in do
              assertEqual "the level screen" Levels (appStage app)
              assertEqual "on the level just left" (appIndex app) (appPick app),
      testCase "from the level screen, the title" $
        assertEqual "the title" Title (appStage (esc (esc (go opened))))
    ]

finishingIsNoticed :: TestTree
finishingIsNoticed =
  testGroup
    "a finished level is a thing that happened"
    [ testCase "the level says so" $
        assertBool "should be solved" (isCleared (appStage finished)),
      testCase "and is remembered as finished" $
        assertBool "level one should be ticked" (Set.member 0 (appFinished finished)),
      testCase "space is the next level" $
        assertEqual "on to the second" 1 (appIndex (go finished)),
      testCase "undo takes the winning move back and puts the level back" $
        assertEqual "playing again" Playing (appStage (typed "u" finished))
    ]
  where
    isCleared (Cleared _) = True
    isCleared _ = False

-- | Nobody is left sitting on the last level: past the end is the level
-- screen, which with everything ticked is the only ending the game has.
theLastLevelEnds :: TestTree
theLastLevelEnds =
  testCase "past the last level is the level screen, not the first level" $
    let app = (go opened) {appIndex = length builtinLevels - 1, appStage = Cleared 9}
     in assertEqual "the level screen" Levels (appStage (go app))

choosingALevelPlaysIt :: TestTree
choosingALevelPlaysIt =
  testCase "the level screen picks a level and plays it" $
    let app = go (typed "d" (esc (go opened)))
     in do
          assertEqual "playing" Playing (appStage app)
          assertEqual "the second one" 1 (appIndex app)

-- | The lap drawn over a finished level is walked with the game's own step, so
-- it says what the strip says: one lap lands in the row on the far side of the
-- middle, and it takes two to come home.
theLapComesHome :: TestTree
theLapComesHome =
  case drop 1 builtinLevels of
    (SomeLevel lvl : _) -> check (newGame lvl)
    [] -> testCase "the lap" (assertFailure "not enough levels")
  where
    check :: forall w h. (KnownStrip w h) => Game w h -> TestTree
    check g =
      testGroup
        "the lap a finished level takes"
        [ testCase "one lap is the mirrored row" $
            assertEqual "across the middle" (Just (x, across - 1 - y)) (at around),
          testCase "two laps is home" $
            assertEqual "back at the player" (Just (x, y)) (at (2 * around)),
          testCase "and then it stops" $
            assertEqual "nothing left to draw" Nothing (at (2 * around + 1))
        ]
      where
        (around, across) = stripSize @w @h
        (x, y) = spotXY (playPlayer (gamePlay g))
        -- Half a cell in, so the test is not sitting on a boundary of 'floor'.
        at k = spotXY <$> lapMark ((fromIntegral k + 0.5) * lapPace) g

shellTests :: TestTree
shellTests =
  testGroup
    "the game around the board"
    [ opensOnTheTitle,
      thereIsAWayOut,
      escapeGoesBack,
      finishingIsNoticed,
      theLastLevelEnds,
      choosingALevelPlaysIt,
      theLapComesHome
    ]
