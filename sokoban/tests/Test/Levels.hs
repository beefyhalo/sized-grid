{-# LANGUAGE DataKinds #-}

-- | The level format, and the levels that ship.
--
-- The solvability check is what stops a level shipping that nobody has
-- finished, and because the solver plays through "Sokoban.Rules" it is also
-- how a change to the rules announces itself: a level that stops having a
-- solution is a rule that stopped meaning what it meant.
module Test.Levels
  ( levelTests
  ) where

import           Sokoban.Board
import           Sokoban.Level
import           Sokoban.Rules
import           Sokoban.Solve

import           Data.Either      (isLeft)
import           Test.Tasty
import           Test.Tasty.HUnit

-- | Generous: the built-in levels are small, and a budget that has to be
-- tuned per level is a budget hiding a level that is too big.
budget :: Int
budget = 500000

everyBuiltinIsSolvable :: TestTree
everyBuiltinIsSolvable =
    testGroup
        "every built-in level has a solution"
        [ testCase (show i ++ ". " ++ name lvl) (check lvl)
        | (i, lvl) <- zip [1 :: Int ..] builtinLevels
        ]
  where
    name :: SomeLevel -> String
    name (SomeLevel l) = levelName l
    check :: SomeLevel -> Assertion
    check (SomeLevel l) =
        case solveLevel budget l of
            Nothing -> assertFailure "no solution found"
            Just s -> do
                let (played, outs) = replay ChartFrame (solutionMoves s) (newGame l)
                assertBool
                    "the solver's answer is all legal moves"
                    (all outcomeMoved outs)
                assertBool "and it finishes the level" (solved played)

everyBuiltinIsNamedAndExplained :: TestTree
everyBuiltinIsNamedAndExplained =
    testCase "every built-in level says what it is and what it teaches" $
    assertEqual
        "levels with an empty name or note"
        []
        [ levelName l
        | SomeLevel l <- builtinLevels
        , null (levelName l) || null (levelNote l)
        ]

-- | The claim the game is making. A level whose solution never uses the seam
-- would work as well on a flat board, and this checks the two built-ins that
-- say they need it really do: cut the seam, and there is no solution left.
--
-- Cutting it is done in the picture rather than in the code --- walling off
-- the first and last columns leaves the same level on a board a crate cannot
-- get around --- so what is being tested is the level, not a second
-- implementation of the surface.
seamIsLoadBearing :: TestTree
seamIsLoadBearing =
    testGroup
        "the levels that claim to need the seam do need it"
        [ testCase "the far row, with the seam walled off, has no solution" $
          noSolution
              [ "#------#"
              , "#--.---#"
              , "########"
              , "#-@$---#"
              , "#------#"
              ]
        , testCase "twice around, with the seam walled off, has no solution" $
          noSolution
              [ "#------#"
              , "#------#"
              , "########"
              , "#-$@#.-#"
              , "#------#"
              ]
        ]
  where
    noSolution :: [String] -> Assertion
    noSolution rows =
        case parseLevel (unlines ("name: cut" : "note: cut" : rows)) of
            Left err -> assertFailure ("test level: " ++ err)
            Right (SomeLevel l) ->
                assertEqual "should be unsolvable" Nothing (solveLevel budget l)

picturesRoundTrip :: TestTree
picturesRoundTrip =
    testCase "a level written back out reads as the same level" $
    mapM_ check builtinLevels
  where
    check :: SomeLevel -> Assertion
    check (SomeLevel l) =
        case parseLevel (levelPicture l) of
            Left err -> assertFailure (levelName l ++ ": " ++ err)
            Right (SomeLevel l') ->
                assertEqual
                    (levelName l ++ ": the picture of the picture")
                    (levelPicture l)
                    (levelPicture l')

parserRefusals :: TestTree
parserRefusals =
    testGroup
        "the parser refuses what it cannot make a level of"
        [ bad "no picture at all" ["name: nothing"]
        , bad "an unknown character" ["-@$%-."]
        , bad "no player" ["-$---."]
        , bad "two players" ["@@$--."]
        , bad "no crates" ["-@---."]
        , bad "more crates than goals" ["-@$$-."]
        , bad "more goals than crates" ["-@$-.."]
        ]
  where
    bad what rows =
        testCase what $
        assertBool "should not parse" (isLeft (parseLevel (unlines rows)))

commentsAndHeadings :: TestTree
commentsAndHeadings =
    testCase "comments are dropped and a multi-line note is one paragraph" $
    case parseLevel
             (unlines
                  [ "; not part of the level"
                  , "name: A name"
                  , "note: first half"
                  , "note: second half"
                  , "-@$--."
                  ]) of
        Left err -> assertFailure err
        Right (SomeLevel l) -> do
            assertEqual "name" "A name" (levelName l)
            assertEqual "note" "first half second half" (levelNote l)

shortRowsAreFloor :: TestTree
shortRowsAreFloor =
    testCase "a short row is padded with floor, so the width is the widest row" $
    case parseLevel (unlines ["name: n", "note: n", "-@$--.", "--"]) of
        Left err -> assertFailure err
        Right (SomeLevel l) ->
            assertEqual
                "padded out to six"
                "-@$--.\n------\n"
                (levelPicture l)

levelTests :: TestTree
levelTests =
    testGroup
        "levels"
        [ everyBuiltinIsSolvable
        , everyBuiltinIsNamedAndExplained
        , seamIsLoadBearing
        , picturesRoundTrip
        , parserRefusals
        , commentsAndHeadings
        , shortRowsAreFloor
        ]
