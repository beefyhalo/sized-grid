{-# LANGUAGE DataKinds #-}

-- | The level format, the levels that ship, and the standard they are held to.
--
-- Three separate things are checked here, and they fail for different reasons:
--
--   * Every level has a solution. Because the solver plays through
--     "Sokoban.Rules", a level that stops having one is a rule that stopped
--     meaning what it meant, and this is where that shows up.
--
--   * Every level after the first needs the half turn --- no solution on a
--     cylinder of the same shape, none on a plain rectangle of the same shape.
--     This is the standard the epic asks for, and it is the one that separates
--     a puzzle from a gimmick. It has already caught one level of mine that
--     looked like a Mobius puzzle and was a cylinder puzzle.
--
--   * The two solvers agree. "Sokoban.Solve" plays the real rules on the real
--     surface; "Sokoban.Flat" is forty lines of @Int@ arithmetic that has
--     never heard of grid-sized. On a strip whose seam is walled off the two
--     are looking at the same surface, so they must reach the same answer.
module Test.Levels
  ( levelTests,
  )
where

import Data.Either (isLeft)
import Data.Maybe (isJust, mapMaybe)
import Sokoban.Board
import Sokoban.Flat
import Sokoban.Level
import Sokoban.Rules
import Sokoban.Solve
import Test.Tasty
import Test.Tasty.HUnit

-- | Generous: the built-in levels are small, and a budget that has to be tuned
-- per level is a budget hiding a level that is too big. The largest built-in
-- reaches 3772 states.
budget :: Int
budget = 500000

layoutOf :: SomeLevel -> Either String Layout
layoutOf (SomeLevel l) = readLayout (levelPicture l)

named :: SomeLevel -> String
named (SomeLevel l) = levelName l

solved' :: SomeLevel -> Maybe Solution
solved' (SomeLevel l) = solveLevel budget l

everyBuiltinIsSolvable :: TestTree
everyBuiltinIsSolvable =
  testGroup
    "every built-in level has a solution, and it is a legal one"
    [ testCase (show i ++ ". " ++ named lvl) (check lvl)
    | (i, lvl) <- zip [1 :: Int ..] builtinLevels
    ]
  where
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

-- | The standard. The first level is the deliberate exception and is asserted
-- as one, so that a change making it need the twist fails here rather than
-- quietly contradicting its own note.
everyBuiltinNeedsTheTwist :: TestTree
everyBuiltinNeedsTheTwist =
  testGroup
    "the levels need the surface they are on"
    [ testCase "the first level needs the wrap and not the twist" $
        case layoutOf firstLevel of
          Left err -> assertFailure err
          Right lay -> do
            assertEqual
              "should be unsolvable on a rectangle"
              Nothing
              (solvableOn Rectangle budget lay)
            assertBool
              "should be solvable on a cylinder"
              (isJust (solvableOn Cylinder budget lay)),
      testCase "every level after the first is unsolvable on both flat surfaces" $
        assertEqual
          "levels solvable without the half turn"
          []
          [ (named lvl, verdictLine v)
          | lvl <- drop 1 builtinLevels,
            Right lay <- [layoutOf lvl],
            let v = verdict budget lay,
            v /= Verdict Nothing Nothing
          ]
    ]

-- | Two implementations, one answer.
--
-- Walling off the first and last columns puts a wall on both sides of the
-- seam, so no move can reach it and the strip /is/ the rectangle of the same
-- shape. Both solvers are then looking at the same surface. The controls below
-- are corridors in which every move is a push, so the two searches --- one
-- shortest by moves, the other shortest by pushes --- are also answering the
-- same question and the counts are comparable.
solversAgree :: TestTree
solversAgree =
  testGroup
    "with the seam walled off, the strip is a rectangle and both solvers say so"
    [ agree "a forced corridor" 3 ["########", "#-@$--.#", "########"],
      agree "a forced push upwards" 1 ["#####", "#-.-#", "#-$-#", "#-@-#", "#####"],
      agree
        "two crates, two pushes"
        2
        ["#######", "#--..-#", "#--$$-#", "#-@---#", "#######"],
      testCase "and both refuse the same impossible one" $
        case parseLevel (unlines (headers ++ ["######", "#-@$#.#", "######"])) of
          Left err -> assertFailure err
          Right sl@(SomeLevel l) -> do
            assertEqual "the strip" Nothing (solveLevel budget l)
            case layoutOf sl of
              Left err -> assertFailure err
              Right lay ->
                assertEqual "the rectangle" Nothing (solvableOn Rectangle budget lay)
    ]
  where
    headers = ["name: control", "note: control"]
    agree :: String -> Int -> [String] -> TestTree
    agree what pushes rows =
      testCase what $
        case parseLevel (unlines (headers ++ rows)) of
          Left err -> assertFailure err
          Right sl@(SomeLevel l) -> do
            assertEqual
              "Sokoban.Solve, on the strip"
              (Just pushes)
              (solutionPushes <$> solveLevel budget l)
            case layoutOf sl of
              Left err -> assertFailure err
              Right lay ->
                assertEqual
                  "Sokoban.Flat, on the rectangle"
                  (Just pushes)
                  (solvableOn Rectangle budget lay)

-- | A ramp, not a heap. Weak on purpose: asserting the exact ordering would
-- break on every level edit, and what actually matters is that the game does
-- not open on its hardest level or end on its easiest.
theRampIsARamp :: TestTree
theRampIsARamp =
  testGroup
    "the levels are a ramp"
    [ testCase "there are at least six of them" $
        assertBool
          ("only " ++ show (length builtinLevels))
          (length builtinLevels >= 6),
      testCase "the first is the smallest search and the last is the largest" $
        case mapMaybe (fmap solutionSeen . solved') builtinLevels of
          [] -> assertFailure "no level solved"
          sizes@(smallest : _) -> do
            assertEqual "first" (minimum sizes) smallest
            assertEqual "last" (maximum sizes) (last sizes)
    ]

everyBuiltinIsNamedAndExplained :: TestTree
everyBuiltinIsNamedAndExplained =
  testCase "every built-in level says what it is and the one thing it teaches" $
    assertEqual
      "levels with an empty name, or a note too short to be an explanation"
      []
      [ levelName l
      | SomeLevel l <- builtinLevels,
        null (levelName l) || length (levelNote l) < 40
      ]

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
    [ bad "no picture at all" ["name: nothing"],
      bad "an unknown character" ["-@$%-."],
      bad "no player" ["-$---."],
      bad "two players" ["@@$--."],
      bad "no crates" ["-@---."],
      bad "more crates than goals" ["-@$$-."],
      bad "more goals than crates" ["-@$-.."]
    ]
  where
    bad what rows =
      testCase what $
        assertBool "should not parse" (isLeft (parseLevel (unlines rows)))

commentsAndHeadings :: TestTree
commentsAndHeadings =
  testCase "comments are dropped and a multi-line note is one paragraph" $
    case parseLevel
      ( unlines
          [ "; not part of the level",
            "name: A name",
            "note: first half",
            "note: second half",
            "-@$--."
          ]
      ) of
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
        assertEqual "padded out to six" "-@$--.\n------\n" (levelPicture l)

levelTests :: TestTree
levelTests =
  testGroup
    "levels"
    [ everyBuiltinIsSolvable,
      everyBuiltinNeedsTheTwist,
      solversAgree,
      theRampIsARamp,
      everyBuiltinIsNamedAndExplained,
      picturesRoundTrip,
      parserRefusals,
      commentsAndHeadings,
      shortRowsAreFloor
    ]
