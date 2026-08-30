-- | Three ways in: a window, a terminal, and the solver.
--
-- The terminal view is not a fallback. It is the same layout the window's
-- flat view uses --- the strip with the far side of each edge drawn past it
-- --- at the cheapest possible fidelity, and it is where that layout was tried
-- first. It stays because it is testable in a pipe, which the window is not.
module Main (main) where

import Control.Monad (when)
import Data.Char (toLower)
import Data.Maybe (fromMaybe, isJust)
import Data.Set qualified as Set
import Sokoban.Board
import Sokoban.Flat
import Sokoban.Level
import Sokoban.Rules
import Sokoban.Shell (run)
import Sokoban.Solve
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO
  ( BufferMode (..),
    hPutStrLn,
    hSetBuffering,
    isEOF,
    stderr,
    stdout,
  )

main :: IO ()
main = do
  args <- getArgs
  prog <- getProgName
  case args of
    ["--help"] -> putStr (usage prog)
    [] -> run builtinLevels
    ["--check"] -> checkAll builtinLevels
    ["--check", path] -> withFile path checkAll
    ["--text"] -> start builtinLevels
    ["--text", path] -> withFile path start
    [path] -> withFile path run
    _ -> die (usage prog)
  where
    withFile path k = do
      src <- readFile path
      case parseLevels src of
        Left err -> die (path ++ ": " ++ err)
        Right ls -> k ls

usage :: String -> String
usage prog =
  unlines
    [ "usage: " ++ prog ++ " [--text] [LEVELS-FILE]",
      "       " ++ prog ++ " --check",
      "",
      "  with no argument, opens a window on the built-in levels",
      "  --text    play in the terminal instead",
      "  --check   solve every level on its own surface and on each flat",
      "            surface of the same shape it is fair to compare against,",
      "            and report whether the half turn was needed",
      "",
      "a level is a picture of itself, one character per cell: # wall, space",
      "or - floor, . goal, $ crate, * crate on goal, @ player, + player on a",
      "goal. Levels in a file are separated by a line of three or more equals",
      "signs, and each may carry name:, note: and surface: lines. surface: is",
      "one of mobius, klein or projective, and defaults to mobius.",
      "",
      "the window opens on a title screen: space plays, l is the level",
      "screen, escape is one screen back, and q leaves.",
      "",
      "keys, in a level: arrows or wasd / hjkl move, u undo, r restart,",
      "      n / p change level, v change view, f change frame,",
      "      esc the level screen, q quit",
      "",
      "  v cycles three views: the strip flat with the far side of each edge",
      "  drawn past it, the same surface centred on the player, and the strip",
      "  as the band it is. Only the first is meant to be played in.",
      "",
      "keys, in the terminal: h j k l or w a s d move, u undo, r restart,",
      "      n next level, f change frame, q quit"
    ]

die :: String -> IO a
die msg = hPutStrLn stderr msg >> exitFailure

-- | Solve every level on its own surface, and again on each flat surface of
-- the same shape it is fair to compare against. The first says the level can
-- be finished; the rest say whether finishing it needed the gluing.
checkAll :: [SomeLevel] -> IO ()
checkAll ls = do
  mapM_ report (zip [1 ..] ls)
  putStrLn ""
  putStrLn
    ( "Levels whose answer is the gluing: "
        ++ show (length headline)
        ++ " of "
        ++ show (length ls)
    )
  where
    budget = 500000
    report :: (Int, SomeLevel) -> IO ()
    report (i, SomeLevel lvl) = do
      putStrLn
        ( show i
            ++ ". "
            ++ levelName lvl
            ++ "   "
            ++ size lvl
            ++ " on "
            ++ surfaceTitle (levelSurface lvl)
        )
      putStrLn ("     " ++ surfaceName (levelSurface lvl) ++ ": " ++ onSurface lvl)
      putStrLn ("     " ++ flatVerdict lvl)
    size :: forall w h. (KnownStrip w h) => Level w h -> String
    size _ = let (a, b) = stripSize @w @h in show a ++ " around, " ++ show b ++ " across"
    onSurface :: forall w h. (KnownStrip w h) => Level w h -> String
    onSurface lvl =
      case solveLevel budget lvl of
        Nothing -> "NO SOLUTION FOUND"
        Just s ->
          show (length (solutionMoves s))
            ++ " moves, "
            ++ show (solutionPushes s)
            ++ " pushes, "
            ++ show (solutionSeen s)
            ++ " states"
    flatVerdict :: forall w h. (KnownStrip w h) => Level w h -> String
    flatVerdict lvl =
      case readLayout (levelPicture lvl) of
        Left err -> "cannot re-read this level's own picture: " ++ err
        Right lay -> verdictLine surface (verdict surface budget lay)
      where
        surface = levelSurface lvl
    headline =
      [ ()
      | SomeLevel lvl <- ls,
        Right lay <- [readLayout (levelPicture lvl)],
        Verdict Nothing <- [verdict (levelSurface lvl) budget lay]
      ]

start :: [SomeLevel] -> IO ()
start [] = die "no levels"
start levels = do
  hSetBuffering stdout NoBuffering
  go levels
  where
    go [] = putStrLn "That is all of them."
    go (SomeLevel lvl : rest) = do
      finished <- loop ChartFrame (newGame lvl)
      when finished $ go rest

-- | Returns whether the level was finished, as opposed to quit out of.
--
-- The end of input is a way out from both prompts, and it did not used to be
-- from the second: a level solved by a script rather than by a person got as
-- far as \"press return\" and then died on the read. Which is the same
-- complaint sized-grid-lopy.8 is about, one screen over.
loop :: (KnownStrip w h) => Frame -> Game w h -> IO Bool
loop frame game = do
  putStr (render frame game)
  when done (putStrLn "\nSolved. Press return for the next level.")
  eof <- isEOF
  if eof
    then pure False
    else do
      line <- getLine
      if done
        then pure True
        else keys (map toLower line) frame game
  where
    done = solved game

keys :: (KnownStrip w h) => String -> Frame -> Game w h -> IO Bool
keys [] frame game = loop frame game
keys (c : cs) frame game =
  case c of
    'q' -> pure False
    'u' -> keys cs frame (fromMaybe game (undo game))
    'r' -> keys cs frame (restart game)
    'n' -> pure True
    'f' -> keys cs (other frame) game
    _ ->
      case dirKey c of
        Nothing -> keys cs frame game
        Just dir -> keys cs frame (fst (move frame dir game))
  where
    other ChartFrame = PlayerFrame
    other PlayerFrame = ChartFrame

dirKey :: Char -> Maybe Dir
dirKey 'h' = Just DirLeft
dirKey 'a' = Just DirLeft
dirKey 'l' = Just DirRight
dirKey 'd' = Just DirRight
dirKey 'k' = Just DirUp
dirKey 'w' = Just DirUp
dirKey 'j' = Just DirDown
dirKey 's' = Just DirDown
dirKey _ = Nothing

-- | The board as text, with the far side of every seam drawn dimmed past it.
--
-- This is candidate (b) of sized-grid-lopy.1 --- the flat rectangle with ghost
-- continuations --- at the cheapest possible fidelity, and it asks the surface
-- where the picture carries on rather than knowing (sized-grid-lopy.7). On a
-- Mobius strip that means three columns each side and nothing above or below,
-- because there is genuinely nothing there; on a Klein bottle or a projective
-- plane it means all four. A player who can plan a seam crossing from this
-- does not need a window; if this is unreadable, that is what the window has
-- to fix.
render :: forall w h. (KnownStrip w h) => Frame -> Game w h -> String
render frame game =
  unlines $
    [ levelName lvl ++ "   on " ++ surfaceTitle surface,
      "",
      replicate (ghostCols + 4) ' '
        ++ "dimmed cells are the far side of a seam"
    ]
      ++ map row (reverse [-ghostRows .. across - 1 + ghostRows])
      ++ [ "",
           "goals left "
             ++ show (goalsLeft game)
             ++ "   moves "
             ++ show (playMoves play)
             ++ "   pushes "
             ++ show (playPushes play)
             ++ "   facing "
             ++ dirName (dirOf frame (playTurn play) (playFacing play))
             ++ "   frame "
             ++ frameName
             ++ standing
         ]
      ++ ( if null (levelNote lvl)
             then []
             else ["", levelNote lvl]
         )
  where
    lvl = gameLevel game
    surface = gameSurface game
    play = gamePlay game
    frameName =
      case frame of
        ChartFrame -> "chart"
        PlayerFrame -> "player"
    standing =
      case turnNote (playTurn play) of
        "" -> ""
        note -> " (" ++ note ++ ")"
    (around, across) = stripSize @w @h
    ghostCols = depth (around, 0) 3 around
    ghostRows = depth (0, across) 2 across
    depth outside most n
      | isJust (spotBeyond @w @h surface outside) = min most n
      | otherwise = 0
    row y =
      pad (label y)
        ++ " "
        ++ dim [cellChar x y | x <- [-ghostCols .. -1]]
        ++ wall y
        ++ [cellChar x y | x <- [0 .. around - 1]]
        ++ wall y
        ++ dim [cellChar x y | x <- [around .. around + ghostCols - 1]]
        ++ " "
        ++ label y
    -- A ghost row is not numbered: it is somewhere else on the surface, and
    -- giving it a row number would be claiming it is a row of this picture.
    label y
      | y < 0 || y >= across = ""
      | otherwise = show y
    wall y
      | y < 0 || y >= across = " "
      | otherwise = "|"
    pad t = replicate (2 - length t) ' ' ++ t
    dim t = "\ESC[2m" ++ t ++ "\ESC[0m"
    cellChar x y =
      case spotBeyond @w @h surface (x, y) of
        Nothing -> ' '
        Just s
          | spotCoord s == spotCoord (playPlayer play) ->
              if onGoal s
                then '+'
                else '@'
          | crateAt play s ->
              if onGoal s
                then '*'
                else '$'
          | otherwise ->
              case tileAt game s of
                Wall -> '#'
                Goal -> '.'
                Floor -> '-'
    onGoal s = Set.member (spotCoord s) (levelGoals lvl)
