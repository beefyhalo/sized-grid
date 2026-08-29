-- | The game at a terminal, until sized-grid-lopy.1 settles how to draw a
-- surface that does not lie flat.
--
-- A text view is not a placeholder for the sake of one: the flat picture is
-- one of the two candidate views, and it is the one that has to be beaten. If
-- a player can plan a seam crossing from this, a window is a nicety; if they
-- cannot, that is the finding.
module Main (main) where

import           Sokoban.Board
import           Sokoban.Level
import           Sokoban.Rules
import           Sokoban.Solve

import           Data.Char          (toLower)
import qualified Data.Set           as Set
import           System.Environment (getArgs, getProgName)
import           System.Exit        (exitFailure)
import           System.IO          (BufferMode (..), hPutStrLn, hSetBuffering,
                                     isEOF, stderr, stdout)

main :: IO ()
main = do
    args <- getArgs
    prog <- getProgName
    case args of
        ["--help"] -> putStr (usage prog)
        [] -> start builtinLevels
        ["--check"] -> checkAll builtinLevels
        [path] -> do
            src <- readFile path
            case parseLevels src of
                Left err -> die (path ++ ": " ++ err)
                Right ls -> start ls
        _ -> die (usage prog)

usage :: String -> String
usage prog =
    unlines
        [ "usage: " ++ prog ++ " [LEVELS-FILE | --check]"
        , ""
        , "  with no argument, plays the built-in levels"
        , "  --check   solve every built-in level and report, without playing"
        , ""
        , "keys: h j k l or w a s d to move, u undo, r restart, n next level,"
        , "      f flip between chart frame and player frame, q quit"
        ]

die :: String -> IO a
die msg = hPutStrLn stderr msg >> exitFailure

checkAll :: [SomeLevel] -> IO ()
checkAll ls =
    mapM_
        (\(i, SomeLevel lvl) ->
             putStrLn $
             show (i :: Int) ++ ". " ++ levelName lvl ++ ": " ++
             case solveLevel 200000 lvl of
                 Nothing -> "NO SOLUTION FOUND"
                 Just s ->
                     show (length (solutionMoves s)) ++ " moves, " ++
                     show (solutionPushes s) ++ " pushes, " ++
                     show (solutionSeen s) ++ " states")
        (zip [1 ..] ls)

start :: [SomeLevel] -> IO ()
start [] = die "no levels"
start levels = do
    hSetBuffering stdout NoBuffering
    go levels
  where
    go [] = putStrLn "That is all of them."
    go (SomeLevel lvl:rest) = do
        finished <- loop ChartFrame (newGame lvl)
        if finished
            then go rest
            else pure ()

-- | Returns whether the level was finished, as opposed to quit out of.
loop :: KnownStrip w h => Frame -> Game w h -> IO Bool
loop frame game = do
    putStr (render frame game)
    if solved game
        then putStrLn "\nSolved. Press return for the next level." >>
             getLine >> pure True
        else do
            eof <- isEOF
            if eof
                then pure False
                else do
                    line <- getLine
                    keys (map toLower line) frame game

keys :: KnownStrip w h => String -> Frame -> Game w h -> IO Bool
keys [] frame game = loop frame game
keys (c:cs) frame game =
    case c of
        'q' -> pure False
        'u' -> keys cs frame (maybe game id (undo game))
        'r' -> keys cs frame (restart game)
        'n' -> pure True
        'f' -> keys cs (other frame) game
        _ ->
            case dirKey c of
                Nothing  -> keys cs frame game
                Just dir -> keys cs frame (fst (move frame dir game))
  where
    other ChartFrame  = PlayerFrame
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
dirKey _   = Nothing

-- | The board as text, with the far side of the seam drawn dimmed past each
-- edge.
--
-- This is candidate (b) of sized-grid-lopy.1 --- the flat rectangle with ghost
-- continuations --- at the cheapest possible fidelity. Leaving the board
-- sideways puts you in the row the mirror sends you to, so past the left edge
-- is the /last/ few columns of that row and past the right edge is the first
-- few, and both are drawn upside down relative to the row they are printed
-- beside. A player who can plan a seam crossing from this does not need a
-- window; if this is unreadable, that is what the window has to fix.
render :: forall w h. KnownStrip w h => Frame -> Game w h -> String
render frame game =
    unlines $
    [ levelName lvl
    , ""
    , replicate (ghost + 4) ' ' ++ "the seam: dimmed cells are the far side," ++
      " mirrored"
    ] ++
    map row (reverse [0 .. across - 1]) ++
    [ ""
    , "goals left " ++ show (goalsLeft game) ++ "   moves " ++
      show (playMoves play) ++ "   pushes " ++ show (playPushes play) ++
      "   facing " ++ dirName (playFacing play) ++ "   frame " ++ frameName ++
      upright
    ] ++
    (if null (levelNote lvl)
         then []
         else ["", levelNote lvl])
  where
    lvl = gameLevel game
    play = gamePlay game
    frameName =
        case frame of
            ChartFrame  -> "chart"
            PlayerFrame -> "player"
    upright
        | playFlipped play = " (upside down)"
        | otherwise = ""
    (around, across) = stripSize @w @h
    ghost = min 3 around
    -- The row a step through the seam from row y arrives in, as the row
    -- number this function prints rather than as a chart coordinate.
    partner y = across - 1 - y
    row y =
        pad (show y) ++ " " ++
        dim [cellChar x (partner y) | x <- [around - ghost .. around - 1]] ++
        "|" ++
        [cellChar x y | x <- [0 .. around - 1]] ++
        "|" ++
        dim [cellChar x (partner y) | x <- [0 .. ghost - 1]] ++ " " ++ show y
    pad t = replicate (2 - length t) ' ' ++ t
    dim t = "\ESC[2m" ++ t ++ "\ESC[0m"
    cellChar x y =
        case spotAt @w @h x y of
            Nothing -> '?'
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
                        Wall  -> '#'
                        Goal  -> '.'
                        Floor -> '-'
    onGoal s = Set.member (spotCoord s) (levelGoals lvl)
