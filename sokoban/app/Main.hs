-- | Three ways in: a window, a terminal, and the solver.
--
-- The terminal view is not a fallback. It is the same layout the window's
-- flat view uses --- the strip with the far side of each edge drawn past it
-- --- at the cheapest possible fidelity, and it is where that layout was tried
-- first. It stays because it is testable in a pipe, which the window is not.
module Main (main) where

import           Sokoban.Board
import           Sokoban.Flat
import           Sokoban.Level
import           Sokoban.Render     (run)
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
        [ "usage: " ++ prog ++ " [--text] [LEVELS-FILE]"
        , "       " ++ prog ++ " --check"
        , ""
        , "  with no argument, opens a window on the built-in levels"
        , "  --text    play in the terminal instead"
        , "  --check   solve every level three times -- on the strip, on a"
        , "            cylinder of the same shape and on a plain rectangle --"
        , "            and report whether the twist was needed"
        , ""
        , "keys, in the window: arrows or wasd / hjkl move, u undo, r restart,"
        , "      n / p change level, v change view, f change frame"
        , "keys, in the terminal: h j k l or w a s d move, u undo, r restart,"
        , "      n next level, f change frame, q quit"
        ]

die :: String -> IO a
die msg = hPutStrLn stderr msg >> exitFailure

-- | Solve every level three times: on the strip, on a cylinder of the same
-- shape, and on a plain rectangle of the same shape. The first says the level
-- can be finished; the other two say whether finishing it needed the surface.
checkAll :: [SomeLevel] -> IO ()
checkAll ls = do
    mapM_ report (zip [1 ..] ls)
    putStrLn ""
    putStrLn
        ("Levels whose answer is the twist: " ++ show (length headline) ++ " of " ++
         show (length ls))
  where
    budget = 500000
    report :: (Int, SomeLevel) -> IO ()
    report (i, SomeLevel lvl) = do
        putStrLn (show i ++ ". " ++ levelName lvl ++ "   " ++ size lvl)
        putStrLn ("     strip: " ++ onStrip lvl)
        putStrLn ("     " ++ flatVerdict lvl)
    size :: forall w h. KnownStrip w h => Level w h -> String
    size _ = let (a, b) = stripSize @w @h in show a ++ " around, " ++ show b ++ " across"
    onStrip :: forall w h. KnownStrip w h => Level w h -> String
    onStrip lvl =
        case solveLevel budget lvl of
            Nothing -> "NO SOLUTION FOUND"
            Just s ->
                show (length (solutionMoves s)) ++ " moves, " ++
                show (solutionPushes s) ++ " pushes, " ++ show (solutionSeen s) ++
                " states"
    flatVerdict :: forall w h. KnownStrip w h => Level w h -> String
    flatVerdict lvl =
        case readLayout (levelPicture lvl) of
            Left err -> "cannot re-read this level's own picture: " ++ err
            Right lay -> verdictLine (verdict budget lay)
    headline =
        [ ()
        | SomeLevel lvl <- ls
        , Right lay <- [readLayout (levelPicture lvl)]
        , Verdict Nothing Nothing <- [verdict budget lay]
        ]

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
      "   facing " ++ dirName (dirOf frame (playFlipped play) (playFacing play)) ++
      "   frame " ++ frameName ++
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
