-- | A sudoku board sliced into its rows, columns and 3x3 squares purely by
-- type-level shape, then solved by backtracking --- and, since
-- sized-grid-nnww.4, with that search visible rather than hidden inside one
-- call.
--
-- Three front ends over one solver:
--
--   [@--gloss@] the default, a window replaying the search step by step;
--   [@--ansi@]  the same replay in the terminal, for a machine with no
--               display (see sized-grid-nnww.6);
--   [@--text@]  the slice dump this demo used to be, plus the answer.
module Main (main) where

import           Sudoku.Board
import           Sudoku.Solve

import qualified Sudoku.Render.Ansi  as Ansi
import qualified Sudoku.Render.Gloss as Gloss

import           Data.Grid.Sized     hiding (All, Compose)

import           Data.Maybe          (fromJust)
import           System.Environment  (getArgs, getProgName)
import           System.Exit         (exitFailure)
import           System.IO           (hPutStrLn, stderr)

data Mode
    = Gloss
    | Ansi
    | Text
    deriving (Eq)

data Options = Options
    { optMode   :: Mode
    , optRate   :: Float
    -- ^ Steps of the search per second, for the two animated modes.
    , optSource :: Maybe FilePath
    -- ^ 'Nothing' for the built-in example, @Just "-"@ for stdin.
    }

defaultOptions :: Options
defaultOptions = Options {optMode = Gloss, optRate = 20, optSource = Nothing}

parseArgs :: [String] -> Either String Options
parseArgs = go defaultOptions
  where
    go opts [] = Right opts
    go opts ("--gloss":as) = go opts {optMode = Gloss} as
    go opts ("--ansi":as) = go opts {optMode = Ansi} as
    go opts ("--text":as) = go opts {optMode = Text} as
    go opts ("--rate":r:as) =
        case reads r of
            [(v, "")]
                | v > 0 -> go opts {optRate = v} as
            _ -> Left ("--rate wants a positive number, got " ++ show r)
    go _ ["--rate"] = Left "--rate wants a number after it"
    go opts (a:as)
        | take 2 a == "--" = Left ("unknown option " ++ a)
        | Just _ <- optSource opts = Left "only one board can be given"
        | otherwise = go opts {optSource = Just a} as

usage :: String -> String
usage prog =
    unlines
        [ "usage: " ++ prog ++ " [--gloss|--ansi|--text] [--rate N] [FILE|-]"
        , ""
        , "  --gloss   animate the search in a window (default)"
        , "  --ansi    animate the search in the terminal"
        , "  --text    print the slices and the answer, no animation"
        , "  --rate N  steps of the search per second (default 20)"
        , ""
        , "  FILE      a board to solve; '-' reads one from stdin."
        , "            Digits 1-9 are clues and 0, . or _ are blanks;"
        , "            everything else is ignored, so most layouts read."
        , "            With no FILE the built-in example is used."
        ]

main :: IO ()
main = do
    args <- getArgs
    prog <- getProgName
    case parseArgs args of
        Left err -> die (err ++ "\n\n" ++ usage prog)
        Right opts -> do
            board <- readBoard (optSource opts)
            case optMode opts of
                Gloss -> Gloss.animate (optRate opts) board
                Ansi  -> Ansi.animate (optRate opts) board
                Text  -> textReport board

die :: String -> IO a
die msg = hPutStrLn stderr msg >> exitFailure

readBoard :: Maybe FilePath -> IO Board
readBoard Nothing = pure exampleBoard
readBoard (Just path) = do
    input <-
        if path == "-"
            then getContents
            else readFile path
    case parseBoard input of
        Left err -> die (describe ++ ": " ++ err)
        Right b  -> pure b
  where
    describe =
        if path == "-"
            then "stdin"
            else path

-- | What this demo printed before it had a window: the board, the three
-- families of slices it is cut into, and the answer.
textReport :: Board -> IO ()
textReport board = do
    putStrLn "Board:"
    putStr (displayBoard board)
    putStrLn ""
    putStr (showSlices "rows" (rows board))
    putStr (showSlices "columns" (columns board))
    putStr (showSlices "squares" (squares board))
    putStrLn ""
    putStrLn ("solved:  " ++ show (gameIsSolved board))
    putStrLn ("invalid: " ++ show (gameIsInvalid board))
    putStrLn ""
    putStrLn "Slices through (4,4):"
    putStr (showSlices "row" [rowAtPoint samplePoint board])
    putStr (showSlices "column" [columnAtPoint samplePoint board])
    putStr (showSlices "square" [squareAtPoint samplePoint board])
    putStrLn
        ("candidates at (4,4): " ++
         displaySlice (map Just (indexGrid (allValues board) samplePoint)))
    putStrLn ""
    let st = traceStats (solveTrace board)
    putStrLn
        ("search: " ++
         show (statsPlacements st) ++
         " placements, " ++ show (statsBacktracks st) ++ " backtracks")
    case solveBoard board of
        Nothing     -> putStrLn "No solution."
        Just solved -> do
            putStrLn "Solved board:"
            putStr (displayBoard solved)

samplePoint :: Coord Cs
samplePoint =
    fromJust (numToOrdinal (4 :: Integer)) :|
    singleCoord (fromJust (numToOrdinal (4 :: Integer)))
