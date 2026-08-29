-- | A maze, carved and then solved, with the moving part of both algorithms
-- being a grid's own focus rather than a coordinate carried alongside it.
--
-- See "Maze.Generate" for the half that makes the point: no index arithmetic
-- and no bounds test anywhere in it, because on a @Clamped@ axis a step that
-- would leave the board is @Nothing@ and that is the only wall test the
-- carving needs.
module Main (main) where

import           Maze.Render        (animate)

import           System.Environment (getArgs, getProgName)
import           System.Exit        (exitFailure)
import           System.IO          (hPutStrLn, stderr)
import           System.Random      (StdGen, mkStdGen, newStdGen)

data Options = Options
    { optRate :: Float
    , optSeed :: Maybe Int
    }

parseArgs :: [String] -> Either String Options
parseArgs = go (Options 250 Nothing)
  where
    go opts [] = Right opts
    go opts ("--rate":r:as) = readInto r "--rate" (\v -> opts {optRate = v}) as
    go _ ["--rate"] = Left "--rate wants a number after it"
    go opts ("--seed":r:as) =
        case reads r of
            [(v, "")] -> go opts {optSeed = Just v} as
            _         -> Left ("--seed wants a whole number, got " ++ show r)
    go _ ["--seed"] = Left "--seed wants a number after it"
    go _ (a:_) = Left ("unknown argument " ++ a)
    readInto r what f as =
        case reads r of
            [(v, "")]
                | v > 0 -> go (f v) as
            _ -> Left (what ++ " wants a positive number, got " ++ show r)

usage :: String -> String
usage prog =
    unlines
        [ "usage: " ++ prog ++ " [--rate N] [--seed N]"
        , ""
        , "  --rate N  moves of the algorithm per second (default 250)"
        , "  --seed N  build the same maze every time"
        ]

main :: IO ()
main = do
    args <- getArgs
    prog <- getProgName
    case parseArgs args of
        Left err -> do
            hPutStrLn stderr (err ++ "\n\n" ++ usage prog)
            exitFailure
        Right opts -> do
            g <- seedGen (optSeed opts)
            animate (optRate opts) g

seedGen :: Maybe Int -> IO StdGen
seedGen Nothing  = newStdGen
seedGen (Just n) = pure (mkStdGen n)
