-- | Two automata that are not Conway's Game of Life, paired because they
-- disagree about what shape a rule is.
--
-- Wireworld is a bulk rule and runs on the same @Stencil@ machinery the
-- game-of-life example uses --- four states instead of two, @Clamped@ axes
-- instead of @Periodic@, and no new library surface at all.
--
-- Langton's ant is a single walker with a heading, which no stencil can hold,
-- and runs on @Walker@ and @stepWalker@. Its topology is a flag, because
-- @stepWalker@ asks the axis type what a step off the edge means and the ant
-- itself does not care.
module Main (main) where

import Automata.Ant qualified as Ant
import Automata.Wireworld qualified as Wireworld
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

data Demo
  = Wireworld
  | Ant Ant.Topology

data Options = Options
  { optDemo :: Demo,
    -- | Generations or steps per second. 'Nothing' takes each demo's own
    -- default, which differ by two orders of magnitude: a Wireworld circuit is
    -- worth watching at ten generations a second and Langton's ant needs
    -- around ten thousand steps before it does anything interesting.
    optRate :: Maybe Float
  }

parseArgs :: [String] -> Either String Options
parseArgs = go (Options (Ant Ant.Torus) Nothing)
  where
    go opts [] = Right opts
    go opts ("--wireworld" : as) = go opts {optDemo = Wireworld} as
    go opts ("--ant" : as) = go opts {optDemo = Ant Ant.Torus} as
    go opts ("--torus" : as) = go opts {optDemo = Ant Ant.Torus} as
    go opts ("--walls" : as) = go opts {optDemo = Ant Ant.Walls} as
    go opts ("--mirror" : as) = go opts {optDemo = Ant Ant.Mirror} as
    go opts ("--rate" : r : as) =
      case reads r of
        [(v, "")]
          | v > 0 -> go opts {optRate = Just v} as
        _ -> Left ("--rate wants a positive number, got " ++ show r)
    go _ ["--rate"] = Left "--rate wants a number after it"
    go _ (a : _) = Left ("unknown argument " ++ a)

usage :: String -> String
usage prog =
  unlines
    [ "usage: "
        ++ prog
        ++ " [--wireworld | --torus | --walls | --mirror]"
        ++ " [--rate N]",
      "",
      "  --wireworld  a Wireworld circuit on Clamped axes, stepped by the",
      "               same Stencil machinery the game-of-life example uses",
      "",
      "  Langton's ant, the same eleven lines on three different boards:",
      "  --torus      Periodic 101  -- off one edge and back on the other",
      "  --walls      Clamped 101   -- into the wall, and stays there",
      "  --mirror     Reflective 101 -- bounces, and the heading reverses",
      "               (--ant is a synonym for --torus, the default)",
      "",
      "  --rate N     generations or steps per second"
    ]

main :: IO ()
main = do
  args <- getArgs
  prog <- getProgName
  case parseArgs args of
    Left err -> do
      hPutStrLn stderr (err ++ "\n\n" ++ usage prog)
      exitFailure
    Right opts ->
      case optDemo opts of
        Wireworld -> Wireworld.run (maybe 10 id (optRate opts))
        Ant t -> Ant.run t (maybe 400 id (optRate opts))
