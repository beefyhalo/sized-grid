module Main (main) where

import Sudoku.Board (displayBoard, exampleBoard)
import Sudoku.Solve (solveBoard)
import System.Exit (exitFailure)

main :: IO ()
main =
  case solveBoard exampleBoard of
    Just board
      | displayBoard board == solvedExample -> pure ()
    _ -> exitFailure

solvedExample :: String
solvedExample =
  unlines
    [ "483921657",
      "967345821",
      "251876493",
      "548132976",
      "729564138",
      "136798245",
      "372689514",
      "814253769",
      "695417382"
    ]
