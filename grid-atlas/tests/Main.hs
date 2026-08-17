module Main
  ( main
  ) where

import           Test.Tasty
import           Test.Tiles

main :: IO ()
main = defaultMain tilesTests
