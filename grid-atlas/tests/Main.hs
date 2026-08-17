module Main
  ( main
  ) where

import           Test.Tasty
import           Test.CubeMap
import           Test.Tiles

main :: IO ()
main = defaultMain (testGroup "grid-atlas" [tilesTests, cubeMapTests])
