module Main
  ( main
  ) where

import           Test.Tasty
import           Test.CubeMap
import           Test.Klein
import           Test.Mobius
import           Test.Projective
import           Test.Tiles
import           Test.VertexCycles

main :: IO ()
main = defaultMain (testGroup "grid-atlas" [tilesTests, cubeMapTests, mobiusTests, kleinTests, projectiveTests, vertexCycleTests])
