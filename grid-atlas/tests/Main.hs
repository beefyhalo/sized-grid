module Main
  ( main
  ) where

import           Test.CubeMap
import           Test.Frames
import           Test.Klein
import           Test.Mobius
import           Test.Projective
import           Test.Tiles
import           Test.VertexCycles

import           Test.Tasty

main :: IO ()
main =
    defaultMain
        (testGroup
             "grid-atlas"
             [ tilesTests
             , cubeMapTests
             , mobiusTests
             , kleinTests
             , projectiveTests
             , frameTests
             , vertexCycleTests
             ])
