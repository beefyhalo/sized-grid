module Main
  ( main,
  )
where

import Test.CubeMap
import Test.Frames
import Test.Klein
import Test.Mobius
import Test.Projective
import Test.Tasty
import Test.Tiles
import Test.VertexCycles

main :: IO ()
main =
  defaultMain
    ( testGroup
        "grid-atlas"
        [ tilesTests,
          cubeMapTests,
          mobiusTests,
          kleinTests,
          projectiveTests,
          frameTests,
          vertexCycleTests
        ]
    )
