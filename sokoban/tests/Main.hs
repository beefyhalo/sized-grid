module Main (main) where

import Test.Band (bandTests)
import Test.Levels (levelTests)
import Test.Rules (ruleTests)
import Test.Seam (seamTests)
import Test.Shell (shellTests)
import Test.Surface (surfaceTests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain (testGroup "sokoban" [seamTests, surfaceTests, ruleTests, levelTests, bandTests, shellTests])
