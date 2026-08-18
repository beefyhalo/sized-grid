module Main
  ( main
  ) where

import           Test.Seam
import           Test.Tasty

main :: IO ()
main = defaultMain (testGroup "atlas-topology" [seamTests])
