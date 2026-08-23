{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Slice
  ( sliceTests
  ) where

import           Control.Lens      ((.~), (^.), over)
import           Data.Foldable     (toList)
import           Data.Grid.Sized
import           Data.Maybe        (fromJust)
import           Test.Tasty
import           Test.Tasty.HUnit

source :: Grid '[Ordinal 5] Int
source = fromJust $ gridFromList [1, 2, 3, 4, 5]

replacement :: Grid '[Ordinal 2] Int
replacement = fromJust $ gridFromList [20, 30]

sliceTests :: TestTree
sliceTests =
    testGroup
        "Slice lenses"
        [ testCase "slice reads the requested window" $
          assertEqual "slice" [2, 3] (toList (source ^. slice 1 2))
        , testCase "slice replaces only the requested window" $
          assertEqual "slice set" [1, 20, 30, 4, 5]
            (toList ((slice 1 2 .~ replacement) source))
        , testCase "prefix replaces only the prefix" $
          assertEqual "prefix set" [20, 30, 3, 4, 5]
            (toList ((prefix 2 .~ replacement) source))
        , testCase "suffix reads and replaces the suffix" $
          assertEqual "suffix set" [1, 2, 3, 20, 30]
            (toList ((suffix 3 .~ replacement) source))
        , testCase "slice identity and put-get laws" $ do
          assertEqual "identity" source (over (slice 1 2) id source)
          assertEqual "put-get" replacement ((slice 1 2 .~ replacement) source ^. slice 1 2)
        ]
