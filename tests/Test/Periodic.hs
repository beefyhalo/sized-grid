{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module Test.Periodic
  ( periodicTests
  ) where

import           Data.Grid.Sized

import           Test.Tasty
import           Test.Tasty.HUnit

periodicTests :: TestTree
periodicTests =
  testGroup
    "Periodic"
    [testGroup "Enum stops after one lap instead of repeating forever" enumTests]

enumTests :: [TestTree]
enumTests =
  [ testCase "[p ..] enumerates the axis once, starting at p" $
    assertEqual
      ""
      [0, 1, 2, 3]
      (map fromEnum (take 4 (enumFrom (toEnum 0 :: Periodic 4))))
  , testCase "[p ..] starting mid-axis wraps around and stops" $
    assertEqual
      ""
      [2, 3, 0, 1]
      (map fromEnum (take 4 (enumFrom (toEnum 2 :: Periodic 4))))
  , testCase "[p, q ..] steps and stops after one lap" $
    assertEqual
      ""
      [1, 3, 1, 3]
      (map
         fromEnum
         (take 4 (enumFromThen (toEnum 1) (toEnum 3 :: Periodic 4))))
  , testCase "sum [p ..] terminates" $
    assertEqual "" 6 (sum (map fromEnum (enumFrom (toEnum 0 :: Periodic 4))))
  , testCase "length [p ..] terminates" $
    assertEqual "" 4 (length (enumFrom (toEnum 0 :: Periodic 4)))
  ]
