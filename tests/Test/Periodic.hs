module Test.Periodic
  ( periodicTests
  ) where

import           Data.Grid.Sized

import           Data.Group          (generated')
import           Test.Tasty
import           Test.Tasty.HUnit

periodicTests :: TestTree
periodicTests =
  testGroup
    "Periodic"
    [ testGroup "Enum stops after one lap instead of repeating forever" enumTests
    , testGroup "Cyclic: 1 generates every element, once" cyclicTests
    ]

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

-- | 'Data.Group.generated'' terminates (finite group, 'Eq' stops it) and
-- lands on the same lap 'enumFrom' does, starting at the generator: '1' is
-- the additive step, exactly what @(':|')@-free translation is.
cyclicTests :: [TestTree]
cyclicTests =
  [ testCase "generated' matches one full lap from 1" $
    assertEqual
      ""
      (map fromEnum (enumFrom (toEnum 1 :: Periodic 7)))
      (map fromEnum (generated' :: [Periodic 7]))
  , testCase "generated' visits every element exactly once" $
    assertEqual "" 7 (length (generated' :: [Periodic 7]))
  ]
