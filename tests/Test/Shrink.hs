{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Shrink where

import           Data.Maybe       (fromJust)
import           SizedGrid
import           Test.Tasty
import           Test.Tasty.HUnit

exampleGrid :: Grid '[Ordinal 3, Ordinal 3] Int
exampleGrid = fromJust $ gridFromList [[1,2,3],[4,5,6],[7,8,9]]

focusCenter :: Grid '[ Ordinal 1, Ordinal 1] Int
focusCenter =
    let c :: Coord '[Ordinal 3, Ordinal 3] =
            fromJust $
            (\x y -> x :| y :| EmptyCoord) <$> numToOrdinal (1 :: Int) <*>
            numToOrdinal (1 :: Int)
     in shrinkGrid c exampleGrid

-- | A window that is genuinely smaller than its source, and where the number of
-- positions differs from the source size: x = 3 positions, y = 5 source, z = 3
-- window. The old @z <= x - y + 1@ constraint rejected this outright (3 - 5
-- truncates to 0 in Nat, leaving @z <= 1@), which is how the transposed
-- constraint went unnoticed -- every existing case had x == y.
sourceOfFive :: Grid '[ Ordinal 5] Int
sourceOfFive = fromJust $ gridFromList [1, 2, 3, 4, 5]

windowAt :: Int -> Grid '[ Ordinal 3] Int
windowAt n =
  let c :: Coord '[ Ordinal 3]
      c = fromJust $ (:| EmptyCoord) <$> numToOrdinal n
   in shrinkGrid c sourceOfFive

shrinkTests :: TestTree
shrinkTests =
  testGroup
    "Shrinking"
    [ testCase "Focus Center" $
      assertEqual "Focus Center" focusCenter $ fromJust (gridFromList [[5]])
    , testCase "window of 3 over a source of 5, offset 0" $
      assertEqual "offset 0" (windowAt 0) $ fromJust (gridFromList [1, 2, 3])
    , testCase "window of 3 over a source of 5, offset 1" $
      assertEqual "offset 1" (windowAt 1) $ fromJust (gridFromList [2, 3, 4])
    , testCase "window of 3 over a source of 5, offset 2" $
      assertEqual "offset 2" (windowAt 2) $ fromJust (gridFromList [3, 4, 5])
    ]
