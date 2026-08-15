{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for 'gridTiles' and 'zipLowerDim'.
--
-- The 4x4 board here is the stand-in that found the original defect: rows came
-- out right, but columns were built with 'mapLowerDim' in the list applicative,
-- which is a cartesian product -- 256 results instead of 4, and 9^9 on the real
-- 9x9 sudoku board this API exists to slice.
module Test.Tiling
  ( tilingTests
  ) where

import           Data.Foldable    (toList)
import           Data.Maybe       (fromJust)
import           Data.Grid.Sized
import           Test.Tasty
import           Test.Tasty.HUnit

-- | 1..16 laid out row-major.
fourByFour :: Grid '[ Ordinal 4, Ordinal 4] Int
fourByFour =
    fromJust $
    gridFromList [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12], [13, 14, 15, 16]]

nineByNine :: Grid '[ Ordinal 9, Ordinal 9] Int
nineByNine =
    fromJust $ gridFromList [[9 * r + c | c <- [0 .. 8]] | r <- [0 .. 8]]

rows :: Grid '[ Ordinal 4, Ordinal 4] Int -> [Grid '[ Ordinal 1, Ordinal 4] Int]
rows = gridTiles

columns :: Grid '[ Ordinal 4, Ordinal 4] Int -> [Grid '[ Ordinal 4, Ordinal 1] Int]
columns = zipLowerDim gridTiles

-- | Two horizontal bands of two 2x2 squares each, in band-major order.
squares :: Grid '[ Ordinal 4, Ordinal 4] Int -> [Grid '[ Ordinal 2, Ordinal 2] Int]
squares b = do
    band :: Grid '[ Ordinal 2, Ordinal 4] Int <- gridTiles b
    zipLowerDim gridTiles band

-- | The same shapes at the size that matters: on a 9x9 board the buggy
-- combinator produced 387,420,489 results, so a test that merely forces the
-- length is a sufficient regression test -- it would not terminate before.
nineColumns :: [Grid '[ Ordinal 9, Ordinal 1] Int]
nineColumns = zipLowerDim gridTiles nineByNine

tilingTests :: TestTree
tilingTests =
    testGroup
        "Tiling"
        [ testGroup
              "gridTiles cuts the outermost axis into disjoint tiles"
              [ testCase "a 4x4 gives 4 rows" $
                assertEqual "count" 4 (length (rows fourByFour))
              , testCase "the rows are the rows" $
                assertEqual
                    "contents"
                    [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12], [13, 14, 15, 16]]
                    (map toList (rows fourByFour))
              , testCase "the tiles partition the source" $
                assertEqual
                    "concat"
                    (toList fourByFour)
                    (concatMap toList (rows fourByFour))
              ]
        , testGroup
              "zipLowerDim tiles the second axis"
              [ testCase "a 4x4 gives 4 columns, not 4^4" $
                assertEqual "count" 4 (length (columns fourByFour))
              , testCase "the columns are the columns" $
                assertEqual
                    "contents"
                    [[1, 5, 9, 13], [2, 6, 10, 14], [3, 7, 11, 15], [4, 8, 12, 16]]
                    (map toList (columns fourByFour))
              , testCase "a 9x9 gives 9 columns and terminates" $
                assertEqual "count" 9 (length nineColumns)
              , testCase "the 9x9 columns are the columns" $
                assertEqual
                    "first column"
                    [0, 9, 18, 27, 36, 45, 54, 63, 72]
                    (concatMap toList (take 1 nineColumns))
              ]
        , testGroup
              "tiling both axes gives squares in band-major order"
              [ testCase "a 4x4 gives 4 squares" $
                assertEqual "count" 4 (length (squares fourByFour))
              , testCase "the squares are the squares" $
                assertEqual
                    "contents"
                    [ [1, 2, 5, 6]
                    , [3, 4, 7, 8]
                    , [9, 10, 13, 14]
                    , [11, 12, 15, 16]
                    ]
                    (map toList (squares fourByFour))
              ]
          -- Pins the distinction the bug turned on, so that anyone tempted to
          -- "simplify" zipLowerDim back into mapLowerDim sees what changes.
        , testCase "mapLowerDim in the list applicative is a cartesian product" $
          assertEqual
              "4^4"
              256
              (length (mapLowerDim gridTiles fourByFour ::
                           [Grid '[ Ordinal 4, Ordinal 1] Int]))
        ]
