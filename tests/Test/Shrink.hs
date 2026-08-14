{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Shrink
  ( shrinkTests
  ) where

import           Data.Foldable         (toList)
import           Data.Maybe            (fromJust)
import           Data.Grid.Sized
import           Test.Arbitrary        ()
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (Property, testProperty, (===))

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

-- | What the four cases above check one offset at a time, stated once for every
-- offset and every source.
--
-- A window is a slice: @shrinkGrid@ at offset @n@ must hand back exactly the
-- cells @drop n@ then @take z@ would. The example-based cases pin down three
-- specific slices of one specific grid, which leaves the contents free to be
-- transposed, reversed or read from the wrong end as long as those three
-- happen to come out right.
windowIsSlice :: Grid '[ Ordinal 5] Int -> Ordinal 3 -> Property
windowIsSlice src o =
  let win = shrinkGrid (o :| EmptyCoord) src :: Grid '[ Ordinal 3] Int
   in toList win === take 3 (drop (ordinalToNum o) (toList src))

-- | The same law in two dimensions, where a window is a submatrix rather than a
-- slice and both axes have to be offset independently.
--
-- This is the shape the @x + z <= y + 1@ constraint was got wrong for: with a
-- 4x4 source, a 2x2 window and 3x3 offsets, the two axes carry different
-- numbers so a transposed index cannot pass by coincidence. It also fixes the
-- orientation, which nothing else does -- @collapseGrid@ produces rows, so the
-- first coordinate has to be the one that selects among them.
windowIsSubmatrix ::
     Grid '[ Ordinal 4, Ordinal 4] Int
  -> Coord '[ Ordinal 3, Ordinal 3]
  -> Property
windowIsSubmatrix src off =
  let win = shrinkGrid off src :: Grid '[ Ordinal 2, Ordinal 2] Int
      (i :| j :| EmptyCoord) = off
      rows = drop (ordinalToNum i) (collapseGrid src)
   in collapseGrid win === map (take 2 . drop (ordinalToNum j)) (take 2 rows)

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
    , testProperty "a 1D window at offset n is take 3 . drop n" windowIsSlice
    , testProperty
        "a 2D window at (i, j) is the 2x2 submatrix there"
        windowIsSubmatrix
    ]
