{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Tests for 'gridWindows': the sliding-window counterpart to 'gridTiles'.
-- 'gridTiles' cuts a grid into disjoint tiles; 'gridWindows' gives every
-- overlapping window at stride 1.
module Test.Windows
  ( windowTests
  ) where

import           Control.Lens          (toListOf)
import           Data.Foldable         (toList)
import           Data.Maybe            (fromJust)
import           Data.Grid.Sized
import           Test.Arbitrary        ()
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (Property, testProperty, (===))

sourceOfFive :: Grid '[ Ordinal 5] Int
sourceOfFive = fromJust $ gridFromList [1, 2, 3, 4, 5]

windowsOfThree :: [Grid '[ Ordinal 3] Int]
windowsOfThree = gridWindows sourceOfFive

-- | 'gridWindows' is exactly 'shrinkGrid' applied at every offset the window
-- size admits.
windowIsShrinkGrid :: Grid '[ Ordinal 5] Int -> Property
windowIsShrinkGrid src =
  let expected =
        [shrinkGrid (o :| EmptyCoord) src | o <- [minBound .. maxBound :: Ordinal 3]]
   in gridWindows @(Ordinal 3) src === expected

-- | The same law with a second axis present and left alone: keeping an axis at
-- its own size takes the singleton offset 'Ordinal' 1, the only offset that
-- does not shrink it.
windowIsShrinkGrid2D :: Grid '[ Ordinal 5, Ordinal 3] Int -> Property
windowIsShrinkGrid2D src =
  let expected =
        [ shrinkGrid (o :| (minBound :: Ordinal 1) :| EmptyCoord) src
        | o <- [minBound .. maxBound :: Ordinal 3]
        ]
   in gridWindows @(Ordinal 3) src === expected

-- | A window is a slice: 'gridWindows' produces exactly the length-3 runs
-- @take 3 . drop n@ would, one for every valid @n@ in order.
windowsAreSlices :: Grid '[ Ordinal 5] Int -> Property
windowsAreSlices src =
  map toList (gridWindows src :: [Grid '[ Ordinal 3] Int]) ===
  [take 3 (drop n (toList src)) | n <- [0 .. 2]]

-- | 'windows' as a getter agrees with 'gridWindows'. There is no 'over' law
-- to check here, on purpose: 'windows' is a 'Fold', not a 'Traversal', because
-- its foci overlap (see the module Haddock on 'Data.Grid.Sized.windows') --
-- there is no lawful write-back to test.
windowsIsGridWindows :: Grid '[ Ordinal 5] Int -> Property
windowsIsGridWindows src =
  toListOf (windows @(Ordinal 3)) src === (gridWindows @(Ordinal 3) src)

windowTests :: TestTree
windowTests =
  testGroup
    "Windows"
    [ testCase "a window of 3 over a source of 5 gives 3 - 5 + 1 = 3 windows" $
      assertEqual "count" 3 (length windowsOfThree)
    , testCase "the windows overlap, offset by offset" $
      assertEqual
        "windows"
        [[1, 2, 3], [2, 3, 4], [3, 4, 5]]
        (map toList windowsOfThree)
    , testProperty "a window is take 3 . drop n" windowsAreSlices
    , testProperty "gridWindows agrees with shrinkGrid, 1D" windowIsShrinkGrid
    , testProperty
        "gridWindows agrees with shrinkGrid, 2D with the second axis fixed"
        windowIsShrinkGrid2D
    , testProperty "toListOf windows == gridWindows" windowsIsGridWindows
    ]
