{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for 'gridWindows': the sliding-window counterpart to 'gridTiles'.
-- 'gridTiles' cuts a grid into disjoint tiles; 'gridWindows' gives every
-- overlapping window at stride 1.
module Test.Windows
  ( windowTests,
  )
where

import Control.Lens (toListOf)
import Data.Foldable (toList)
import Data.Grid.Sized
import Data.Maybe (fromJust)
import Test.Arbitrary ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
  ( Property,
    choose,
    forAll,
    property,
    testProperty,
    (===),
  )

sourceOfFive :: Grid '[Ordinal 5] Int
sourceOfFive = fromJust $ gridFromList [1, 2, 3, 4, 5]

windowsOfThree :: [Grid '[Ordinal 3] Int]
windowsOfThree = gridWindows sourceOfFive

-- | 'gridWindows' is exactly 'shrinkGrid' applied at every offset the window
-- size admits.
windowIsShrinkGrid :: Grid '[Ordinal 5] Int -> Property
windowIsShrinkGrid src =
  let expected =
        [shrinkGrid (o :| EmptyCoord) src | o <- [minBound .. maxBound :: Ordinal 3]]
   in gridWindows @3 src === expected

-- | The same law with a second axis present and left alone: keeping an axis at
-- its own size takes the singleton offset 'Ordinal' 1, the only offset that
-- does not shrink it.
windowIsShrinkGrid2D :: Grid '[Ordinal 5, Ordinal 3] Int -> Property
windowIsShrinkGrid2D src =
  let expected =
        [ shrinkGrid (o :| (minBound :: Ordinal 1) :| EmptyCoord) src
        | o <- [minBound .. maxBound :: Ordinal 3]
        ]
   in gridWindows @3 src === expected

-- | The same law again, at a source that is not already 'Ordinal'-axed.
--
-- This is what sized-grid-mbh0 bought. 'shrinkGrid' used to force the
-- window's axis type to equal the source's while 'gridWindows' left it free,
-- so the two agreed only where the source was 'Ordinal' to begin with and the
-- law above was the whole law that could be written. Both now return the same
-- 'Ordinal'-axed window whatever the source's policy is, so the law is
-- statable at every policy, and these two instantiations say so.
windowIsShrinkGridPeriodic :: Grid '[Periodic 5] Int -> Property
windowIsShrinkGridPeriodic src =
  gridWindows @3 src
    === [shrinkGrid (o :| EmptyCoord) src | o <- [minBound .. maxBound :: Ordinal 3]]

windowIsShrinkGridClamped :: Grid '[Clamped 5] Int -> Property
windowIsShrinkGridClamped src =
  gridWindows @3 src
    === [shrinkGrid (o :| EmptyCoord) src | o <- [minBound .. maxBound :: Ordinal 3]]

-- | A window has no topology of its own, so a step that leaves it is
-- 'Nothing' --- not the source's wrap, and not the source's wall.
--
-- The window is a faithful view: any step that stays inside it reads the same
-- cell the source does at the corresponding position. Any step that leaves it
-- fails, and fails for exactly one reason --- the arithmetic left @[0, 3)@ ---
-- rather than quietly landing somewhere else.
--
-- The second half is the bug. Under the old types a window of a
-- @'Periodic' 9@ was a @Grid '['Periodic' 3]@, so one step left of its first
-- cell wrapped round to its /last/, reading 4 where the same step in the
-- source reads 1; a window of a @'Clamped' 9@ stood still at a wall the
-- source does not have there. Both answers were silent.
windowHasNoSeamOfItsOwn :: Grid '[Periodic 9] Int -> Ordinal 7 -> Ordinal 3 -> Property
windowHasNoSeamOfItsOwn src k o =
  forAll (choose (-4, 4 :: Int)) $ \d ->
    let win = gridWindows @3 src !! ordinalToInt k
        target = ordinalToInt o + d
     in case offsetIsCoord o d of
          Just o' ->
            toList win !! ordinalToInt o'
              === toList src !! (ordinalToInt k + ordinalToInt o')
          Nothing -> property (target < 0 || target >= 3)

-- | A window is a slice: 'gridWindows' produces exactly the length-3 runs
-- @take 3 . drop n@ would, one for every valid @n@ in order.
windowsAreSlices :: Grid '[Ordinal 5] Int -> Property
windowsAreSlices src =
  map toList (gridWindows src :: [Grid '[Ordinal 3] Int])
    === [take 3 (drop n (toList src)) | n <- [0 .. 2]]

-- | 'windows' as a getter agrees with 'gridWindows'. There is no 'over' law
-- to check here, on purpose: 'windows' is a 'Fold', not a 'Traversal', because
-- its foci overlap (see the module Haddock on 'Data.Grid.Sized.windows') --
-- there is no lawful write-back to test.
windowsIsGridWindows :: Grid '[Ordinal 5] Int -> Property
windowsIsGridWindows src =
  toListOf (windows @3) src === gridWindows @3 src

windowTests :: TestTree
windowTests =
  testGroup
    "Windows"
    [ testCase "a window of 3 over a source of 5 gives 3 - 5 + 1 = 3 windows" $
        assertEqual "count" 3 (length windowsOfThree),
      testCase "the windows overlap, offset by offset" $
        assertEqual
          "windows"
          [[1, 2, 3], [2, 3, 4], [3, 4, 5]]
          (map toList windowsOfThree),
      testProperty "a window is take 3 . drop n" windowsAreSlices,
      testProperty "gridWindows agrees with shrinkGrid, 1D" windowIsShrinkGrid,
      testProperty
        "gridWindows agrees with shrinkGrid, 2D with the second axis fixed"
        windowIsShrinkGrid2D,
      testProperty "toListOf windows == gridWindows" windowsIsGridWindows,
      testGroup
        "a restriction destroys the boundary policy"
        [ testProperty
            "gridWindows agrees with shrinkGrid over a Periodic source"
            windowIsShrinkGridPeriodic,
          testProperty
            "gridWindows agrees with shrinkGrid over a Clamped source"
            windowIsShrinkGridClamped,
          testProperty
            "a step out of a window is Nothing, not the source's wrap"
            windowHasNoSeamOfItsOwn,
          testCase "the window of a periodic source has no seam of its own" $ do
            let nine = fromJust (gridFromList [1 .. 9]) :: Grid '[Periodic 9] Int
                win = gridWindows @3 nine !! 1 :: Grid '[Ordinal 3] Int
            assertEqual "the window itself" [2, 3, 4] (toList win)
            -- One step left of the window's first cell. The source has a cell
            -- there (value 1); the window does not, and says so.
            assertEqual
              "one step left of the window's first cell"
              Nothing
              (offsetIsCoord (minBound :: Ordinal 3) (-1))
            assertEqual
              "one step right of the window's last cell"
              Nothing
              (offsetIsCoord (maxBound :: Ordinal 3) 1)
        ]
    ]
