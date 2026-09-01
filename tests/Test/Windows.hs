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

import Control.Comonad (extract)
import Control.Lens (itoListOf, toListOf)
import Data.Foldable (toList)
import Data.Grid.Sized
import Data.Maybe (fromJust, isNothing)
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

-- * Moving inside a window (sized-grid-i0ob.2)

-- | The window of @[1 .. 9]@ at offset 1: @[2, 3, 4]@, on an 'Ordinal' axis.
windowOfNine :: Grid '[Ordinal 3] Int
windowOfNine = gridWindows @3 nine !! 1
  where
    nine = fromJust (gridFromList [1 .. 9]) :: Grid '[Periodic 9] Int

-- | A coordinate on that window's single axis.
ord1 :: Int -> Coord '[Ordinal 3]
ord1 i = fromJust (numToOrdinal i) :| EmptyCoord

-- | The corresponding one-dimensional step.
d1 :: Int -> Delta (MapStep '[Ordinal 3])
d1 a = a :^ NoDelta

-- | The 3x3 window of a doubly-'Periodic' 9x9 at offset (1, 1): the shape the
-- issue reproduced against, both axes 'Ordinal'.
windowOf9x9 :: Grid '[Ordinal 3, Ordinal 3] Int
windowOf9x9 = shrinkGrid offset src
  where
    offset = one :| one :| EmptyCoord :: Coord '[Ordinal 7, Ordinal 7]
    one = fromJust (numToOrdinal (1 :: Int))
    src =
      fromJust (gridFromList [[9 * r + c + 1 | c <- [0 .. 8]] | r <- [0 .. 8]]) ::
        Grid '[Periodic 9, Periodic 9] Int

ord2 :: Int -> Int -> Coord '[Ordinal 3, Ordinal 3]
ord2 r c = fromJust (numToOrdinal r) :| fromJust (numToOrdinal c) :| EmptyCoord

d2 :: Int -> Int -> Delta (MapStep '[Ordinal 3, Ordinal 3])
d2 a b = a :^ b :^ NoDelta

-- | Checked movement inside a window: the capability the window's own type is
-- chosen to promise.
--
-- 'Data.Grid.Sized.Coord.offsetCoord' and everything built on it used to be
-- indexed by @'MapDiff' cs@, which is stuck on 'Ordinal' -- so none of these
-- calls typechecked, and a window was a grid nothing could move in. They are
-- indexed by 'MapStep' now. The assertions are about what the window
-- /answers/: a step that stays inside it succeeds, and a step that leaves it
-- is 'Nothing' rather than the source's wrap.
movementInsideAWindow :: TestTree
movementInsideAWindow =
  testGroup
    "checked movement inside a window"
    [ testCase "the window under test" $
        assertEqual "" [2, 3, 4] (toList windowOfNine),
      testCase "a step that stays inside the window succeeds" $
        assertEqual "" (Just (ord1 1)) (offsetCoord (ord1 0) (d1 1)),
      -- The source is Periodic 9, so the same step at the same cell wraps
      -- there. The window is not, and does not.
      testCase "a step off the window's far edge is Nothing, not a wrap" $
        assertEqual "" Nothing (offsetCoord (ord1 2) (d1 1)),
      testCase "a step off the window's near edge is Nothing, not a wrap" $
        assertEqual "" Nothing (offsetCoord (ord1 0) (d1 (-1))),
      -- A ray on the Periodic source never ends; on the window it ends at the
      -- window's own edge, which is the whole point of the Ordinal axis.
      testCase "a ray stops at the window's edge" $
        assertEqual "" [ord1 1, ord1 2] (coordRay (ord1 0) (d1 1)),
      testCase "the same ray on the Periodic source does not stop" $
        assertEqual
          ""
          20
          (length (take 20 (coordRay (zeroCoord :: Coord '[Periodic 9]) (1 :^ NoDelta)))),
      testCase "offsetCoordUpTo reports where the walk left the window" $
        assertEqual
          ""
          (Left (OffGrid (ord1 2) 2))
          (offsetCoordUpTo 5 (ord1 0) (d1 1)),
      testCase "a path that leaves the window and comes back still fails" $
        assertEqual
          ""
          Nothing
          (walkPath (ord1 0) (Path [d1 (-1), d1 1])),
      testCase "a path that stays inside the window succeeds" $
        assertEqual
          ""
          (Just (ord1 2))
          (walkPath (ord1 0) (Path [d1 1, d1 1])),
      testCase "traceOffset reads the window's own cells" $
        assertEqual
          ""
          (Just 3)
          (traceOffset (d1 1) (focusedAtZero windowOfNine)),
      testCase "traceOffset off the window's edge is Nothing" $
        assertEqual
          ""
          Nothing
          (traceOffset (d1 (-1)) (focusedAtZero windowOfNine)),
      testGroup
        "the same, on the two-axis window the issue reproduced against"
        [ testCase "the window under test" $
            assertEqual "" [11, 12, 13, 20, 21, 22, 29, 30, 31] (toList windowOf9x9),
          testCase "a diagonal step inside the window succeeds" $
            assertEqual "" (Just (ord2 1 1)) (offsetCoord (ord2 0 0) (d2 1 1)),
          testCase "either axis can refuse on its own" $ do
            assertEqual "row" Nothing (offsetCoord (ord2 2 0) (d2 1 0))
            assertEqual "column" Nothing (offsetCoord (ord2 0 2) (d2 0 1)),
          testCase "walkEverywhere is defined on an Ordinal-axed grid" $
            assertEqual
              ""
              (Just 21)
              ( extract
                  (walkEverywhere (Path [d2 1 1]) (focusedAtZero windowOf9x9))
              ),
          -- sized-grid-qbal: a Walker can be written down and stepped inside a
          -- window --- its heading is a MapStep now, not the stuck
          -- Diff (Ordinal 3). It steps in bounds and stops at the window's own
          -- edge, never wrapping to the Periodic source it was cut from.
          testCase "a walker steps across the window and stops at its edge" $
            let w0 =
                  Walker
                    (FocusedGrid windowOf9x9 (ord2 0 0))
                    (d2 0 1)
                    identityFrame
             in do
                  assertEqual
                    "the row it walks"
                    [11, 12, 13]
                    (map (extract . walkerGrid) (walkerTrail w0))
                  assertBool "the checked step never turns the heading" $
                    all ((== d2 0 1) . walkerHeading) (walkerTrail w0),
          testCase "a walker step off the window's edge is Nothing, not a wrap" $
            assertBool "" $
              isNothing
                ( stepWalkerWithin
                    (Walker (FocusedGrid windowOf9x9 (ord2 0 2)) (d2 0 1) identityFrame)
                )
        ]
    ]

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

windowsCarryOffsets :: Property
windowsCarryOffsets =
  map (fmap toList) (itoListOf (windows @3) sourceOfFive)
    === [ (unsafeOrdinal 0 :| EmptyCoord, [1, 2, 3]),
          (unsafeOrdinal 1 :| EmptyCoord, [2, 3, 4]),
          (unsafeOrdinal 2 :| EmptyCoord, [3, 4, 5])
        ]

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
      testProperty "windows carry their offsets" windowsCarryOffsets,
      movementInsideAWindow,
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
