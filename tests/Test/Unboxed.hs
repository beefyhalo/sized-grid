{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for the unboxed representation, "Data.Grid.Sized.Unboxed".
--
-- The claim this module exists to check is not that unboxing is fast -- that is
-- the benchmark's job -- but that it is the /same library/. @UGrid@ and @Grid@
-- are one implementation at two vector types, so every shape operation should
-- give an answer indistinguishable from the boxed one, and a size proof that
-- holds for one should hold for the other.
--
-- So each test below runs an operation at both representations and compares the
-- results element by element. A regression in which the two drift apart -- the
-- exact failure mode a separate hand-written unboxed module would have risked --
-- shows up here as a mismatch rather than as a silently wrong grid.
module Test.Unboxed
  ( unboxedTests
  ) where

import           Data.Grid.Sized
import           Data.Grid.Sized.Unboxed (UGrid, ugridFromList)

import           Data.Maybe              (fromJust, isNothing)
import qualified Data.Vector             as V
import qualified Data.Vector.Unboxed     as U
import           Test.Tasty
import           Test.Tasty.HUnit

type Square = '[ Ordinal 4, Ordinal 4]

cells :: [[Int]]
cells = [[4 * r + c | c <- [0 .. 3]] | r <- [0 .. 3]]

boxed :: Grid Square Int
boxed = fromJust $ gridFromList cells

unboxed :: UGrid Square Int
unboxed = fromJust $ ugridFromList cells

-- | The two representations hold the same elements in the same order.
sameAs :: UGrid cs Int -> Grid cs Int -> Assertion
sameAs u b =
    assertEqual
        "unboxed and boxed disagree"
        (V.toList (gridVector b))
        (U.toList (gridVector u))

unboxedTests :: TestTree
unboxedTests =
    testGroup
        "Unboxed"
        [ testGroup
              "construction agrees with the boxed grid"
              [ testCase "ugridFromList lays out row-major, as gridFromList does" $
                unboxed `sameAs` boxed
              , testCase "tabulateGrid agrees with tabulateGrid" $
                (tabulateGrid coordPosition :: UGrid Square Int) `sameAs`
                (tabulateGrid coordPosition :: Grid Square Int)
              , testCase "collapseGrid round-trips back to the nested lists" $
                assertEqual "cells" cells (collapseGrid unboxed)
              , testCase "a ragged list is rejected, as it is when boxed" $
                assertBool
                    "should not build"
                    (isNothing
                         (ugridFromList [[1, 2, 3, 4], [5, 6], [7], []] ::
                              Maybe (UGrid Square Int)))
              ]
        , testGroup
              "the bulk operations agree"
              [ testCase "mapGrid" $
                mapGrid (* 3) unboxed `sameAs` mapGrid (* 3) boxed
              , testCase "imapGrid" $
                imapGrid (\c x -> coordPosition c + x) unboxed `sameAs`
                imapGrid (\c x -> coordPosition c + x) boxed
              , testCase "zipWithGrid" $
                zipWithGrid (+) unboxed unboxed `sameAs`
                zipWithGrid (+) boxed boxed
              , testCase "scanl1Grid" $
                scanl1Grid (+) unboxed `sameAs` scanl1Grid (+) boxed
              , testCase "foldlGrid' totals the same" $
                assertEqual
                    "sum"
                    (foldlGrid' (+) 0 boxed)
                    (foldlGrid' (+) 0 unboxed)
              , testCase "indexGrid reads the same cell at every coordinate" $
                assertEqual
                    "elements"
                    (map (indexGrid boxed) (allCoord @Square))
                    (map (indexGrid unboxed) (allCoord @Square))
              ]
        , testGroup
              "the shared shape algebra works unboxed"
              -- None of these are defined for the unboxed grid separately.
              -- That they typecheck at all is half the test.
              [ testCase "transposeGrid" $
                transposeGrid unboxed `sameAs` transposeGrid boxed
              , testCase "gridTiles cuts the same rows" $
                assertEqual
                    "rows"
                    (map (V.toList . gridVector)
                         (gridTiles boxed :: [Grid '[ Ordinal 1, Ordinal 4] Int]))
                    (map (U.toList . gridVector)
                         (gridTiles unboxed :: [UGrid '[ Ordinal 1, Ordinal 4] Int]))
              , testCase "zipLowerDim gridTiles cuts the same columns" $
                assertEqual
                    "columns"
                    (map (V.toList . gridVector)
                         (zipLowerDim gridTiles boxed ::
                              [Grid '[ Ordinal 4, Ordinal 1] Int]))
                    (map (U.toList . gridVector)
                         (zipLowerDim gridTiles unboxed ::
                              [UGrid '[ Ordinal 4, Ordinal 1] Int]))
              , testCase "shrinkGrid takes the same window" $
                (shrinkGrid (zeroCoord :: Coord '[ Ordinal 2, Ordinal 2]) unboxed ::
                     UGrid '[ Ordinal 3, Ordinal 3] Int) `sameAs`
                shrinkGrid (zeroCoord :: Coord '[ Ordinal 2, Ordinal 2]) boxed
              , testCase "splitGrid then combineGrid is the identity" $
                combineGrid (splitGrid unboxed) `sameAs` boxed
              , testCase "gridFromVector still checks the length" $
                assertBool
                    "should not build"
                    (isNothing
                         (gridFromVector (U.fromList [1, 2, 3]) ::
                              Maybe (UGrid Square Int)))
              ]
        ]
