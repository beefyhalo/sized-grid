{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Tests for 'gridTiles', 'tiles' and 'zipLowerDim'.
module Test.Tiling
  ( tilingTests
  ) where

import           Control.Lens      (over, toListOf)
import           Data.Foldable     (toList)
import           Data.Maybe        (fromJust)
import           Data.Grid.Sized
import           Test.Arbitrary    ()
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (Property, testProperty, (===))

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

-- | On a 9x9 board the buggy combinator produced 387,420,489 results, so
-- merely forcing the length is a sufficient regression test.
nineColumns :: [Grid '[ Ordinal 9, Ordinal 1] Int]
nineColumns = zipLowerDim gridTiles nineByNine

-- | 'tiles' as a getter agrees with 'gridTiles'.
tilesIsGridTiles :: Grid '[ Ordinal 4, Ordinal 4] Int -> Property
tilesIsGridTiles g = toListOf (tiles @(Ordinal 1)) g === rows g

-- | 'over' at the identity function is the identity -- the first half of the
-- Traversal laws.
overTilesId :: Grid '[ Ordinal 4, Ordinal 4] Int -> Property
overTilesId g = over (tiles @(Ordinal 1)) id g === g

-- | @'over' l f . 'over' l g == 'over' l (f . g)@ -- the composition law that
-- fails for 'gridWindows' (overlapping foci) but holds here because the tiles
-- are disjoint and covering.
overTilesComposes :: Grid '[ Ordinal 4, Ordinal 4] Int -> Property
overTilesComposes g =
  over (tiles @(Ordinal 1)) negate' (over (tiles @(Ordinal 1)) double g) ===
  over (tiles @(Ordinal 1)) (negate' . double) g
  where
    double = mapGrid (* 2)
    negate' = mapGrid negate

-- | Writing back through the tiles and mapping the whole grid directly agree,
-- since every tile is transformed the same way.
overTilesMatchesMapGrid :: Grid '[ Ordinal 4, Ordinal 4] Int -> Property
overTilesMatchesMapGrid g =
  over (tiles @(Ordinal 1)) (mapGrid (+ 100)) g === mapGrid (+ 100) g

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
          -- Pins the distinction the bug turned on.
        , testCase "mapLowerDim in the list applicative is a cartesian product" $
          assertEqual
              "4^4"
              256
              (length (mapLowerDim gridTiles fourByFour ::
                           [Grid '[ Ordinal 4, Ordinal 1] Int]))
        , testGroup
              "tiles is a lawful Traversal"
              [ testProperty "toListOf tiles == gridTiles" tilesIsGridTiles
              , testProperty "over tiles id == id" overTilesId
              , testProperty
                    "over tiles f . over tiles g == over tiles (f . g)"
                    overTilesComposes
              , testProperty
                    "over tiles (mapGrid f) == mapGrid f"
                    overTilesMatchesMapGrid
              ]
        ]
