{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}

-- | For @Grid cs a@, the underlying vector holds exactly @MaxCoordSize cs@ elements.
module Test.Invariant
  ( invariantTests
  ) where

import           Data.Grid.Sized
import           Data.Grid.Sized.Unsafe (unsafeGridFromVector)

import           Control.Lens         (over)
import           Data.Aeson           (decode, encode)
import           Data.ByteString.Lazy (ByteString)
import           Data.Foldable        (toList)
import           Data.Functor.Identity (Identity (..))
import           Data.Maybe           (fromJust, isNothing)
import           Data.Functor.Rep (tabulate)
import           Data.Kind         (Type)
import           Data.Proxy
import qualified Data.Vector          as V
import           GHC.TypeLits
import           Test.Tasty
import           Test.Tasty.HUnit

assertWellSized ::
     forall cs a. KnownNat (MaxCoordSize cs)
  => String
  -> Grid cs a
  -> Assertion
assertWellSized what g =
  assertEqual
    (what ++ ": vector length must equal MaxCoordSize")
    (fromIntegral (natVal (Proxy @(MaxCoordSize cs))) :: Int)
    (V.length (gridVector g))

assertRejects ::
     forall (cs :: [Type]). AllSizedKnown cs
  => String
  -> ByteString
  -> Assertion
assertRejects what bs =
  assertBool
    (what ++ ": malformed JSON must not decode")
    (isNothing (decode bs :: Maybe (Grid cs Int)))

threeByThree :: Grid '[ Ordinal 3, Ordinal 3] Int
threeByThree = fromJust $ gridFromList [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

oneByThree :: Grid '[ Ordinal 3] Int
oneByThree = fromJust $ gridFromList [1, 2, 3]

twoByThree :: Grid '[ Ordinal 2, Ordinal 3] Int
twoByThree = fromJust $ gridFromList [[10, 11, 12], [13, 14, 15]]

-- | Three distinct sizes and three axes, so a transposition bug can't hide and 'scanAxis' has a middle axis to reach.
twoByThreeByTwo :: Grid '[ Ordinal 2, Ordinal 3, Ordinal 2] Int
twoByThreeByTwo =
  fromJust $
  gridFromList
    [ [[0, 1], [10, 11], [20, 21]]
    , [[100, 101], [110, 111], [120, 121]]
    ]

assertAllWellSized ::
     forall cs a. KnownNat (MaxCoordSize cs)
  => String
  -> [Grid cs a]
  -> Assertion
assertAllWellSized what =
  mapM_ (\(n, g) -> assertWellSized (what ++ " [" ++ show n ++ "]") g) .
  zip [0 :: Int ..]

invariantTests :: TestTree
invariantTests =
  testGroup
    "Size invariant"
    [ testGroup
        "Well-formed constructions hold the invariant"
        [ testCase "gridFromList 3x3" $ assertWellSized "gridFromList" threeByThree
        , testCase "tabulate 3x3" $
          assertWellSized "tabulate" (tabulate (const (0 :: Int)) :: Grid '[ Ordinal 3, Ordinal 3] Int)
        , testCase "pure 3x3" $
          assertWellSized "pure" (pure (0 :: Int) :: Grid '[ Ordinal 3, Ordinal 3] Int)
        , testCase "fmap preserves size" $
          assertWellSized "fmap" (fmap (+ 1) threeByThree)
        , testCase "round-tripped JSON" $
          assertWellSized "decode . encode" $
          fromJust (decode (encode threeByThree) :: Maybe (Grid '[ Ordinal 3, Ordinal 3] Int))
        ]
    , testGroup
        "Malformed JSON must be rejected"
        [ testCase "too few rows" $
          assertRejects @'[ Ordinal 3, Ordinal 3] "too few rows" "[[1,2,3],[4,5,6]]"
        , testCase "too many rows" $
          assertRejects @'[ Ordinal 3, Ordinal 3]
            "too many rows"
            "[[1,2,3],[4,5,6],[7,8,9],[10,11,12]]"
        , testCase "ragged rows" $
          assertRejects @'[ Ordinal 3, Ordinal 3] "ragged rows" "[[1,2],[3,4],[5,6]]"
        ]
    , testGroup
        "take/split hold the invariant they now promise"
        [ testCase "takeGrid within the source length" $
          assertWellSized "takeGrid 2 of a 3-grid" (takeGrid 2 oneByThree :: Grid '[ Ordinal 2] Int)
        , testCase "dropGrid within the source length" $
          assertWellSized "dropGrid 2 of a 3-grid" (dropGrid 2 oneByThree :: Grid '[ Ordinal 1] Int)
        , testCase "splitHigherDim remainder is forced to x - y" $
          let (_ :: Grid '[ Ordinal 1, Ordinal 3] Int, b) = splitHigherDim threeByThree
           in assertWellSized "splitHigherDim snd" (b :: Grid '[ Ordinal 2, Ordinal 3] Int)
        ]
      -- These compute a length from MaxCoordSize by hand, so an off-by-one would silently break the invariant.
    , testGroup
        "The shape-changing operations hold the invariant"
        [ testCase "splitGrid's outer grid" $
          assertWellSized
            "splitGrid outer"
            (splitGrid threeByThree :: Grid '[ Ordinal 3] (Grid '[ Ordinal 3] Int))
        , testCase "splitGrid's sub-grids" $
          assertAllWellSized
            "splitGrid sub"
            (toList (splitGrid threeByThree :: Grid '[ Ordinal 3] (Grid '[ Ordinal 3] Int)))
        , testCase "combineGrid" $
          assertWellSized
            "combineGrid"
            (combineGrid (splitGrid threeByThree) :: Grid '[ Ordinal 3, Ordinal 3] Int)
          -- Result size is neither argument's: 3 + 2 rows of 3 = 15 cells.
        , testCase "combineHigherDim sums the outer axis" $
          assertWellSized
            "combineHigherDim"
            (combineHigherDim threeByThree twoByThree :: Grid '[ Ordinal 5, Ordinal 3] Int)
        , testCase "splitHigherDim's first component" $
          let (a :: Grid '[ Ordinal 1, Ordinal 3] Int, _) = splitHigherDim threeByThree
           in assertWellSized "splitHigherDim fst" a
        , testCase "mapLowerDim" $
          assertWellSized
            "mapLowerDim"
            (runIdentity (mapLowerDim (Identity . scanl1Grid (+)) threeByThree))
        , testCase "gridTiles' tiles" $
          assertAllWellSized
            "gridTiles"
            (gridTiles threeByThree :: [Grid '[ Ordinal 1, Ordinal 3] Int])
        , testCase "zipLowerDim's tiles" $
          assertAllWellSized
            "zipLowerDim"
            (zipLowerDim gridTiles threeByThree :: [Grid '[ Ordinal 3, Ordinal 1] Int])
        , testCase "transposeGrid" $
          assertWellSized "transposeGrid" (transposeGrid twoByThree)
        , testCase "mapAxis 0 on a 3D grid" $
          assertWellSized "mapAxis 0" (mapAxis 0 id twoByThreeByTwo)
        , testCase "mapAxis 1 on a 3D grid" $
          assertWellSized "mapAxis 1" (mapAxis 1 id twoByThreeByTwo)
        , testCase "mapAxis 2 on a 3D grid" $
          assertWellSized "mapAxis 2" (mapAxis 2 id twoByThreeByTwo)
        , testCase "shrinkGrid" $
          let off :: Coord '[ Ordinal 2, Ordinal 2]
              off = fromJust $ (\x y -> x :| y :| EmptyCoord)
                      <$> numToOrdinal (1 :: Int) <*> numToOrdinal (1 :: Int)
           in assertWellSized
                "shrinkGrid"
                (shrinkGrid off threeByThree :: Grid '[ Ordinal 2, Ordinal 2] Int)
        ]
    , testGroup
        "gridFromVector checks the length the constructor used to assume"
        [ testCase "a vector of exactly MaxCoordSize is accepted" $
          assertEqual
            "9 elements build the 3x3"
            (Just threeByThree)
            (gridFromVector (V.fromList [1 .. 9]))
        , testCase "an accepted vector holds the invariant" $
          assertWellSized "gridFromVector" $
          fromJust (gridFromVector (V.fromList [1 .. 9]) :: Maybe (Grid '[ Ordinal 3, Ordinal 3] Int))
        , testCase "too few elements are rejected" $
          assertBool "8 elements must not build a 3x3" $
          isNothing (gridFromVector (V.fromList [1 .. 8]) :: Maybe (Grid '[ Ordinal 3, Ordinal 3] Int))
        , testCase "too many elements are rejected" $
          assertBool "10 elements must not build a 3x3" $
          isNothing (gridFromVector (V.fromList [1 .. 10]) :: Maybe (Grid '[ Ordinal 3, Ordinal 3] Int))
        , testCase "gridVector is the left inverse of gridFromVector" $
          let v = V.fromList [1 .. 9] :: V.Vector Int
           in assertEqual
                "round trip"
                v
                (gridVector (fromJust (gridFromVector v :: Maybe (Grid '[ Ordinal 3, Ordinal 3] Int))))
        ]
      -- The only way to build a Grid from a raw vector without a length check, so it must be an identity on the representation.
    , testCase "unsafeGridFromVector round-trips through gridVector" $
      assertEqual
        "unsafeGridFromVector . gridVector == id"
        threeByThree
        (unsafeGridFromVector (gridVector threeByThree))
    , testGroup
        "scanl1Grid is length-preserving, so it needs no escape hatch"
        [ testCase "scans a one-dimensional grid" $
          assertEqual
            "running sums of 1,2,3"
            (gridFromList [1, 3, 6] :: Maybe (Grid '[ Ordinal 3] Int))
            (Just (scanl1Grid (+) oneByThree))
        , testCase "preserves the invariant on a 3x3" $
          assertWellSized "scanl1Grid" (scanl1Grid (+) threeByThree)
        , testCase "mapLowerDim gives per-row prefix sums" $
          assertEqual
            "each row scanned independently"
            (gridFromList [[1, 3, 6], [4, 9, 15], [7, 15, 24]] :: Maybe (Grid '[ Ordinal 3, Ordinal 3] Int))
            (Just (runIdentity (mapLowerDim (Identity . scanl1Grid (+)) threeByThree)))
        ]
    , testGroup
        "scanAxis names any axis, not just the outermost"
        [ testCase "scanAxis 1 agrees with mapLowerDim . scanl1Grid on 2D" $
          assertEqual
            "scanning across each row"
            (runIdentity (mapLowerDim (Identity . scanl1Grid (+)) threeByThree))
            (scanAxis 1 (+) threeByThree)
        , testCase "scanAxis 0 scans down each column" $
          assertEqual
            "cumulative sums down each column"
            (gridFromList [[1, 2, 3], [5, 7, 9], [12, 15, 18]] :: Maybe (Grid '[ Ordinal 3, Ordinal 3] Int))
            (Just (scanAxis 0 (+) threeByThree))
        , testCase "scanAxis 1 reaches the middle axis of a 3D grid" $
          -- transposeGrid can't: no 2-axis swap brings a middle axis to the edge alone.
          assertEqual
            "scanning the middle axis, independently for every (outer, inner) pair"
            (gridFromList
               [ [[0, 1], [10, 12], [30, 33]]
               , [[100, 101], [210, 212], [330, 333]]
               ] :: Maybe (Grid '[ Ordinal 2, Ordinal 3, Ordinal 2] Int))
            (Just (scanAxis 1 (+) twoByThreeByTwo))
        , testCase "scanAxis 0 scans the outermost axis of a 3D grid" $
          assertEqual
            "scanning the outer axis, independently for every (middle, inner) pair"
            (gridFromList
               [ [[0, 1], [10, 11], [20, 21]]
               , [[100, 102], [120, 122], [140, 142]]
               ] :: Maybe (Grid '[ Ordinal 2, Ordinal 3, Ordinal 2] Int))
            (Just (scanAxis 0 (+) twoByThreeByTwo))
        , testCase "scanAxis 2 scans the innermost axis of a 3D grid" $
          assertEqual
            "scanning the inner axis, independently for every (outer, middle) pair"
            (gridFromList
               [ [[0, 1], [10, 21], [20, 41]]
               , [[100, 201], [110, 221], [120, 241]]
               ] :: Maybe (Grid '[ Ordinal 2, Ordinal 3, Ordinal 2] Int))
            (Just (scanAxis 2 (+) twoByThreeByTwo))
        ]
      -- The setter laws are in Main; what is checked here is that the optic
      -- reaches the same fibres the function does, on every axis of a grid
      -- whose three sizes are distinct.
    , testGroup
        "axis is mapAxis wearing an optic"
        [ testCase "over (axis 0) agrees with mapAxis 0" $
          assertEqual
            "outermost axis"
            (mapAxis 0 (scanl1Grid (+)) twoByThreeByTwo)
            (over (axis 0) (scanl1Grid (+)) twoByThreeByTwo)
        , testCase "over (axis 1) agrees with mapAxis 1" $
          assertEqual
            "middle axis"
            (mapAxis 1 (scanl1Grid (+)) twoByThreeByTwo)
            (over (axis 1) (scanl1Grid (+)) twoByThreeByTwo)
        , testCase "over (axis 2) agrees with mapAxis 2" $
          assertEqual
            "innermost axis"
            (mapAxis 2 (scanl1Grid (+)) twoByThreeByTwo)
            (over (axis 2) (scanl1Grid (+)) twoByThreeByTwo)
        , testCase "scanAxis n f is over (axis n) (scanl1Grid f)" $
          assertEqual
            "the identity the Haddock claims"
            (scanAxis 1 (+) twoByThreeByTwo)
            (over (axis 1) (scanl1Grid (+)) twoByThreeByTwo)
        , testCase "over (axis 1) preserves the invariant" $
          assertWellSized
            "over (axis 1)"
            (over (axis 1) (scanl1Grid (+)) twoByThreeByTwo)
        ]
    ]
