{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}

-- | The invariant this whole library exists to enforce: for @Grid cs a@, the
-- underlying vector holds exactly @MaxCoordSize cs@ elements.
--
-- Nothing checked that before. These tests pin it down at the boundaries where
-- it is currently violated: JSON decoding, and the take/split family whose
-- signatures do not constrain the sizes they claim.
module Test.Invariant
  ( invariantTests
  ) where

import           Data.Grid.Sized
import           Data.Grid.Sized.Unsafe (unsafeGridFromVector)

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

-- | Compare a grid's actual vector length against what its type promises.
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

-- | A grid decoded from JSON must either be well sized or not exist.
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

-- | A grid with three distinct axis sizes, so no two axes could be confused
-- with each other by a transposition bug -- and three axes, so 'scanAxis' has
-- a middle one to reach that 'mapLowerDim' composed with 'scanl1Grid' cannot.
twoByThreeByTwo :: Grid '[ Ordinal 2, Ordinal 3, Ordinal 2] Int
twoByThreeByTwo =
  fromJust $
  gridFromList
    [ [[0, 1], [10, 11], [20, 21]]
    , [[100, 101], [110, 111], [120, 121]]
    ]

-- | 'assertWellSized' over a list of grids, for the operations that return one
-- grid per sub-grid rather than a single result.
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
      -- Each of the following fails against the tree as of 2026-08-09.
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
      -- The two cases that used to fail at runtime here are now rejected by the
      -- compiler, so they cannot be expressed as runtime tests any more:
      --
      -- >  takeGrid 9 oneByThree :: Grid '[Ordinal 9] Int
      -- >    error: Cannot satisfy: 9 <= 3
      --
      -- >  let (_ :: Grid '[Ordinal 1, Ordinal 3] Int, b) = splitHigherDim threeByThree
      -- >   in b :: Grid '[Ordinal 7, Ordinal 3] Int
      -- >    error: Cannot match 'Ordinal 7' with 'Ordinal (3 - 1)'
      --
      -- Those cases, plus the ShrinkableGrid window bound, are now pinned down
      -- by the compile-fail harness in Test.CompileFail (sized-grid-cti). What
      -- is left below is the positive half: the sizes the signatures now force
      -- are the sizes the vectors actually have.
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
      -- The operations above either keep the shape or are the ones whose
      -- signatures were tightened. The rest of the shape-changing API was never
      -- checked at all, and it is the half where a size mistake is most easily
      -- made: every one of these computes a length from @MaxCoordSize@ of a
      -- type-level list it takes apart, so an off-by-one in the arithmetic
      -- produces a grid whose vector disagrees with its own type, silently,
      -- exactly as the constructor used to allow.
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
          -- The one case where the result's size is neither of its arguments':
          -- 3 + 2 rows of 3, so 15 cells, and nothing but this checks it.
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
      -- The escape hatch is deliberately in its own module. It is the only way
      -- left to build a `Grid` from a raw vector without a length check, so the
      -- one thing worth pinning down is that it is genuinely an identity on the
      -- representation -- code reaching for it is doing so to keep a length it
      -- already knows is right.
    , testCase "unsafeGridFromVector round-trips through gridVector" $
      assertEqual
        "unsafeGridFromVector . gridVector == id"
        threeByThree
        (unsafeGridFromVector (gridVector threeByThree))
      -- The reach-through that motivated all of this: ../aoc/src/2018/11.hs
      -- built prefix sums as @Grid . V.scanl1' (+) . unGrid@ because the
      -- library had no length-preserving scan. It now does, so the escape hatch
      -- is not needed for it.
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
      -- sized-grid-e6h: 'mapAxis' and 'scanAxis' name any axis by position,
      -- so the summed-area-table build-up no longer has to reach the second
      -- axis of a 2D grid by physically rotating it with 'transposeGrid'.
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
          -- The case 'transposeGrid' cannot: there is no 2-axis swap that
          -- brings a middle axis of a 3-axis grid to the edge alone.
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
    ]
