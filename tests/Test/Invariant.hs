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

import           SizedGrid

import           Data.Aeson           (decode, encode)
import           Data.ByteString.Lazy (ByteString)
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
    (V.length (unGrid g))

-- | A grid decoded from JSON must either be well sized or not exist.
assertRejects ::
     forall (cs :: [Type]). (KnownNat (MaxCoordSize cs), All IsCoordLifted cs)
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
    , testGroup
        "take/split must not claim more than they hold"
        [ testCase "takeGrid beyond the source length" $
          assertWellSized "takeGrid @9 of a 3-grid" (takeGrid (Proxy @9) oneByThree :: Grid '[ Ordinal 9] Int)
        , testCase "takeGrid within the source length" $
          assertWellSized "takeGrid @2 of a 3-grid" (takeGrid (Proxy @2) oneByThree :: Grid '[ Ordinal 2] Int)
        , testCase "splitHigherDim, honest annotation" $
          let (_ :: Grid '[ Ordinal 1, Ordinal 3] Int, b) = splitHigherDim threeByThree
           in assertWellSized "splitHigherDim snd" (b :: Grid '[ Ordinal 2, Ordinal 3] Int)
          -- The second component's size variable is free in splitHigherDim's
          -- signature, so this dishonest annotation still typechecks. It should
          -- not: 3 - 1 is 2, not 7.
        , testCase "splitHigherDim, dishonest annotation" $
          let (_ :: Grid '[ Ordinal 1, Ordinal 3] Int, b) = splitHigherDim threeByThree
           in assertWellSized "splitHigherDim snd @7" (b :: Grid '[ Ordinal 7, Ordinal 3] Int)
        ]
    ]
