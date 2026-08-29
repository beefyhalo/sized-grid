{-# LANGUAGE DataKinds #-}

-- | Tests for "Data.Grid.Atlas".
module Test.Tiles
  ( tilesTests
  ) where

import           Data.Grid.Atlas
import           Data.Grid.Sized

import           Data.Functor.Rep      (index, tabulate)
import           GHC.TypeLits          (KnownNat)
import           Test.QuickCheck       (Gen, chooseInt, forAll, property,
                                        (===))
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty)

-- | A 12x4 board whose cells are their own linear position, so reading the
-- wrong cell through the atlas shows up immediately.
board :: Grid '[ Ordinal 12, Ordinal 4] Int
board = tabulate coordPosition

-- | 4 charts of 3x4, tiling 'board' along its outermost axis.
atlas :: Atlas '[ Ordinal 3, Ordinal 4] 4 Int
atlas = atlasFromTiles @3 board

-- | The same split 'atlasFromTiles' makes, computed independently.
splitChart :: Coord '[ Ordinal 12, Ordinal 4] -> AtlasCoord '[ Ordinal 3, Ordinal 4] 4
splitChart (a0 :| rest) =
    let (q, r) = ordinalToInt a0 `divMod` 3
    in (unsafeOrdinal q, unsafeOrdinal r :| rest)

genOrdinal :: forall n. KnownNat n => Gen (Ordinal n)
genOrdinal = unsafeOrdinal <$> chooseInt (0, ordinalSize @n - 1)

tilesTests :: TestTree
tilesTests =
    testGroup
        "Data.Grid.Atlas"
        [ testCase "atlasIndex agrees with index on the untiled grid, at every coordinate" $
          mapM_
              (\c -> assertEqual (show c) (index board c) (atlasIndex atlas (splitChart c)))
              (allCoord @'[ Ordinal 12, Ordinal 4])
        , testProperty
              "atlasOffsetHead crossing a seam lands where offsetting the untiled head axis would" $
          forAll genOrdinal $ \(a0 :: Ordinal 12) ->
          forAll genOrdinal $ \(a1 :: Ordinal 4) ->
          forAll (chooseInt (-15, 15)) $ \d ->
              property $
              let p = ordinalToInt a0 + d
                  got = atlasOffsetHead atlas (splitChart (a0 :| singleCoord a1)) d
              in if p < 0 || p >= 12
                     then got === Nothing
                     else case got of
                              Nothing -> property False
                              Just (chart, local :| _) ->
                                  (ordinalToInt chart * 3 + ordinalToInt local) === p
        ]
