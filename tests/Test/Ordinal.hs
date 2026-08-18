{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Ordinal
  ( ordinalTests
  ) where

import           Data.Grid.Sized

import           Control.Lens     (re, view)
import           Data.AffineSpace ((.+^), (.-.))
import           Data.Proxy
import           GHC.TypeLits     (natVal)
import           Test.Tasty
import           Test.Tasty.HUnit

ordinalTests :: TestTree
ordinalTests =
  testGroup
    "Ordinal"
    [ testGroup "numToOrdinal is the range check" numToOrdinalTests
    , testGroup "Enum covers the range and stops at the end" enumTests
    , testGroup "reifyCoord recovers the value" sizeProxyTests
    , testGroup "Coord arithmetic stays in range" arithmeticTests
    ]

numToOrdinalTests :: [TestTree]
numToOrdinalTests =
  [ testCase "accepts every value in [0, n)" $
    assertEqual
      "round trip"
      (Just [0 .. 4])
      (traverse (fmap ordinalToInt . numToOrdinal @Int @5) [0 .. 4])
  , testCase "rejects n" $
    assertEqual "" Nothing (ordinalToInt <$> numToOrdinal @Int @5 5)
  , testCase "rejects negatives" $
    assertEqual "" Nothing (ordinalToInt <$> numToOrdinal @Int @5 (-1))
  , testCase "rejects a value wider than Int" $
    assertEqual
      ""
      Nothing
      (ordinalToInt <$> numToOrdinal @Integer @5 (2 ^ (70 :: Int) + 3))
  , testCase "ordinalSize is the type-level size" $
    assertEqual "" 5 (ordinalSize @5)
  , testCase "maxCoord of the smallest possible coord" $
    assertEqual "" 0 (ordinalToInt (maxCoord :: Ordinal 1))
  ]

enumTests :: [TestTree]
enumTests =
  [ testCase "[minBound .. maxBound] is the whole range" $
    assertEqual
      ""
      [0 .. 4]
      (map ordinalToInt [minBound .. maxBound :: Ordinal 5])
  , testCase "[minBound ..] stops at maxBound" $
    assertEqual "" [0 .. 4] (map ordinalToInt ([minBound ..] :: [Ordinal 5]))
  , testCase "[maxBound, pred maxBound ..] counts down to minBound" $
    assertEqual
      ""
      [4,3 .. 0]
      (map ordinalToInt ([maxBound,toEnum 3 ..] :: [Ordinal 5]))
  , testCase "Periodic toEnum round trips over its own range" $
    assertEqual
      ""
      [0 .. 4]
      (map (fromEnum . (toEnum :: Int -> Periodic 5)) [0 .. 4])
  , testCase "Periodic toEnum wraps with period n" $
    assertEqual
      ""
      [0, 1, 2]
      (map (fromEnum . (toEnum :: Int -> Periodic 3)) [3, 4, 5])
  , testCase "Clamped toEnum round trips over its own range" $
    assertEqual
      ""
      [0 .. 4]
      (map (fromEnum . (toEnum :: Int -> Clamped 5)) [0 .. 4])
  ]

sizeProxyTests :: [TestTree]
sizeProxyTests =
  [ testCase "reifyCoord at every value of an Ordinal 5" $
    assertEqual
      ""
      [0 .. 4]
      [ reifyCoord o (\m -> natVal (Proxy @m))
      | i <- [0 .. 4]
      , Just (o :: Ordinal 5) <- [numToOrdinal (i :: Int)]
      ]
  , testCase "reifyCoord through a Periodic" $
    assertEqual
      ""
      3
      (reifyCoord
           (view (re asOrdinal) (toEnum 3) :: Periodic 5)
           (\m -> natVal (Proxy @m)))
  ]

arithmeticTests :: [TestTree]
arithmeticTests =
  [ testCase "Periodic .+^ reduces a huge positive offset" $
    assertEqual
      ""
      3
      (fromEnum (zeroPosition @Periodic @5 .+^ (10 ^ (18 :: Int) * 5 + 3)))
  , testCase "Periodic .+^ reduces a huge negative offset" $
    assertEqual
      ""
      2
      (fromEnum (zeroPosition @Periodic @5 .+^ negate (10 ^ (18 :: Int) * 5 + 3)))
  , testCase "Periodic .-. is the wrapped difference" $
    assertEqual
      ""
      3
      ((toEnum 1 :: Periodic 5) .-. toEnum 3)
  , testCase "Clamped .+^ clamps a huge positive offset to maxBound" $
    assertEqual
      ""
      4
      (fromEnum (zeroPosition @Clamped @5 .+^ (10 ^ (18 :: Int) * 5 + 3)))
  , testCase "Clamped .+^ clamps a huge negative offset to minBound" $
    assertEqual
      ""
      0
      (fromEnum ((maxBound :: Clamped 5) .+^ negate (10 ^ (18 :: Int) * 5 + 3)))
    -- Both start at the top of the axis deliberately: from position 0 these
    -- would pass even over an implementation that adds before reducing,
    -- since nothing wraps there.
  , testCase "Clamped .+^ maxBound from the top edge saturates up, not through zero" $
    assertEqual "" 4 (fromEnum ((maxBound :: Clamped 5) .+^ maxBound))
  , testCase "Clamped .+^ minBound saturates down" $
    assertEqual "" 0 (fromEnum ((maxBound :: Clamped 5) .+^ minBound))
  , testCase "Periodic .+^ maxBound from the top is the true residue" $
    assertEqual
      ""
      (fromIntegral ((4 + toInteger (maxBound :: Int)) `mod` 5))
      (fromEnum ((toEnum 4 :: Periodic 5) .+^ maxBound))
  , testCase "Periodic .+^ minBound from the top is the true residue" $
    assertEqual
      ""
      (fromIntegral ((4 + toInteger (minBound :: Int)) `mod` 5))
      (fromEnum ((toEnum 4 :: Periodic 5) .+^ minBound))
  , testCase "Clamped .-. is a signed displacement, unclamped" $
    assertEqual "" (-3) ((toEnum 1 :: Clamped 5) .-. toEnum 4)
  ]
