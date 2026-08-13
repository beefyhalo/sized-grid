{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- |
-- Tests for the 'Ordinal' representation itself: the range check that replaced
-- the type-level construction, the 'Enum' instance that replaced the
-- @fromJust . numToOrdinal@ one, and the evidence recovery that replaced
-- unpacking the GADT.
module Test.Ordinal
  ( ordinalTests
  ) where

import           SizedGrid

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
  , -- The check converts to 'Integer' before comparing. Narrowing first would
    -- wrap this into the accepted range on a 64-bit 'Int'.
    testCase "rejects a value wider than Int" $
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
  , -- The derived 'enumFrom' counts up from @fromEnum x@ with no upper bound
    -- and calls 'toEnum' on each, so it used to walk off the end and throw
    -- rather than stop at 'maxBound'.
    testCase "[minBound ..] stops at maxBound" $
    assertEqual "" [0 .. 4] (map ordinalToInt ([minBound ..] :: [Ordinal 5]))
  , testCase "[maxBound, pred maxBound ..] counts down to minBound" $
    assertEqual
      ""
      [4,3 .. 0]
      (map ordinalToInt ([maxBound,toEnum 3 ..] :: [Ordinal 5]))
  , -- 'Periodic' wrapped modulo @maxCoordSize@, which is @n - 1@, so
    -- @toEnum 2 :: Periodic 3@ came back as 0.
    testCase "Periodic toEnum round trips over its own range" $
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
  -- The GADT handed this back by unpacking a value. It is now recomputed with
  -- 'someNatVal' and a runtime comparison, so it needs testing away from zero,
  -- where an off-by-one would still look right.
  [ testCase "reifyCoord at every value of an Ordinal 5" $
    assertEqual
      ""
      [0 .. 4]
      -- A generator rather than a lazy @let Just o = ...@: matching in a list
      -- comprehension is total (a 'Nothing' drops the element), and the
      -- expected @[0 .. 4]@ still fails the assertion if one ever does.
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
  [ -- The displacement is an 'Int' as of sized-grid-0tj, where it had been an
    -- unbounded 'Integer'. These pin down the reduction and the clamp at offsets
    -- far larger than the coord, which is what they were always for.
    testCase "Periodic .+^ reduces a huge positive offset" $
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
    -- The offsets above used to be 2^70, which an 'Int' cannot hold. The
    -- extremes of the type replace them, and they are the sharper test: these
    -- are the inputs on which forming @position + offset@ before reducing it
    -- overflows.
    --
    -- Both start at the /top/ of the axis, and that is the whole point. From
    -- position 0 the sum is exactly 'maxBound', nothing wraps, and the naive
    -- body gets the right answer by luck --- these tests would pass over the
    -- implementation they exist to reject. From position 4 the sum wraps to a
    -- large negative number, and then @max 0 . min 4@ folds a huge /positive/
    -- offset onto the /low/ edge (0 instead of 4), while @`mod` 5@ of the
    -- wrapped sum lands on the wrong residue (0 instead of 1). Verified against
    -- both bodies before these were written down.
  , testCase "Clamped .+^ maxBound from the top edge saturates up, not through zero" $
    assertEqual "" 4 (fromEnum ((maxBound :: Clamped 5) .+^ maxBound))
  , testCase "Clamped .+^ minBound saturates down" $
    assertEqual "" 0 (fromEnum ((maxBound :: Clamped 5) .+^ minBound))
    -- Expected values computed in 'Integer', so the oracle is arithmetic rather
    -- than a restatement of the instance.
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
