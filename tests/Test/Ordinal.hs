{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Ordinal
  ( ordinalTests,
  )
where

import Control.Lens (re, view)
import Data.AffineSpace ((.+^), (.-.))
import Data.Grid.Sized
import Data.Proxy
import GHC.TypeLits (natVal)
import Test.Tasty
import Test.Tasty.HUnit

ordinalTests :: TestTree
ordinalTests =
  testGroup
    "Ordinal"
    [ testGroup "numToOrdinal is the range check" numToOrdinalTests,
      testGroup "Enum covers the range and stops at the end" enumTests,
      testGroup "reifyCoord recovers the value" sizeProxyTests,
      testGroup "reifySize turns a run-time size into an axis type" reifySizeTests,
      testGroup "Coord arithmetic stays in range" arithmeticTests
    ]

numToOrdinalTests :: [TestTree]
numToOrdinalTests =
  [ testCase "accepts every value in [0, n)" $
      assertEqual
        "round trip"
        (Just [0 .. 4])
        (traverse (fmap ordinalToInt . numToOrdinal @Int @5) [0 .. 4]),
    testCase "rejects n" $
      assertEqual "" Nothing (ordinalToInt <$> numToOrdinal @Int @5 5),
    testCase "rejects negatives" $
      assertEqual "" Nothing (ordinalToInt <$> numToOrdinal @Int @5 (-1)),
    testCase "rejects a value wider than Int" $
      assertEqual
        ""
        Nothing
        (ordinalToInt <$> numToOrdinal @Integer @5 (2 ^ (70 :: Int) + 3)),
    testCase "ordinalSize is the type-level size" $
      assertEqual "" 5 (ordinalSize @5),
    testCase "maxCoord of the smallest possible coord" $
      assertEqual "" 0 (ordinalToInt (maxCoord :: Ordinal 1))
  ]

enumTests :: [TestTree]
enumTests =
  [ testCase "[minBound .. maxBound] is the whole range" $
      assertEqual
        ""
        [0 .. 4]
        (map ordinalToInt [minBound .. maxBound :: Ordinal 5]),
    testCase "[minBound ..] stops at maxBound" $
      assertEqual "" [0 .. 4] (map ordinalToInt ([minBound ..] :: [Ordinal 5])),
    testCase "[maxBound, pred maxBound ..] counts down to minBound" $
      assertEqual
        ""
        [4, 3 .. 0]
        (map ordinalToInt ([maxBound, toEnum 3 ..] :: [Ordinal 5])),
    testCase "Periodic toEnum round trips over its own range" $
      assertEqual
        ""
        [0 .. 4]
        (map (fromEnum . (toEnum :: Int -> Periodic 5)) [0 .. 4]),
    testCase "Periodic toEnum wraps with period n" $
      assertEqual
        ""
        [0, 1, 2]
        (map (fromEnum . (toEnum :: Int -> Periodic 3)) [3, 4, 5]),
    testCase "Clamped toEnum round trips over its own range" $
      assertEqual
        ""
        [0 .. 4]
        (map (fromEnum . (toEnum :: Int -> Clamped 5)) [0 .. 4])
  ]

-- reifyCoord's continuation is @forall m -> (KnownNat m, m + 1 <= n) => x@, so
-- @x@ is bound outside an implication that carries givens. Under GHC 9.10 that
-- makes an as-yet-unsolved @x@ untouchable from within the continuation
-- ([GHC-83865]), and the numeric literals here are then ambiguous for want of
-- it. Annotating each result fixes @x@ before the continuation is checked.
-- GHC 9.12 solves this unaided; the annotations are what keep 9.10 building.
sizeProxyTests :: [TestTree]
sizeProxyTests =
  [ testCase "reifyCoord at every value of an Ordinal 5" $
      assertEqual
        ""
        [0 .. 4]
        [ reifyCoord o (\m -> natVal (Proxy @m)) :: Integer
        | i <- [0 .. 4],
          Just (o :: Ordinal 5) <- [numToOrdinal (i :: Int)]
        ],
    testCase "reifyCoord through a Periodic" $
      assertEqual
        ""
        3
        ( reifyCoord
            (view (re asOrdinal) (toEnum 3) :: Periodic 5)
            (\m -> natVal (Proxy @m)) ::
            Integer
        )
  ]

-- The same annotation the note above 'sizeProxyTests' explains: @x@ in
-- @reifySize@'s continuation is untouchable under GHC 9.10, so each result is
-- pinned with a signature before the continuation is checked.
reifySizeTests :: [TestTree]
reifySizeTests =
  [ testCase "recovers a positive size as a type-level Nat" $
      assertEqual
        ""
        (Just 7)
        (reifySize 7 (\n -> natVal (Proxy @n)) :: Maybe Integer),
    -- @maxBound :: Ordinal n@ needs @(KnownNat n, 1 <= n)@; this compiles only
    -- because reifySize hands the continuation both, which is the whole point.
    testCase "the reified size carries 1 <= n" $
      assertEqual
        ""
        (Just 6)
        (reifySize 7 (\n -> ordinalToInt (maxBound :: Ordinal n)) :: Maybe Int),
    testCase "accepts a size of 1, the smallest axis" $
      assertEqual
        ""
        (Just 1)
        (reifySize 1 (\n -> natVal (Proxy @n)) :: Maybe Integer),
    testCase "rejects zero" $
      assertEqual
        ""
        Nothing
        (reifySize 0 (\n -> natVal (Proxy @n)) :: Maybe Integer),
    testCase "rejects a negative size" $
      assertEqual
        ""
        Nothing
        (reifySize (-3) (\n -> natVal (Proxy @n)) :: Maybe Integer)
  ]

arithmeticTests :: [TestTree]
arithmeticTests =
  [ testCase "Periodic .+^ reduces a huge positive offset" $
      assertEqual
        ""
        3
        (fromEnum (zeroPosition @Periodic @5 .+^ (10 ^ (18 :: Int) * 5 + 3))),
    testCase "Periodic .+^ reduces a huge negative offset" $
      assertEqual
        ""
        2
        (fromEnum (zeroPosition @Periodic @5 .+^ negate (10 ^ (18 :: Int) * 5 + 3))),
    testCase "Periodic .-. is the shortest signed difference" $
      assertEqual
        ""
        (-2)
        ((toEnum 1 :: Periodic 5) .-. toEnum 3),
    testCase "Clamped .+^ clamps a huge positive offset to maxBound" $
      assertEqual
        ""
        4
        (fromEnum (zeroPosition @Clamped @5 .+^ (10 ^ (18 :: Int) * 5 + 3))),
    testCase "Clamped .+^ clamps a huge negative offset to minBound" $
      assertEqual
        ""
        0
        (fromEnum ((maxBound :: Clamped 5) .+^ negate (10 ^ (18 :: Int) * 5 + 3))),
    -- Both start at the top of the axis deliberately: from position 0 these
    -- would pass even over an implementation that adds before reducing,
    -- since nothing wraps there.
    testCase "Clamped .+^ maxBound from the top edge saturates up, not through zero" $
      assertEqual "" 4 (fromEnum ((maxBound :: Clamped 5) .+^ maxBound)),
    testCase "Clamped .+^ minBound saturates down" $
      assertEqual "" 0 (fromEnum ((maxBound :: Clamped 5) .+^ minBound)),
    testCase "Periodic .+^ maxBound from the top is the true residue" $
      assertEqual
        ""
        (fromIntegral ((4 + toInteger (maxBound :: Int)) `mod` 5))
        (fromEnum ((toEnum 4 :: Periodic 5) .+^ maxBound)),
    testCase "Periodic .+^ minBound from the top is the true residue" $
      assertEqual
        ""
        (fromIntegral ((4 + toInteger (minBound :: Int)) `mod` 5))
        (fromEnum ((toEnum 4 :: Periodic 5) .+^ minBound)),
    testCase "Clamped .-. is a signed displacement, unclamped" $
      assertEqual "" (-3) ((toEnum 1 :: Clamped 5) .-. toEnum 4)
  ]
