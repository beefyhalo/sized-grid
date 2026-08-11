{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- | Tests for the checked offset and the neighbourhoods built on it
-- (@sized-grid-7gs@).
module Test.Neighbours
  ( neighbourTests
  ) where

import           SizedGrid
import           Test.Arbitrary   ()

import           Data.Maybe       (fromJust)
import           Test.Tasty
import           Test.Tasty.HUnit

-- | The bounded space: off-grid is 'Nothing'.
hw :: Int -> HardWrap 5
hw = HardWrap . fromJust . numToOrdinal

-- | The torus: every offset succeeds.
pe :: Int -> Periodic 5
pe = Periodic . fromJust . numToOrdinal

offsetIsCoordTests :: TestTree
offsetIsCoordTests =
    testGroup
        "offsetIsCoord is the checked counterpart of (.+^)"
        [ testCase "a zero offset is the identity" $ do
              assertEqual "HardWrap" (Just (hw 2)) (offsetIsCoord (hw 2) 0)
              assertEqual "Periodic" (Just (pe 2)) (offsetIsCoord (pe 2) 0)
        , testCase "an in-range offset moves" $ do
              assertEqual "HardWrap" (Just (hw 3)) (offsetIsCoord (hw 2) 1)
              assertEqual "Periodic" (Just (pe 3)) (offsetIsCoord (pe 2) 1)
        , testCase "HardWrap fails off the low edge instead of clamping" $
              assertEqual "" Nothing (offsetIsCoord (hw 0) (-1))
        , testCase "HardWrap fails off the high edge instead of clamping" $
              assertEqual "" Nothing (offsetIsCoord (hw 4) 1)
        , testCase "Periodic wraps at the low edge" $
              assertEqual "" (Just (pe 4)) (offsetIsCoord (pe 0) (-1))
        , testCase "Periodic wraps at the high edge" $
              assertEqual "" (Just (pe 0)) (offsetIsCoord (pe 4) 1)
        , testCase "a displacement wider than Int cannot wrap into range" $
              assertEqual
                  ""
                  Nothing
                  (offsetIsCoord (hw 0) (toInteger (maxBound :: Int) + 2))
        , testCase "Periodic reduces a huge displacement" $
              assertEqual
                  ""
                  (Just (pe 1))
                  (offsetIsCoord (pe 1) (5 ^ (20 :: Int)))
        ]

hwc :: Int -> Int -> Coord '[HardWrap 5, HardWrap 5]
hwc r c = hw r :| hw c :| EmptyCoord

pec :: Int -> Int -> Coord '[Periodic 5, Periodic 5]
pec r c = pe r :| pe c :| EmptyCoord

-- | One bounded axis and one torus axis, to pin down that the policy is read
-- per axis rather than once for the whole coordinate.
mixc :: Int -> Int -> Coord '[HardWrap 5, Periodic 5]
mixc r c = hw r :| pe c :| EmptyCoord

offsetCoordTests :: TestTree
offsetCoordTests =
    testGroup
        "offsetCoord applies each axis's own boundary policy"
        [ testCase "a zero offset is the identity" $ do
              assertEqual "HardWrap" (Just (hwc 2 2)) (offsetCoord (hwc 2 2) (0, 0))
              assertEqual "Periodic" (Just (pec 2 2)) (offsetCoord (pec 2 2) (0, 0))
        , testCase "an in-range offset moves on every axis" $
              assertEqual "" (Just (hwc 3 3)) (offsetCoord (hwc 2 2) (1, 1))
        , testCase "failing on the first axis fails the whole offset" $
              assertEqual "" Nothing (offsetCoord (hwc 0 0) (-1, 0))
        , testCase "failing on the second axis fails the whole offset" $
              assertEqual "" Nothing (offsetCoord (hwc 0 0) (0, -1))
        , testCase "off the high corner fails" $
              assertEqual "" Nothing (offsetCoord (hwc 4 4) (1, 1))
        , testCase "a torus wraps on every axis" $
              assertEqual "" (Just (pec 4 4)) (offsetCoord (pec 0 0) (-1, -1))
        , testCase "a torus axis wraps while a bounded axis stands still" $
              assertEqual "" (Just (mixc 0 4)) (offsetCoord (mixc 0 0) (0, -1))
        , testCase "a bounded axis fails while a torus axis would have wrapped" $
              assertEqual "" Nothing (offsetCoord (mixc 0 0) (-1, -1))
        ]

neighbourTests :: TestTree
neighbourTests =
    testGroup "Neighbours" [offsetIsCoordTests, offsetCoordTests]
