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
import           Test.Arbitrary        ()

import           Data.List             (nub)
import           Data.Maybe            (fromJust)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty, (===))

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

mooreTests :: TestTree
mooreTests =
    testGroup
        "mooreNeighbours"
        [ testCase "the centre is not its own neighbour" $ do
              assertBool "HardWrap" (hwc 2 2 `notElem` neighbours (hwc 2 2))
              assertBool "Periodic" (pec 2 2 `notElem` neighbours (pec 2 2))
        , testCase "a bounded grid has 3 neighbours at a corner" $
              assertEqual "" 3 (length (neighbours (hwc 0 0)))
        , testCase "a bounded grid has 5 neighbours on an edge" $
              assertEqual "" 5 (length (neighbours (hwc 0 2)))
        , testCase "a bounded grid has 8 neighbours in the interior" $
              assertEqual "" 8 (length (neighbours (hwc 2 2)))
        , testCase "a torus has 8 neighbours everywhere, corners included" $ do
              assertEqual "corner" 8 (length (neighbours (pec 0 0)))
              assertEqual "interior" 8 (length (neighbours (pec 2 2)))
        , -- The measurement recorded on sized-grid-7gs: moorePoints 1 at a
          -- corner of a HardWrap 5 x HardWrap 5 returned nine results of which
          -- only four were distinct, because (.+^) clamped every off-grid
          -- offset back onto an edge cell. Callers had to nubOrd it away.
          testCase "regression: a corner yields no clamped duplicates" $ do
              let ns = neighbours (hwc 0 0)
              assertEqual "count" 3 (length ns)
              assertEqual "all distinct" 3 (length (nub ns))
              assertEqual
                  "the three cells that exist"
                  [hwc 0 1, hwc 1 0, hwc 1 1]
                  ns
        , testCase "results are in row-major order, last axis fastest" $
              assertEqual
                  ""
                  [ hwc 1 1, hwc 1 2, hwc 1 3
                  , hwc 2 1,           hwc 2 3
                  , hwc 3 1, hwc 3 2, hwc 3 3
                  ]
                  (neighbours (hwc 2 2))
        , testCase "a torus axis is ordered by offset, not by value" $
              assertEqual
                  ""
                  [ pec 4 4, pec 4 0, pec 4 1
                  , pec 0 4,          pec 0 1
                  , pec 1 4, pec 1 0, pec 1 1
                  ]
                  (neighbours (pec 0 0))
        , testCase "radius 2 reaches further" $
              assertEqual "" 24 (length (mooreNeighbours 2 (hwc 2 2)))
        , testCase "radius 0 is empty" $
              assertEqual "" [] (mooreNeighbours 0 (hwc 2 2))
        , testProperty "never contains duplicates" $ \(c :: Coord '[HardWrap 5, HardWrap 5]) ->
              length (nub (neighbours c)) === length (neighbours c)
        , testProperty "a torus neighbourhood has no duplicates either" $ \(c :: Coord '[Periodic 5, Periodic 5]) ->
              length (nub (neighbours c)) === length (neighbours c)
        , testProperty "never contains the centre" $ \(c :: Coord '[HardWrap 5, HardWrap 5]) ->
              c `notElem` neighbours c
        , testProperty "is symmetric on a bounded grid" $ \(c :: Coord '[HardWrap 5, HardWrap 5]) ->
              all (\c' -> c `elem` neighbours c') (neighbours c)
        , testProperty "is symmetric on a torus" $ \(c :: Coord '[Periodic 5, Periodic 5]) ->
              all (\c' -> c `elem` neighbours c') (neighbours c)
        ]

neighbourTests :: TestTree
neighbourTests =
    testGroup
        "Neighbours"
        [offsetIsCoordTests, offsetCoordTests, mooreTests]
