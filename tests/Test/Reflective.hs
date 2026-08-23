{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for the two bounce boundary policies, 'Reflective' and 'Reflect101'.
--
-- Both are total on ('.+^') by construction, so the obligation checked here is
-- not "does it stay in range" but "does the closed form compute the right
-- bounce" -- checked against slow, obviously-correct recursive references.
module Test.Reflective
  ( reflectiveTests
  ) where

import           Data.Grid.Sized
import           Test.Arbitrary        ()

import           Control.Lens          (view)
import           Data.AffineSpace      ((.+^))
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty, (===), (==>))

-- | Billiard bounce off two walls @size@ apart, written the obvious recursive way.
bounceRef :: Int -> Int -> Int
bounceRef size = go
  where
    go i
      | i < 0 = go (negate i - 1)
      | i >= size = go (2 * size - 1 - i)
      | otherwise = i

-- | Like 'bounceRef', but mirrors around the edge /cell/ (fixed points @0@,
-- @m@) rather than the wall beyond it. Degenerate at @m == 0@: every
-- displacement is absorbed by the single cell.
reflect101Ref :: Int -> Int -> Int
reflect101Ref m
  | m == 0 = const 0
  | otherwise = go
  where
    go i
      | i < 0 = go (negate i)
      | i > m = go (2 * m - i)
      | otherwise = i

-- | Whether 'bounceRef' bounces an odd number of times reaching @i@.
--
-- Well-defined for every integer: 'Reflective''s walls sit at half-integers,
-- never landed on by an integer position, so this agrees with the closed
-- form's parity at every input. Contrast 'reflect101FlipRef', whose walls
-- sit on lattice points and so genuinely part ways there.
bounceFlipRef :: Int -> Int -> Bool
bounceFlipRef size = go False
  where
    go flipped i
      | i < 0 = go (not flipped) (negate i - 1)
      | i >= size = go (not flipped) (2 * size - 1 - i)
      | otherwise = flipped

-- | Tracks parity through 'reflect101Ref''s recursion. Unlike 'bounceFlipRef',
-- not checked at every input: at @i@ an exact multiple of @m@ the wall sits on
-- the lattice, so the fixed point admits more than one reflection convention.
-- The property test below excludes that case; 'frameFlipExamples' pins the
-- convention there explicitly instead.
reflect101FlipRef :: Int -> Int -> Bool
reflect101FlipRef m
  | m == 0 = const False
  | otherwise = go False
  where
    go flipped i
      | i < 0 = go (not flipped) (negate i)
      | i > m = go (not flipped) (2 * m - i)
      | otherwise = flipped

rf :: Int -> Reflective 5
rf = Reflective . unsafeOrdinal

r1 :: Int -> Reflect101 5
r1 = Reflect101 . unsafeOrdinal

bounceExamples :: TestTree
bounceExamples =
  testGroup
    "Reflective bounces off the wall, not around the edge cell"
    [ testCase "-1 becomes 0" $ assertEqual "" (rf 0) (rf 0 .+^ (-1))
    , testCase "-2 becomes 1" $ assertEqual "" (rf 1) (rf 0 .+^ (-2))
    , -- Mirror image of the -1/-2 cases above, at the other wall.
      testCase "size becomes size - 1" $
        assertEqual "" (rf 4) (rf 0 .+^ 5)
    , testCase "size + 1 becomes size - 2" $
        assertEqual "" (rf 3) (rf 0 .+^ 6)
    , -- Distinguishes this from 'Reflect101': the edge cell is visited twice in a row.
      testCase "stepping onto the top and one past it both land on the top" $ do
        assertEqual "onto" (rf 4) (rf 3 .+^ 1)
        assertEqual "past" (rf 4) (rf 3 .+^ 2)
    ]

reflect101Examples :: TestTree
reflect101Examples =
  testGroup
    "Reflect101 mirrors around the edge cell, never repeating it"
    [ testCase "-1 becomes 1, not 0" $ assertEqual "" (r1 1) (r1 0 .+^ (-1))
    , testCase "-2 becomes 2" $ assertEqual "" (r1 2) (r1 0 .+^ (-2))
    , -- Mirrored around the top cell (4), so it lands one below the top, not on it.
      testCase "size becomes size - 2" $
        assertEqual "" (r1 3) (r1 0 .+^ 5)
    , testCase "the top cell is its own mirror image" $
        assertEqual "" (r1 4) (r1 4 .+^ 0)
    , -- No neighbour to mirror around, so every displacement is absorbed.
      testCase "a one-cell axis absorbs every displacement" $
        assertEqual "" (Reflect101 (unsafeOrdinal 0) :: Reflect101 1)
                    (Reflect101 (unsafeOrdinal 0) .+^ 37)
    ]

closedFormAgreesWithReferenceTests :: TestTree
closedFormAgreesWithReferenceTests =
  testGroup
    "the closed form agrees with the issue's own recursive definition"
    [ testProperty "Reflective, size 5" $ \(c :: Reflective 5) (d :: Int) ->
        let i = ordinalToInt (view asOrdinal c)
         in ordinalToInt (view asOrdinal (c .+^ d)) === bounceRef 5 (i + d)
    , testProperty "Reflective, size 1" $ \(c :: Reflective 1) (d :: Int) ->
        let i = ordinalToInt (view asOrdinal c)
         in ordinalToInt (view asOrdinal (c .+^ d)) === bounceRef 1 (i + d)
    , testProperty "Reflect101, size 5" $ \(c :: Reflect101 5) (d :: Int) ->
        let i = ordinalToInt (view asOrdinal c)
         in ordinalToInt (view asOrdinal (c .+^ d)) === reflect101Ref 4 (i + d)
    , testProperty "Reflect101, size 2" $ \(c :: Reflect101 2) (d :: Int) ->
        let i = ordinalToInt (view asOrdinal c)
         in ordinalToInt (view asOrdinal (c .+^ d)) === reflect101Ref 1 (i + d)
    , testProperty "Reflect101, size 1 (the degenerate axis)" $ \(c :: Reflect101 1) (d :: Int) ->
        let i = ordinalToInt (view asOrdinal c)
         in ordinalToInt (view asOrdinal (c .+^ d)) === reflect101Ref 0 (i + d)
    ]

-- | The bounce lives in ('.+^') alone; 'offsetIsCoord' still reports 'Nothing'
-- on stepping off the axis, unlike 'Data.Grid.Sized.Coord.Periodic.Periodic'.
offsetIsCoordStaysCheckedTests :: TestTree
offsetIsCoordStaysCheckedTests =
  testGroup
    "offsetIsCoord reports leaving the axis; the bounce is (.+^) alone"
    [ testCase "Reflective: one step past either wall is Nothing" $ do
        assertEqual "" Nothing (offsetIsCoord (rf 0) (-1))
        assertEqual "" Nothing (offsetIsCoord (rf 4) 1)
    , testCase "Reflective: (.+^) at the same displacement is total" $ do
        assertEqual "" (rf 0) (rf 0 .+^ (-1))
        assertEqual "" (rf 4) (rf 4 .+^ 1)
    , testCase "Reflect101: one step past either wall is Nothing" $ do
        assertEqual "" Nothing (offsetIsCoord (r1 0) (-1))
        assertEqual "" Nothing (offsetIsCoord (r1 4) 1)
    , testCase "Reflect101: (.+^) at the same displacement is total" $ do
        assertEqual "" (r1 1) (r1 0 .+^ (-1))
        assertEqual "" (r1 3) (r1 4 .+^ 1)
    ]

-- | Both types are still bounded axes with real edges, and 'axisDistance'
-- still measures straight -- neither changes just because ('.+^') bounces.
boundaryAndDistanceStayDefaultTests :: TestTree
boundaryAndDistanceStayDefaultTests =
  testGroup
    "axisBoundary and axisDistance keep the bounded-axis defaults"
    [ testCase "Reflective still has two real edges" $ do
        assertEqual "" (Just AtMin) (axisBoundary (rf 0))
        assertEqual "" (Just AtMax) (axisBoundary (rf 4))
        assertEqual "" Nothing (axisBoundary (rf 2))
    , testCase "Reflect101 still has two real edges" $ do
        assertEqual "" (Just AtMin) (axisBoundary (r1 0))
        assertEqual "" (Just AtMax) (axisBoundary (r1 4))
        assertEqual "" Nothing (axisBoundary (r1 2))
    , testProperty "Reflective distance is straight, not the shorter bounce" $
        \(a :: Reflective 5) (b :: Reflective 5) ->
          axisDistance a b ===
          abs (ordinalToInt (view asOrdinal a) - ordinalToInt (view asOrdinal b))
    , testProperty "Reflect101 distance is straight, not the shorter bounce" $
        \(a :: Reflect101 5) (b :: Reflect101 5) ->
          axisDistance a b ===
          abs (ordinalToInt (view asOrdinal a) - ordinalToInt (view asOrdinal b))
    ]

-- | A bounce off a wall reverses the walker's sense of direction on an odd
-- number of hits, and 'axisFrameFlips' reports exactly that parity.
frameFlipExamples :: TestTree
frameFlipExamples =
  testGroup
    "axisFrameFlips reports the bounce parity, not just whether one occurred"
    [ testCase "Reflective: no wall hit, no flip" $
        assertEqual "" False (axisFrameFlips (rf 2) 1)
    , testCase "Reflective: one wall hit, flips" $
        assertEqual "" True (axisFrameFlips (rf 0) (-1))
    , -- 10 is one full period at size 5: hits both walls once.
      testCase "Reflective: two wall hits (there and back), no net flip" $
        assertEqual "" False (axisFrameFlips (rf 0) 10)
    , testCase "Reflect101: no wall hit, no flip" $
        assertEqual "" False (axisFrameFlips (r1 2) 1)
    , testCase "Reflect101: one wall hit, flips" $
        assertEqual "" True (axisFrameFlips (r1 0) (-1))
    , testCase "Reflect101: the degenerate size-1 axis never bounces" $
        assertEqual "" False
          (axisFrameFlips (Reflect101 (unsafeOrdinal 0) :: Reflect101 1) 37)
    , -- A choice, not a derivable fact: landing exactly on the far wall counts as
      -- flipped even though the position formula's two branches agree there.
      testCase "Reflect101: landing exactly on the far wall counts as flipped" $
        assertEqual "" True (axisFrameFlips (r1 0) (-4))
    ]

frameFlipAgreesWithReferenceTests :: TestTree
frameFlipAgreesWithReferenceTests =
  testGroup
    "axisFrameFlips agrees with the parity of the reference's bounce count"
    [ testProperty "Reflective, size 5" $ \(c :: Reflective 5) (d :: Int) ->
        let i = ordinalToInt (view asOrdinal c)
         in axisFrameFlips c d === bounceFlipRef 5 (i + d)
    , testProperty "Reflective, size 1" $ \(c :: Reflective 1) (d :: Int) ->
        let i = ordinalToInt (view asOrdinal c)
         in axisFrameFlips c d === bounceFlipRef 1 (i + d)
    , -- Excludes multiples of @m@, where 'reflect101FlipRef' doesn't claim to agree.
      testProperty "Reflect101, size 5" $ \(c :: Reflect101 5) (d :: Int) ->
        let i = ordinalToInt (view asOrdinal c)
         in (i + d) `mod` 4 /= 0 ==> axisFrameFlips c d === reflect101FlipRef 4 (i + d)
    , testProperty "Reflect101, size 1 (the degenerate axis)" $
        \(c :: Reflect101 1) (d :: Int) ->
          let i = ordinalToInt (view asOrdinal c)
           in axisFrameFlips c d === reflect101FlipRef 0 (i + d)
    ]

reflectiveTests :: TestTree
reflectiveTests =
  testGroup
    "Reflective and Reflect101"
    [ bounceExamples
    , reflect101Examples
    , closedFormAgreesWithReferenceTests
    , offsetIsCoordStaysCheckedTests
    , boundaryAndDistanceStayDefaultTests
    , frameFlipExamples
    , frameFlipAgreesWithReferenceTests
    ]
