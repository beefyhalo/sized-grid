{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Tests for the two bounce boundary policies, 'Reflective' and
-- 'Reflect101' (@sized-grid-kvs@).
--
-- Both types are total on ('.+^') by construction --- the closed form always
-- lands on a valid 'Ordinal' --- so the interesting obligation is not "does
-- it stay in range" (that is 'isCoordLaws', run on both in @Main.hs@) but
-- "does the closed form compute the bounce the issue actually specified".
-- The reference implementations below are direct transcriptions of the
-- recursive definitions in the issue, deliberately written the slow, obvious
-- way so that a bug shared between the closed form and its reference would
-- have to be a bug in the reference too.
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

-- | The reference the issue itself gives: a billiard bounce off two walls
-- @size@ apart, written as the obviously-correct recursive walk rather than
-- the closed form under test.
bounceRef :: Int -> Int -> Int
bounceRef size = go
  where
    go i
      | i < 0 = go (negate i - 1)
      | i >= size = go (2 * size - 1 - i)
      | otherwise = i

-- | The same shape of reference for 'Reflect101': a mirror around the edge
-- /cell/ rather than the wall beyond it, so the fixed points are @0@ and
-- @m@ instead of the reflection landing between @-1@\/@0@ and
-- @m@\/@m + 1@. Degenerate at @m == 0@, where the single cell has no
-- neighbour to mirror around and every displacement is absorbed.
reflect101Ref :: Int -> Int -> Int
reflect101Ref m
  | m == 0 = const 0
  | otherwise = go
  where
    go i
      | i < 0 = go (negate i)
      | i > m = go (2 * m - i)
      | otherwise = i

-- | Whether 'bounceRef' bounces an odd number of times reaching @i@ --- the
-- seam rule's frame half (sized-grid-o1n), computed the same slow, obvious
-- way 'bounceRef' itself is, so a bug shared between it and the closed form
-- under test ('Data.Grid.Sized.axisFrameFlips') would have to be a bug here
-- too.
--
-- Well-defined for every integer, with no boundary caveat: 'Reflective''s
-- walls sit at half-integers (@-0.5@, @size - 0.5@, ...) and so are never
-- landed on by an integer position, which is what makes counting bounces
-- along /this/ deterministic reduction agree with the closed form's parity
-- at every input --- checked without exception by the property tests below.
-- Contrast 'reflect101FlipRef', where the analogous walls sit ON lattice
-- points and the two reductions genuinely part ways there.
bounceFlipRef :: Int -> Int -> Bool
bounceFlipRef size = go False
  where
    go flipped i
      | i < 0 = go (not flipped) (negate i - 1)
      | i >= size = go (not flipped) (2 * size - 1 - i)
      | otherwise = flipped

-- | The same shape of reference for 'Reflect101', tracking parity through
-- 'reflect101Ref''s recursion.
--
-- Unlike 'bounceFlipRef', this is not checked at every input: at @i@ an
-- exact multiple of @m@, 'Reflect101''s walls (unlike 'Reflective''s) sit ON
-- the lattice rather than between two cells, so the point is fixed by more
-- than one combination of reflections and translations, and nothing forces
-- this recursion's particular reduction path to agree with whatever
-- convention the closed form under test picks --- see the note on
-- 'Data.Grid.Sized.Coord.Reflect101.mirrorAt'. The property test below
-- excludes exactly that case and 'frameFlipExamples' pins the convention
-- there explicitly instead.
reflect101FlipRef :: Int -> Int -> Bool
reflect101FlipRef m
  | m == 0 = const False
  | otherwise = go False
  where
    go flipped i
      | i < 0 = go (not flipped) (negate i)
      | i > m = go (not flipped) (2 * m - i)
      | otherwise = flipped

-- | Reflective 5, read off a bare 'Int' position.
rf :: Int -> Reflective 5
rf = Reflective . unsafeOrdinal

-- | Reflect101 5, read off a bare 'Int' position.
r1 :: Int -> Reflect101 5
r1 = Reflect101 . unsafeOrdinal

bounceExamples :: TestTree
bounceExamples =
  testGroup
    "Reflective bounces off the wall, not around the edge cell"
    [ testCase "-1 becomes 0" $ assertEqual "" (rf 0) (rf 0 .+^ (-1))
    , testCase "-2 becomes 1" $ assertEqual "" (rf 1) (rf 0 .+^ (-2))
    , -- One past the top (index 5, since the valid range is 0..4) bounces to
      -- the top itself, and two past it bounces to one below the top --- the
      -- mirror image of the -1\/-2 cases above the other wall.
      testCase "size becomes size - 1" $
        assertEqual "" (rf 4) (rf 0 .+^ 5)
    , testCase "size + 1 becomes size - 2" $
        assertEqual "" (rf 3) (rf 0 .+^ 6)
    , -- The edge cell is visited twice in a row on the way out and back,
      -- which is what distinguishes this from 'Reflect101' below.
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
    , -- Past the top (index 5, one past the valid range 0..4, mirrored around
      -- the top cell 4) lands one below the top rather than on it.
      testCase "size becomes size - 2" $
        assertEqual "" (r1 3) (r1 0 .+^ 5)
    , testCase "the top cell is its own mirror image" $
        assertEqual "" (r1 4) (r1 4 .+^ 0)
    , -- A single-cell axis has no neighbour to mirror around: every
      -- displacement is absorbed by the one value it has.
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

-- | 'offsetIsCoord' stays the checked default on both types: the bounce lives
-- in ('.+^') alone, so stepping off the axis is reported with 'Nothing'
-- rather than folded back inside. This is the fact the issue's "fit with the
-- thesis" section states, and it is what tells the two types apart from
-- 'Data.Grid.Sized.Coord.Periodic.Periodic', whose 'offsetIsCoord' is total.
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
-- still measures straight: neither fact changes just because ('.+^') bounces.
-- This is the deliberate decision documented on each instance, checked here
-- rather than left to be rediscovered --- the same treatment
-- 'Data.Grid.Sized.Coord.Periodic.Periodic''s override gets in "Test.Boundary".
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

-- | The seam rule's frame half (sized-grid-o1n): a bounce off a wall reverses
-- the walker's sense of direction on an odd number of hits, and
-- 'axisFrameFlips' reports exactly that parity.
frameFlipExamples :: TestTree
frameFlipExamples =
  testGroup
    "axisFrameFlips reports the bounce parity, not just whether one occurred"
    [ testCase "Reflective: no wall hit, no flip" $
        assertEqual "" False (axisFrameFlips (rf 2) 1)
    , testCase "Reflective: one wall hit, flips" $
        assertEqual "" True (axisFrameFlips (rf 0) (-1))
    , testCase "Reflective: two wall hits (there and back), no net flip" $
        -- size 5: 0 + 10 hits the high wall once and the low wall once ---
        -- one full period --- landing back at 0 facing the original way.
        assertEqual "" False (axisFrameFlips (rf 0) 10)
    , testCase "Reflect101: no wall hit, no flip" $
        assertEqual "" False (axisFrameFlips (r1 2) 1)
    , testCase "Reflect101: one wall hit, flips" $
        assertEqual "" True (axisFrameFlips (r1 0) (-1))
    , testCase "Reflect101: the degenerate size-1 axis never bounces" $
        assertEqual "" False
          (axisFrameFlips (Reflect101 (unsafeOrdinal 0) :: Reflect101 1) 37)
    , -- The convention 'Data.Grid.Sized.Coord.Reflect101.mirrorAt' documents:
      -- landing exactly on the far wall (here, index @m = 4@ of a size-5
      -- axis) is already facing the mirrored direction, even though the
      -- position formula's two branches agree on the value there. Pinned
      -- explicitly because it is a choice, not a derivable fact --- see
      -- 'reflect101FlipRef''s note on why the property test below cannot
      -- check it.
      testCase "Reflect101: landing exactly on the far wall counts as flipped" $
        assertEqual "" True (axisFrameFlips (r1 0) (-4))
    ]

-- | 'axisFrameFlips' agrees with the parity of the recursive reference's own
-- bounce count, at the same sizes 'closedFormAgreesWithReferenceTests' checks
-- the position at.
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
    , -- Excludes @(i + d) \`mod\` 4 == 0@: 'reflect101FlipRef' does not claim
      -- to agree with the closed form exactly on the multiples of @m@, per
      -- its own note, and the explicit case in 'frameFlipExamples' covers
      -- that boundary instead.
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
