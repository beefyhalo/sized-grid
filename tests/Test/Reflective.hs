{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for the two bounce boundary policies, 'Reflective' and 'Reflect101'.
--
-- Both are total on ('.+^') by construction, so the obligation checked here is
-- not "does it stay in range" but "does the closed form compute the right
-- bounce" -- checked against slow, obviously-correct recursive references.
--
-- The 'IsCoord' frame-flip law -- a checked step that succeeds has not hit a
-- wall, so the frame does not turn -- is checked here too, over every axis
-- type rather than just these two. It lives here because these two are the
-- only policies with any way to break it: everywhere else
-- 'axisFrameFlipsIsCoord' takes the constant-'False' default.
module Test.Reflective
  ( reflectiveTests,
  )
where

import Control.Lens (view)
import Data.AffineSpace (AffineSpace, Diff, (.+^))
import Data.Grid.Sized
import GHC.TypeLits (KnownNat)
import Test.Arbitrary ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (Property, testProperty, (===))

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

-- | Tracks parity through 'reflect101Ref''s recursion, plus the one thing the
-- recursion cannot derive. At @i@ an exact multiple of @m@ the wall sits on
-- the lattice, so the walker has landed on a mirror rather than crossed one,
-- and the parity is genuinely ambiguous -- whichever answer the fold order
-- happens to reach there is an artefact of the fold, not a fact. The library
-- resolves every such landing as no turn, because the 'IsCoord' law leaves it
-- no choice (sized-grid-c0s9), and the guard below says so once so the
-- property can be total rather than filtered.
reflect101FlipRef :: Int -> Int -> Bool
reflect101FlipRef m
  | m == 0 = const False
  | otherwise = \i -> i `mod` m /= 0 && go False i
  where
    go flipped i
      | i < 0 = go (not flipped) (negate i)
      | i > m = go (not flipped) (2 * m - i)
      | otherwise = flipped

rf :: Int -> Reflective 5
rf = Reflective . unsafeOrdinal

r1 :: Int -> Reflect101 5
r1 = Reflect101 . unsafeOrdinal

plusAgreesWith ::
  (AffineSpace x, Diff x ~ Int, IsCoordLifted x) =>
  (Int -> Int) ->
  x ->
  Int ->
  Property
plusAgreesWith ref c d = toAxisIndex (c .+^ d) === ref (toAxisIndex c + d)

flipAgreesWith :: (IsCoordLifted x) => (Int -> Bool) -> x -> Int -> Property
flipAgreesWith ref c d = axisFrameFlips c d === ref (toAxisIndex c + d)

-- | The two non-reflecting bounded\/unbounded policies, for the law tests
-- below: they take the constant-'False' default, and the point of including
-- them is that the law is 'IsCoord'\'s rather than the mirrors'.
hwOf :: forall n. (KnownNat n) => Int -> Clamped n
hwOf = Clamped . unsafeOrdinal

peOf :: forall n. (KnownNat n) => Int -> Periodic n
peOf = Periodic . unsafeOrdinal

bounceExamples :: TestTree
bounceExamples =
  testGroup
    "Reflective bounces off the wall, not around the edge cell"
    [ testCase "-1 becomes 0" $ assertEqual "" (rf 0) (rf 0 .+^ (-1)),
      testCase "-2 becomes 1" $ assertEqual "" (rf 1) (rf 0 .+^ (-2)),
      -- Mirror image of the -1/-2 cases above, at the other wall.
      testCase "size becomes size - 1" $
        assertEqual "" (rf 4) (rf 0 .+^ 5),
      testCase "size + 1 becomes size - 2" $
        assertEqual "" (rf 3) (rf 0 .+^ 6),
      -- Distinguishes this from 'Reflect101': the edge cell is visited twice in a row.
      testCase "stepping onto the top and one past it both land on the top" $ do
        assertEqual "onto" (rf 4) (rf 3 .+^ 1)
        assertEqual "past" (rf 4) (rf 3 .+^ 2)
    ]

reflect101Examples :: TestTree
reflect101Examples =
  testGroup
    "Reflect101 mirrors around the edge cell, never repeating it"
    [ testCase "-1 becomes 1, not 0" $ assertEqual "" (r1 1) (r1 0 .+^ (-1)),
      testCase "-2 becomes 2" $ assertEqual "" (r1 2) (r1 0 .+^ (-2)),
      -- Mirrored around the top cell (4), so it lands one below the top, not on it.
      testCase "size becomes size - 2" $
        assertEqual "" (r1 3) (r1 0 .+^ 5),
      testCase "the top cell is its own mirror image" $
        assertEqual "" (r1 4) (r1 4 .+^ 0),
      -- No neighbour to mirror around, so every displacement is absorbed.
      testCase "a one-cell axis absorbs every displacement" $
        assertEqual
          ""
          (Reflect101 (unsafeOrdinal 0) :: Reflect101 1)
          (Reflect101 (unsafeOrdinal 0) .+^ 37)
    ]

closedFormAgreesWithReferenceTests :: TestTree
closedFormAgreesWithReferenceTests =
  testGroup
    "the closed form agrees with the issue's own recursive definition"
    [ testProperty "Reflective, size 5" $ \(c :: Reflective 5) (d :: Int) ->
        plusAgreesWith (bounceRef 5) c d,
      testProperty "Reflective, size 1" $ \(c :: Reflective 1) (d :: Int) ->
        plusAgreesWith (bounceRef 1) c d,
      testProperty "Reflect101, size 5" $ \(c :: Reflect101 5) (d :: Int) ->
        plusAgreesWith (reflect101Ref 4) c d,
      testProperty "Reflect101, size 2" $ \(c :: Reflect101 2) (d :: Int) ->
        plusAgreesWith (reflect101Ref 1) c d,
      testProperty "Reflect101, size 1 (the degenerate axis)" $ \(c :: Reflect101 1) (d :: Int) ->
        plusAgreesWith (reflect101Ref 0) c d
    ]

-- | The bounce lives in ('.+^') alone; 'offsetIsCoord' still reports 'Nothing'
-- on stepping off the axis, unlike 'Data.Grid.Sized.Coord.Periodic.Periodic'.
offsetIsCoordStaysCheckedTests :: TestTree
offsetIsCoordStaysCheckedTests =
  testGroup
    "offsetIsCoord reports leaving the axis; the bounce is (.+^) alone"
    [ testCase "Reflective: one step past either wall is Nothing" $ do
        assertEqual "" Nothing (offsetIsCoord (rf 0) (-1))
        assertEqual "" Nothing (offsetIsCoord (rf 4) 1),
      testCase "Reflective: (.+^) at the same displacement is total" $ do
        assertEqual "" (rf 0) (rf 0 .+^ (-1))
        assertEqual "" (rf 4) (rf 4 .+^ 1),
      testCase "Reflect101: one step past either wall is Nothing" $ do
        assertEqual "" Nothing (offsetIsCoord (r1 0) (-1))
        assertEqual "" Nothing (offsetIsCoord (r1 4) 1),
      testCase "Reflect101: (.+^) at the same displacement is total" $ do
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
        assertEqual "" Nothing (axisBoundary (rf 2)),
      testCase "Reflect101 still has two real edges" $ do
        assertEqual "" (Just AtMin) (axisBoundary (r1 0))
        assertEqual "" (Just AtMax) (axisBoundary (r1 4))
        assertEqual "" Nothing (axisBoundary (r1 2)),
      testProperty "Reflective distance is straight, not the shorter bounce" $
        \(a :: Reflective 5) (b :: Reflective 5) ->
          axisDistance a b
            === abs (ordinalToInt (view asOrdinal a) - ordinalToInt (view asOrdinal b)),
      testProperty "Reflect101 distance is straight, not the shorter bounce" $
        \(a :: Reflect101 5) (b :: Reflect101 5) ->
          axisDistance a b
            === abs (ordinalToInt (view asOrdinal a) - ordinalToInt (view asOrdinal b))
    ]

-- | A bounce off a wall reverses the walker's sense of direction on an odd
-- number of hits, and 'axisFrameFlips' reports exactly that parity.
frameFlipExamples :: TestTree
frameFlipExamples =
  testGroup
    "axisFrameFlips reports the bounce parity, not just whether one occurred"
    [ testCase "Reflective: no wall hit, no flip" $
        assertEqual "" False (axisFrameFlips (rf 2) 1),
      testCase "Reflective: one wall hit, flips" $
        assertEqual "" True (axisFrameFlips (rf 0) (-1)),
      -- 10 is one full period at size 5: hits both walls once.
      testCase "Reflective: two wall hits (there and back), no net flip" $
        assertEqual "" False (axisFrameFlips (rf 0) 10),
      testCase "Reflect101: no wall hit, no flip" $
        assertEqual "" False (axisFrameFlips (r1 2) 1),
      testCase "Reflect101: one wall hit, flips" $
        assertEqual "" True (axisFrameFlips (r1 0) (-1)),
      testCase "Reflect101: the degenerate size-1 axis never bounces" $
        assertEqual
          ""
          False
          (axisFrameFlips (Reflect101 (unsafeOrdinal 0) :: Reflect101 1) 37),
      -- A choice, not a derivable fact -- the position formula's two branches
      -- agree on the mirror cell, so only the flag is at stake -- and the
      -- choice the 'IsCoord' law forces: a step the bounds check accepts must
      -- not report a turn (sized-grid-c0s9).
      testCase "Reflect101: landing exactly on a mirror is not a turn" $ do
        assertEqual "onto the far mirror" False (axisFrameFlips (r1 0) 4)
        assertEqual "onto it the mirrored way round" False (axisFrameFlips (r1 0) (-4))
        assertEqual "standing still on it" False (axisFrameFlips (r1 4) 0),
      testCase "Reflect101: one step past the far mirror does turn" $
        assertEqual "" True (axisFrameFlips (r1 0) 5)
    ]

frameFlipAgreesWithReferenceTests :: TestTree
frameFlipAgreesWithReferenceTests =
  testGroup
    "axisFrameFlips agrees with the parity of the reference's bounce count"
    [ testProperty "Reflective, size 5" $ \(c :: Reflective 5) (d :: Int) ->
        flipAgreesWith (bounceFlipRef 5) c d,
      testProperty "Reflective, size 1" $ \(c :: Reflective 1) (d :: Int) ->
        flipAgreesWith (bounceFlipRef 1) c d,
      testProperty "Reflect101, size 5" $ \(c :: Reflect101 5) (d :: Int) ->
        flipAgreesWith (reflect101FlipRef 4) c d,
      -- Every cell is a mirror when @m == 1@, so nothing ever turns.
      testProperty "Reflect101, size 2 (every cell a mirror)" $
        \(c :: Reflect101 2) (d :: Int) ->
          flipAgreesWith (reflect101FlipRef 1) c d,
      testProperty "Reflect101, size 1 (the degenerate axis)" $
        \(c :: Reflect101 1) (d :: Int) ->
          flipAgreesWith (reflect101FlipRef 0) c d
    ]

-- | Every (position, displacement) pair on one axis where the bounds check
-- accepts the step and 'axisFrameFlips' nonetheless reports a turn. The
-- 'IsCoord' law says this list is empty, whatever the policy.
--
-- Exhaustive rather than sampled: an axis is a handful of cells and the
-- displacements that matter all fit inside two periods, so the whole domain
-- is cheaper to check than a generator is to tune -- and the five cases
-- 'Reflect101' used to have were exactly the five landing on one cell, which
-- is the shape a generator is worst at finding.
lawViolations :: forall x. (IsCoordLifted x) => [(Int, Int)]
lawViolations =
  [ (toAxisIndex c, d)
  | c <- allCoordLike @(CoordNat x) @(CoordContainer x) :: [x],
    d <- [negate reach .. reach],
    axisFrameFlips c d,
    Just _ <- [offsetIsCoord c d]
  ]
  where
    reach = 2 * ordinalSize @(CoordNat x)

-- | One walker stepped along an axis by the rule the law licenses: take the
-- checked step, and turn only if the axis says the step turned you. The law
-- makes the second half a no-op, which is the point -- this is
-- @transportCoordMaybe@'s axis-level core, and it is why that operation needs
-- no fold of its own (sized-grid-pc93, sized-grid-qbal).
--
-- Fuelled because a torus never stops.
checkedWalk :: forall x. (IsCoordLifted x) => Int -> x -> Int -> [(Int, Int)]
checkedWalk fuel c d
  | fuel <= 0 = []
  | otherwise =
      (toAxisIndex c, d)
        : case offsetIsCoord c d of
          Nothing -> []
          Just c' ->
            checkedWalk (fuel - 1) c' (if axisFrameFlips c d then negate d else d)

frameFlipLawTests :: TestTree
frameFlipLawTests =
  testGroup
    "a checked step that succeeds does not turn the frame"
    [ testGroup
        "no axis type reports a flip on a step the bounds check accepts"
        [ testCase "Ordinal" $ assertEqual "" [] (lawViolations @(Ordinal 5)),
          testCase "Clamped" $ assertEqual "" [] (lawViolations @(Clamped 5)),
          testCase "Periodic" $ assertEqual "" [] (lawViolations @(Periodic 5)),
          testCase "Reflective" $ assertEqual "" [] (lawViolations @(Reflective 5)),
          testCase "Reflect101" $ assertEqual "" [] (lawViolations @(Reflect101 5)),
          -- The sizes where the two mirrors coincide or sit next to each
          -- other, so every cell is a mirror and the fixed point is not an
          -- edge case but the whole axis.
          testCase "Reflective, size 1" $
            assertEqual "" [] (lawViolations @(Reflective 1)),
          testCase "Reflect101, size 2" $
            assertEqual "" [] (lawViolations @(Reflect101 2)),
          testCase "Reflect101, size 1" $
            assertEqual "" [] (lawViolations @(Reflect101 1))
        ],
      -- The spike's finding, at axis scope: before sized-grid-c0s9 the
      -- 'Reflect101' walker turned around on cell 3 and walked back, while
      -- every other policy ran to the wall and stopped.
      testGroup
        "a walker taking checked steps runs to the wall and stops there"
        [ testCase "Ordinal" $
            assertEqual
              ""
              [(0, 1), (1, 1), (2, 1), (3, 1), (4, 1)]
              (checkedWalk 10 (unsafeOrdinal @5 0) 1),
          testCase "Clamped" $
            assertEqual
              ""
              [(0, 1), (1, 1), (2, 1), (3, 1), (4, 1)]
              (checkedWalk 10 (hwOf @5 0) 1),
          testCase "Reflective" $
            assertEqual
              ""
              [(0, 1), (1, 1), (2, 1), (3, 1), (4, 1)]
              (checkedWalk 10 (rf 0) 1),
          testCase "Reflect101" $
            assertEqual
              ""
              [(0, 1), (1, 1), (2, 1), (3, 1), (4, 1)]
              (checkedWalk 10 (r1 0) 1),
          -- Not a wall to stop at: the torus keeps its heading and laps.
          testCase "Periodic laps instead, still without turning" $
            assertEqual
              ""
              [(0, 1), (1, 1), (2, 1), (3, 1), (4, 1), (0, 1), (1, 1)]
              (checkedWalk 7 (peOf @5 0) 1)
        ]
    ]

reflectiveTests :: TestTree
reflectiveTests =
  testGroup
    "Reflective and Reflect101"
    [ bounceExamples,
      reflect101Examples,
      closedFormAgreesWithReferenceTests,
      offsetIsCoordStaysCheckedTests,
      boundaryAndDistanceStayDefaultTests,
      frameFlipExamples,
      frameFlipAgreesWithReferenceTests,
      frameFlipLawTests
    ]
