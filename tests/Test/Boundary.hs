-- | Tests for boundary detection: which end of its axis a coordinate sits at,
-- and the whole-coordinate questions built on it.
module Test.Boundary
  ( boundaryTests,
  )
where

import Control.Lens
  ( IndexedTraversal',
    asIndex,
    indices,
    itraversed,
    lengthOf,
    sumOf,
    toListOf,
    (&),
    (.~),
  )
import Data.Functor.Rep (tabulate)
import Data.Grid.Sized
import Data.List (sort)
import Data.Maybe (fromJust, isJust, isNothing)
import GHC.TypeLits (KnownNat)
import Test.Arbitrary ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty, (===), (==>))

-- | The bounded space: it has real edges.
hwOf :: (KnownNat n) => Int -> Clamped n
hwOf = Clamped . fromJust . numToOrdinal

-- | The torus: it has none.
peOf :: (KnownNat n) => Int -> Periodic n
peOf = Periodic . fromJust . numToOrdinal

hw :: Int -> Clamped 5
hw = hwOf

pe :: Int -> Periodic 5
pe = peOf

axisBoundaryTests :: TestTree
axisBoundaryTests =
  testGroup
    "axisBoundary reads one axis's own boundary policy"
    [ testCase "a bounded axis is AtMin at the bottom" $
        assertEqual "" (Just AtMin) (axisBoundary (hw 0)),
      testCase "a bounded axis is AtMax at the top" $
        assertEqual "" (Just AtMax) (axisBoundary (hw 4)),
      testCase "a bounded axis is interior in between" $ do
        assertEqual "1" Nothing (axisBoundary (hw 1))
        assertEqual "2" Nothing (axisBoundary (hw 2))
        assertEqual "3" Nothing (axisBoundary (hw 3)),
      testCase "a torus has no edges anywhere" $ do
        assertEqual "bottom" Nothing (axisBoundary (pe 0))
        assertEqual "middle" Nothing (axisBoundary (pe 2))
        assertEqual "top" Nothing (axisBoundary (pe 4)),
      testCase "an Ordinal takes the same default as Clamped" $ do
        assertEqual "bottom" (Just AtMin) (axisBoundary (unsafeOrdinal @5 0))
        assertEqual "middle" Nothing (axisBoundary (unsafeOrdinal @5 2))
        assertEqual "top" (Just AtMax) (axisBoundary (unsafeOrdinal @5 4)),
      -- A one-cell axis is both ends at once; 'AtMin' is the one the bounds check reaches first.
      testCase "the only value of a one-cell bounded axis is at an edge" $
        assertEqual "" (Just AtMin) (axisBoundary (hwOf @1 0)),
      testCase "the only value of a one-cell torus is still edgeless" $
        assertEqual "" Nothing (axisBoundary (peOf @1 0))
    ]

-- | Consistency with 'offsetIsCoord' is the law that makes a boundary mean
-- something: a coordinate is at an edge exactly when stepping that way leaves
-- the space. Stated as a property rather than enforced by the types, in the
-- same way 'axisDistanceIsCoord' is tied to 'offsetIsCoord'.
axisBoundaryLawTests :: TestTree
axisBoundaryLawTests =
  testGroup
    "axisBoundary agrees with offsetIsCoord"
    [ testProperty "a bounded axis is interior iff both steps succeed" $ \(c :: Clamped 5) ->
        isNothing (axisBoundary c)
          === (isJust (offsetIsCoord c (-1)) && isJust (offsetIsCoord c 1)),
      testProperty "a bounded axis is AtMin iff the step down fails" $ \(c :: Clamped 5) ->
        (axisBoundary c == Just AtMin) === isNothing (offsetIsCoord c (-1)),
      testProperty "a bounded axis is AtMax iff the step up fails" $ \(c :: Clamped 5) ->
        (axisBoundary c == Just AtMax) === isNothing (offsetIsCoord c 1),
      testProperty "a torus is interior everywhere, and every step succeeds" $ \(c :: Periodic 5) ->
        isNothing (axisBoundary c)
          === (isJust (offsetIsCoord c (-1)) && isJust (offsetIsCoord c 1))
    ]

hwc :: Int -> Int -> Coord '[Clamped 5, Clamped 5]
hwc r c = hw r :| hw c :| EmptyCoord

pec :: Int -> Int -> Coord '[Periodic 5, Periodic 5]
pec r c = pe r :| pe c :| EmptyCoord

-- | One bounded axis and one torus axis, to pin down that the policy is read
-- per axis rather than once for the whole coordinate.
mixc :: Int -> Int -> Coord '[Clamped 5, Periodic 5]
mixc r c = hw r :| pe c :| EmptyCoord

axisBoundariesTests :: TestTree
axisBoundariesTests =
  testGroup
    "axisBoundaries reports every axis, first axis first"
    [ testCase "a bounded corner is at an end on both axes" $
        assertEqual "" [Just AtMin, Just AtMax] (axisBoundaries (hwc 0 4)),
      testCase "a bounded edge is at an end on one axis only" $
        assertEqual "" [Just AtMin, Nothing] (axisBoundaries (hwc 0 2)),
      testCase "a bounded interior cell is at no end at all" $
        assertEqual "" [Nothing, Nothing] (axisBoundaries (hwc 2 2)),
      testCase "the torus axis of a mixed coord never reports an end" $
        assertEqual "" [Just AtMin, Nothing] (axisBoundaries (mixc 0 0)),
      testCase "a coord with no axes has no boundaries to report" $
        assertEqual "" [] (axisBoundaries EmptyCoord)
    ]

onBoundaryTests :: TestTree
onBoundaryTests =
  testGroup
    "onBoundary: any axis at an end"
    [ testCase "a bounded corner is on the boundary" $
        assertBool "" (onBoundary (hwc 0 0)),
      testCase "a bounded edge is on the boundary" $
        assertBool "" (onBoundary (hwc 0 2)),
      testCase "a bounded interior cell is not" $
        assertBool "" (not (onBoundary (hwc 2 2))),
      testCase "a torus has no boundary anywhere" $ do
        assertBool "corner" (not (onBoundary (pec 0 0)))
        assertBool "interior" (not (onBoundary (pec 2 2))),
      testCase "the bounded axis of a mixed coord still has edges" $ do
        assertBool "at min" (onBoundary (mixc 0 3))
        assertBool "at max" (onBoundary (mixc 4 3))
        assertBool "interior" (not (onBoundary (mixc 2 0))),
      -- A space with one point has no edge to be on. 'axisBoundaries' is
      -- empty, so there is no axis reporting an end, and the fold over
      -- nothing is 'False'.
      testCase "a coord with no axes is not on a boundary" $
        assertBool "" (not (onBoundary EmptyCoord))
    ]

isCornerTests :: TestTree
isCornerTests =
  testGroup
    "isCorner: every axis at an end"
    [ testCase "all four corners of a bounded grid are corners" $ do
        assertBool "0 0" (isCorner (hwc 0 0))
        assertBool "0 4" (isCorner (hwc 0 4))
        assertBool "4 0" (isCorner (hwc 4 0))
        assertBool "4 4" (isCorner (hwc 4 4)),
      testCase "an edge cell is not a corner" $
        assertBool "" (not (isCorner (hwc 0 2))),
      testCase "an interior cell is not a corner" $
        assertBool "" (not (isCorner (hwc 2 2))),
      -- The bug this whole issue was filed for: the hand-rolled check in
      -- ../aoc compares each axis against @natVal - 1@, which reports four
      -- corners on a torus. A torus has none.
      testProperty "a torus has no corners at all" $ \(c :: Coord '[Periodic 5, Periodic 5]) ->
        not (isCorner c),
      testCase "a mixed coord has no corners either: one axis never ends" $ do
        assertBool "0 0" (not (isCorner (mixc 0 0)))
        assertBool "4 4" (not (isCorner (mixc 4 4))),
      testCase "the single cell of a 1x1 bounded grid is a corner" $
        assertBool
          ""
          (isCorner (hwOf @1 0 :| hwOf @1 0 :| EmptyCoord)),
      -- Not a vacuous 'True'. A corner is a boundary point, and a space
      -- with one point has no boundary, so answering 'True' here would
      -- break @isCorner c ==> onBoundary c@ on the one coord where it is
      -- easiest to get wrong.
      testCase "a coord with no axes is not a corner" $
        assertBool "" (not (isCorner EmptyCoord)),
      testProperty "every corner is on the boundary" $ \(c :: Coord '[Clamped 5, Clamped 5]) ->
        isCorner c ==> onBoundary c
    ]

-- | 'onBoundary' and 'isCorner' answer from 'npAnyBoundary'\/'npAllBoundary',
-- which fold the axes directly instead of folding the list 'axisBoundaries'
-- builds. Nothing in the types ties the two together, so these say the
-- answers still match, on axis lists that mix every boundary policy.
fusedBoundaryTests :: TestTree
fusedBoundaryTests =
  testGroup
    "the fused folds agree with axisBoundaries"
    [ testProperty "onBoundary is any isJust, on bounded axes" $ \(c :: Coord '[Clamped 5, Clamped 5]) ->
        onBoundary c === any isJust (axisBoundaries c),
      testProperty "and on mixed policies" $ \(c :: Coord '[Clamped 5, Periodic 4, Reflective 3, Reflect101 5]) ->
        onBoundary c === any isJust (axisBoundaries c),
      testProperty "isCorner is a non-empty all isJust, on bounded axes" $ \(c :: Coord '[Clamped 5, Clamped 5]) ->
        isCorner c === cornerByList c,
      testProperty "and on mixed policies" $ \(c :: Coord '[Clamped 5, Periodic 4, Reflective 3, Reflect101 5]) ->
        isCorner c === cornerByList c,
      testProperty "on a single axis, where the empty tail is the vacuous case" $ \(c :: Coord '[Clamped 5]) ->
        isCorner c === cornerByList c,
      testCase "including on the empty coord, where the list version is False" $ do
        assertEqual "onBoundary" (any isJust (axisBoundaries EmptyCoord)) (onBoundary EmptyCoord)
        assertEqual "isCorner" (cornerByList EmptyCoord) (isCorner EmptyCoord)
    ]
  where
    cornerByList :: (IsCoordList cs) => Coord cs -> Bool
    cornerByList c =
      case axisBoundaries c of
        [] -> False
        bs -> all isJust bs

-- | The property that makes the interior worth naming: a cellular automaton
-- special-cases edge cells precisely because their neighbourhood is short.
interiorTests :: TestTree
interiorTests =
  testGroup
    "the interior is where the full Moore neighbourhood exists"
    [ testProperty "a bounded cell is interior iff it has all 8 neighbours" $ \(c :: Coord '[Clamped 5, Clamped 5]) ->
        not (onBoundary c) === (length (neighbours c) == 8),
      testCase "the interior of a bounded 5x5 is the 3x3 inside it" $
        assertEqual
          ""
          [hwc r c | r <- [1 .. 3], c <- [1 .. 3]]
          (interiorCoords @'[Clamped 5, Clamped 5]),
      testCase "interiorCoords is allCoord without the boundary" $
        assertEqual
          ""
          (filter (not . onBoundary) (allCoord @'[Clamped 5, Clamped 5]))
          (interiorCoords @'[Clamped 5, Clamped 5]),
      testCase "every cell of a torus is interior" $
        assertEqual
          ""
          (allCoord @'[Periodic 5, Periodic 5])
          (interiorCoords @'[Periodic 5, Periodic 5]),
      testCase "a mixed coord keeps every torus column of its inner rows" $
        assertEqual
          ""
          (sort [mixc r c | r <- [1 .. 3], c <- [0 .. 4]])
          (sort (interiorCoords @'[Clamped 5, Periodic 5])),
      testCase "every interior cell really has a full neighbourhood" $
        assertBool
          ""
          ( all
              ((== 8) . length . neighbours)
              (interiorCoords @'[Clamped 5, Clamped 5])
          ),
      -- The equivalence above is a fact about bounded axes, not about being
      -- interior, and this is where the two come apart: on a torus two
      -- cells wide, offsets -1 and +1 wrap onto the same cell, so every
      -- cell is interior and every cell has three neighbours rather than
      -- eight. Pinned here because 'interiorCoords' documents it.
      testCase "a torus narrower than its neighbourhood is still all interior" $ do
        let c = peOf @2 0 :| peOf @2 0 :| EmptyCoord
        assertBool "interior" (not (onBoundary c))
        assertEqual "neighbours" 3 (length (neighbours c))
        assertEqual
          "every cell"
          (allCoord @'[Periodic 2, Periodic 2])
          (interiorCoords @'[Periodic 2, Periodic 2])
    ]

-- | A grid traversal restricted to the interior needs no new API: 'Grid' is
-- already @TraversableWithIndex (Coord cs)@, so it is 'itraversed' filtered by
-- 'onBoundary'. Unlike 'interiorCoords' it reads /and/ writes, which is the
-- half an enumeration cannot give.
--
-- These tests exist because 'interiorCoords' recommends the composition in its
-- haddock, and a documented composition that nothing exercises is a claim, not
-- a fact.
interiorTraversalTests :: TestTree
interiorTraversalTests =
  testGroup
    "the interior of a Grid is itraversed filtered by onBoundary"
    [ testCase "it reaches exactly the interior cells" $
        assertEqual "" 9 (lengthOf interior ones),
      testCase "its indices are interiorCoords" $
        assertEqual
          ""
          (interiorCoords @'[Clamped 5, Clamped 5])
          (toListOf (itraversed . indices (not . onBoundary) . asIndex) ones),
      testCase "writing through it leaves the boundary alone" $ do
        let g = ones & interior .~ 9
        assertEqual "interior" 81 (sumOf interior g)
        assertEqual "boundary" 16 (sumOf boundary g),
      testCase "the boundary is the same optic with the predicate flipped" $
        assertEqual "" 16 (lengthOf boundary ones),
      testCase "isCorner picks out the four corners" $
        assertEqual "" 4 (lengthOf (itraversed . indices isCorner) ones),
      testCase "on a torus the interior is the whole grid" $
        assertEqual
          ""
          25
          ( lengthOf
              (itraversed . indices (not . onBoundary))
              ( tabulate (const (1 :: Int)) ::
                  Grid '[Periodic 5, Periodic 5] Int
              )
          )
    ]
  where
    ones :: Grid '[Clamped 5, Clamped 5] Int
    ones = tabulate (const 1)
    interior ::
      IndexedTraversal' (Coord '[Clamped 5, Clamped 5]) (Grid '[Clamped 5, Clamped 5] Int) Int
    interior = itraversed . indices (not . onBoundary)
    boundary ::
      IndexedTraversal' (Coord '[Clamped 5, Clamped 5]) (Grid '[Clamped 5, Clamped 5] Int) Int
    boundary = itraversed . indices onBoundary

boundaryTests :: TestTree
boundaryTests =
  testGroup
    "Boundary detection"
    [ axisBoundaryTests,
      axisBoundaryLawTests,
      axisBoundariesTests,
      onBoundaryTests,
      isCornerTests,
      fusedBoundaryTests,
      interiorTests,
      interiorTraversalTests
    ]
