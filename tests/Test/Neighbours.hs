{-# LANGUAGE AllowAmbiguousTypes #-}
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

import           Data.Grid.Sized
import           Test.Arbitrary        ()

import           Data.AdditiveGroup    (zeroV)
import           Data.AffineSpace      (Diff, (.+^), (.-.))
import           Data.List             (nub, sort)
import           Data.Maybe            (fromJust)
import           GHC.TypeLits          (KnownNat, type (<=))
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty, (===))

-- | The bounded space: off-grid is 'Nothing'.
hwOf :: KnownNat n => Int -> Clamped n
hwOf = Clamped . fromJust . numToOrdinal

-- | The torus: every offset succeeds.
peOf :: KnownNat n => Int -> Periodic n
peOf = Periodic . fromJust . numToOrdinal

hw :: Int -> Clamped 5
hw = hwOf

pe :: Int -> Periodic 5
pe = peOf

offsetIsCoordTests :: TestTree
offsetIsCoordTests =
    testGroup
        "offsetIsCoord is the checked counterpart of (.+^)"
        [ testCase "a zero offset is the identity" $ do
              assertEqual "Clamped" (Just (hw 2)) (offsetIsCoord (hw 2) 0)
              assertEqual "Periodic" (Just (pe 2)) (offsetIsCoord (pe 2) 0)
        , testCase "an in-range offset moves" $ do
              assertEqual "Clamped" (Just (hw 3)) (offsetIsCoord (hw 2) 1)
              assertEqual "Periodic" (Just (pe 3)) (offsetIsCoord (pe 2) 1)
        , testCase "Clamped fails off the low edge instead of clamping" $
              assertEqual "" Nothing (offsetIsCoord (hw 0) (-1))
        , testCase "Clamped fails off the high edge instead of clamping" $
              assertEqual "" Nothing (offsetIsCoord (hw 4) 1)
        , testCase "Periodic wraps at the low edge" $
              assertEqual "" (Just (pe 4)) (offsetIsCoord (pe 0) (-1))
        , testCase "Periodic wraps at the high edge" $
              assertEqual "" (Just (pe 0)) (offsetIsCoord (pe 4) 1)
        , -- This used to pass a displacement wider than an 'Int' and check that
          -- narrowing it did not fold it into range. The displacement is an
          -- 'Int' now, so that input no longer exists, and the extremes of the
          -- type are what is left to guard.
          --
          -- Unlike the 'Clamped' saturation cases in "Test.Ordinal", these do
          -- not discriminate between the current body and the @numToOrdinal
          -- (i + d)@ one it replaced: an overflowing sum comes back negative,
          -- and 'Nothing' is the right answer for a negative sum too. They are a
          -- regression guard on the answer, not a demonstration of the bug ---
          -- what the rewrite bought here was the 'Integer' conversion, not
          -- correctness.
          testCase "an extreme displacement is refused, not wrapped into range" $ do
              assertEqual "maxBound" Nothing (offsetIsCoord (hw 4) maxBound)
              assertEqual "minBound" Nothing (offsetIsCoord (hw 0) minBound)
        , testCase "Periodic reduces a huge displacement" $
              assertEqual
                  ""
                  (Just (pe 1))
                  (offsetIsCoord (pe 1) (5 ^ (20 :: Int)))
        ]

hwc :: Int -> Int -> Coord '[Clamped 5, Clamped 5]
hwc r c = hw r :| hw c :| EmptyCoord

pec :: Int -> Int -> Coord '[Periodic 5, Periodic 5]
pec r c = pe r :| pe c :| EmptyCoord

-- | One bounded axis and one torus axis, to pin down that the policy is read
-- per axis rather than once for the whole coordinate.
mixc :: Int -> Int -> Coord '[Clamped 5, Periodic 5]
mixc r c = hw r :| pe c :| EmptyCoord

-- | A two-dimensional displacement. This is the shape every @Coord '[_, _]@
-- above takes, because 'Diff' of a coord is a coord of the axes' 'Diff's and
-- both 'Clamped' and 'Periodic' have @Diff ~ Int@.
d2 :: Int -> Int -> Coord '[Int, Int]
d2 a b = a :| b :| EmptyCoord

offsetCoordTests :: TestTree
offsetCoordTests =
    testGroup
        "offsetCoord applies each axis's own boundary policy"
        [ testCase "a zero offset is the identity" $ do
              assertEqual "Clamped" (Just (hwc 2 2)) (offsetCoord (hwc 2 2) (d2 0 0))
              assertEqual "Periodic" (Just (pec 2 2)) (offsetCoord (pec 2 2) (d2 0 0))
        , testCase "an in-range offset moves on every axis" $
              assertEqual "" (Just (hwc 3 3)) (offsetCoord (hwc 2 2) (d2 1 1))
        , testCase "failing on the first axis fails the whole offset" $
              assertEqual "" Nothing (offsetCoord (hwc 0 0) (d2 (-1) 0))
        , testCase "failing on the second axis fails the whole offset" $
              assertEqual "" Nothing (offsetCoord (hwc 0 0) (d2 0 (-1)))
        , testCase "off the high corner fails" $
              assertEqual "" Nothing (offsetCoord (hwc 4 4) (d2 1 1))
        , testCase "a torus wraps on every axis" $
              assertEqual "" (Just (pec 4 4)) (offsetCoord (pec 0 0) (d2 (-1) (-1)))
        , testCase "a torus axis wraps while a bounded axis stands still" $
              assertEqual "" (Just (mixc 0 4)) (offsetCoord (mixc 0 0) (d2 0 (-1)))
        , testCase "a bounded axis fails while a torus axis would have wrapped" $
              assertEqual "" Nothing (offsetCoord (mixc 0 0) (d2 (-1) (-1)))
        ]

mooreTests :: TestTree
mooreTests =
    testGroup
        "mooreNeighbours"
        [ testCase "the centre is not its own neighbour" $ do
              assertBool "Clamped" (hwc 2 2 `notElem` neighbours (hwc 2 2))
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
          -- corner of a Clamped 5 x Clamped 5 returned nine results of which
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
        , testProperty "never contains duplicates" $ \(c :: Coord '[Clamped 5, Clamped 5]) ->
              length (nub (neighbours c)) === length (neighbours c)
        , testProperty "a torus neighbourhood has no duplicates either" $ \(c :: Coord '[Periodic 5, Periodic 5]) ->
              length (nub (neighbours c)) === length (neighbours c)
        , testProperty "never contains the centre" $ \(c :: Coord '[Clamped 5, Clamped 5]) ->
              c `notElem` neighbours c
        , testProperty "is symmetric on a bounded grid" $ \(c :: Coord '[Clamped 5, Clamped 5]) ->
              all (\c' -> c `elem` neighbours c') (neighbours c)
        , testProperty "is symmetric on a torus" $ \(c :: Coord '[Periodic 5, Periodic 5]) ->
              all (\c' -> c `elem` neighbours c') (neighbours c)
        ]

-- | Three dimensions, to keep the tests honest about von Neumann counts: a
-- radius-1 von Neumann neighbourhood has four cells in 2D but six in 3D, which
-- is exactly why the function is not called @neighbours4@.
hwc3 :: Int -> Int -> Int -> Coord '[Clamped 5, Clamped 5, Clamped 5]
hwc3 x y z = hw x :| hw y :| hw z :| EmptyCoord

-- | A torus small enough that offsets -2 and +2 land on the same cell.
pec4 :: Int -> Int -> Coord '[Periodic 4, Periodic 4]
pec4 r c = peOf r :| peOf c :| EmptyCoord

-- | A 3-cycle: every other cell is one step away whichever way you walk.
pec3 :: Int -> Int -> Coord '[Periodic 3, Periodic 3]
pec3 r c = peOf r :| peOf c :| EmptyCoord

vonNeumannTests :: TestTree
vonNeumannTests =
    testGroup
        "vonNeumannNeighbours"
        [ testCase "radius 1 in 2D is the four orthogonal cells" $
              assertEqual
                  ""
                  [hwc 1 2, hwc 2 1, hwc 2 3, hwc 3 2]
                  (vonNeumannNeighbours 1 (hwc 2 2))
        , testCase "radius 1 in 3D is six cells, not four" $
              assertEqual "" 6 (length (vonNeumannNeighbours 1 (hwc3 2 2 2)))
        , testCase "a bounded corner has only the two cells that exist" $
              assertEqual "" [hwc 0 1, hwc 1 0] (vonNeumannNeighbours 1 (hwc 0 0))
        , testCase "radius 2 in 2D is the twelve cells within manhattan 2" $
              assertEqual "" 12 (length (vonNeumannNeighbours 2 (hwc 2 2)))
        , testCase "radius 0 is empty" $
              assertEqual "" [] (vonNeumannNeighbours 0 (hwc 2 2))
        , testProperty "is a subset of the Moore neighbourhood" $ \(c :: Coord '[Clamped 5, Clamped 5]) ->
              all (`elem` mooreNeighbours 2 c) (vonNeumannNeighbours 2 c)
        , testProperty "is a subset of the Moore neighbourhood on a torus" $ \(c :: Coord '[Periodic 5, Periodic 5]) ->
              all (`elem` mooreNeighbours 2 c) (vonNeumannNeighbours 2 c)
        , -- On a 3-cycle every other cell is one step away whichever way you
          -- walk, so a radius-2 von Neumann ball is the entire grid.
          testCase "a small torus counts the shorter way round" $
              assertEqual
                  ""
                  8
                  (length (vonNeumannNeighbours 2 (pec3 0 0)))
        , -- Offsets -2 and +2 reach the same cell on a Periodic 4 axis. If the
          -- axis enumeration kept both, the product would contain repeats.
          testCase "colliding offsets on a torus do not duplicate cells" $ do
              let ns = mooreNeighbours 2 (pec4 0 0)
              assertEqual "every other cell, once" 15 (length ns)
              assertEqual "all distinct" 15 (length (nub ns))
        ]

-- | 'Ordinal' has no 'Data.AffineSpace.AffineSpace' instance, so the old
-- @moorePoints@ could not be called on a coord containing one at all: its
-- @All AffineSpace cs@ was unsatisfiable. These ask only for
-- @IsCoordList cs@, so they can. The ChangeLog claims this; this is the
-- evidence.
ordinalTests :: TestTree
ordinalTests =
    testGroup
        "Ordinal axes are supported, which moorePoints could not be"
        [ testCase "neighbours of an Ordinal corner" $
              assertEqual
                  ""
                  [ord 0 1, ord 1 0, ord 1 1]
                  (neighbours (ord 0 0))
        , testCase "neighbours of an Ordinal interior cell" $
              assertEqual "" 8 (length (neighbours (ord 2 2)))
        , testCase "an Ordinal axis is bounded, so it fails at the edge" $
              assertEqual
                  ""
                  Nothing
                  (offsetIsCoord
                       (fromJust (numToOrdinal (0 :: Int)) :: Ordinal 5)
                       (-1))
        ]
  where
    ord :: Int -> Int -> Coord '[Ordinal 5, Ordinal 5]
    ord r c =
        fromJust (numToOrdinal r) :| fromJust (numToOrdinal c) :| EmptyCoord

-- | Tests for the exported distances (@sized-grid-lcl@).
--
-- The library computed these all along --- 'axisSteps' works out the true
-- per-axis distance and 'stepsWithin' sums it --- and then both neighbourhood
-- functions discarded the number and nothing exported it. The first group below
-- is the one that matters: 'axisDistance' is a second, independent
-- implementation of the "shorter route wins" rule, so it has to be pinned to the
-- enumeration rather than trusted to agree with it.
axisDistanceTests :: TestTree
axisDistanceTests =
    testGroup
        "axisDistance agrees with the axisSteps enumeration"
        [ -- The law in the haddock for 'axisDistanceIsCoord': the distance is
          -- the least @abs d@ for which @offsetIsCoord a d == Just b@, which is
          -- exactly what 'axisSteps' works out by trying every offset.
          --
          -- Checked exhaustively rather than by QuickCheck: these axes have five
          -- inhabitants, so every start value and every reachable target fits in
          -- a list comprehension, which is both cheaper and stronger than
          -- sampling. It also covers 'Ordinal', which has no 'Arbitrary'
          -- instance and so cannot be reached by a property at all.
          testCase "every step axisSteps records is the distance" $ do
              assertBool "Clamped 5" (agreesEverywhere @Clamped @5 5)
              assertBool "Periodic 5" (agreesEverywhere @Periodic @5 5)
              assertBool "Ordinal 5" (agreesEverywhere @Ordinal @5 5)
              -- A 4-cycle is where offsets -2 and +2 collide, and a 3-cycle is
              -- where every other cell is one step away. Both are the cases the
              -- naive @abs (i - j)@ gets wrong.
              assertBool "Periodic 4" (agreesEverywhere @Periodic @4 4)
              assertBool "Periodic 3" (agreesEverywhere @Periodic @3 3)
        , testCase "a bounded axis measures straight" $ do
              assertEqual "0 to 4" 4 (axisDistance (hw 0) (hw 4))
              assertEqual "1 to 3" 2 (axisDistance (hw 1) (hw 3))
        , testCase "a torus axis takes the shorter way round" $ do
              assertEqual "0 to 4 is one step back" 1 (axisDistance (pe 0) (pe 4))
              assertEqual "0 to 3 is two steps back" 2 (axisDistance (pe 0) (pe 3))
              assertEqual "0 to 2 is two steps forward" 2 (axisDistance (pe 0) (pe 2))
        , testCase "on a 3-cycle every other cell is one step away" $ do
              assertEqual "0 to 1" 1 (axisDistance (peOf 0 :: Periodic 3) (peOf 1))
              assertEqual "0 to 2" 1 (axisDistance (peOf 0 :: Periodic 3) (peOf 2))
        , testProperty "a distance is zero only from a value to itself" $ \(c :: Clamped 5) ->
              axisDistance c c === 0
        , testProperty "is symmetric on a torus" $ \(a :: Periodic 5) b ->
              axisDistance a b === axisDistance b a
        ]
  where
    -- Every value of the axis, against every target 'axisSteps' can reach from
    -- it within @r@.
    agreesEverywhere ::
           forall c n. (IsCoord c, KnownNat n, 1 <= n) => Int -> Bool
    agreesEverywhere r =
        and [ axisDistance c v == d
            | c <- allCoordLike @n @c
            , (d, v) <- axisSteps r c
            ]

-- | The two exported metrics are exactly the balls the two neighbourhood
-- functions already enumerate. This is the strongest evidence available that
-- the distances are right, because the neighbourhoods are covered by the tests
-- above and were reviewed when they landed.
metricTests :: TestTree
metricTests =
    testGroup
        "coordDistance and coordManhattan are the neighbourhood metrics"
        [ testProperty "mooreNeighbours r is the coordDistance ball" $ \(c :: Coord '[Clamped 5, Clamped 5]) ->
              ballAgrees coordDistance mooreNeighbours 2 c
        , testProperty "on a torus too" $ \(c :: Coord '[Periodic 5, Periodic 5]) ->
              ballAgrees coordDistance mooreNeighbours 2 c
        , testProperty "and on mixed axes" $ \(c :: Coord '[Clamped 5, Periodic 5]) ->
              ballAgrees coordDistance mooreNeighbours 2 c
        , testProperty "vonNeumannNeighbours r is the coordManhattan ball" $ \(c :: Coord '[Clamped 5, Clamped 5]) ->
              ballAgrees coordManhattan vonNeumannNeighbours 2 c
        , testProperty "on a torus too" $ \(c :: Coord '[Periodic 5, Periodic 5]) ->
              ballAgrees coordManhattan vonNeumannNeighbours 2 c
        , testProperty "and on mixed axes" $ \(c :: Coord '[Clamped 5, Periodic 5]) ->
              ballAgrees coordManhattan vonNeumannNeighbours 2 c
        ]
  where
    -- @f r c@ is every coord whose distance from @c@ is in @[1, r]@.
    ballAgrees ::
           (All Eq cs, All Ord cs, IsCoordList cs)
        => (Coord cs -> Coord cs -> Int)
        -> (Int -> Coord cs -> [Coord cs])
        -> Int
        -> Coord cs
        -> Bool
    ballAgrees dist f r c =
        sort (f r c) ==
        sort [c' | c' <- allCoord, let d = dist c c', d > 0, d <= r]

-- | The mixed-axis case from the acceptance criteria on @sized-grid-lcl@: the
-- bounded axis measures straight while the torus axis takes the shorter way
-- round, in the same coordinate. This is the answer a caller cannot easily
-- write by hand, and the reason the distance is worth exporting at all.
mixedPolicyTests :: TestTree
mixedPolicyTests =
    testGroup
        "each axis applies its own boundary policy"
        [ testCase "the torus axis wraps, the bounded axis does not" $ do
              -- Row 0 -> 4 on a Clamped axis is 4 steps; column 0 -> 4 on a
              -- Periodic axis is 1 step back across the seam.
              assertEqual "per axis" [4, 1] (axisDistances (mixc 0 0) (mixc 4 4))
              assertEqual "chebyshev" 4 (coordDistance (mixc 0 0) (mixc 4 4))
              assertEqual "manhattan" 5 (coordManhattan (mixc 0 0) (mixc 4 4))
        , testCase "the all-bounded coord measures straight on both axes" $ do
              assertEqual "chebyshev" 4 (coordDistance (hwc 0 0) (hwc 4 4))
              assertEqual "manhattan" 8 (coordManhattan (hwc 0 0) (hwc 4 4))
        , testCase "the all-torus coord wraps on both axes" $ do
              assertEqual "chebyshev" 1 (coordDistance (pec 0 0) (pec 4 4))
              assertEqual "manhattan" 2 (coordManhattan (pec 0 0) (pec 4 4))
        , testCase "a diagonal is one Chebyshev step and two Manhattan steps" $ do
              assertEqual "chebyshev" 1 (coordDistance (hwc 2 2) (hwc 3 3))
              assertEqual "manhattan" 2 (coordManhattan (hwc 2 2) (hwc 3 3))
        ]

-- | Both exported distances are metrics. Manhattan and Chebyshev are the sum
-- and the maximum of the per-axis distances, and each axis metric is itself a
-- metric --- @abs (i - j)@ on a line, @min d (n - d)@ on a cycle --- so both
-- inherit the laws. Worth stating because the torus case is where a
-- hand-rolled distance stops satisfying them.
metricLawTests :: TestTree
metricLawTests =
    testGroup
        "both distances are metrics"
        [ testProperty "identity: zero only to itself (Chebyshev)" $ \(c :: Coord '[Clamped 5, Periodic 5]) ->
              coordDistance c c === 0
        , testProperty "identity: zero only to itself (Manhattan)" $ \(c :: Coord '[Clamped 5, Periodic 5]) ->
              coordManhattan c c === 0
        , testProperty "zero distance means equal" $ \(a :: Coord '[Clamped 5, Periodic 5]) b ->
              (coordManhattan a b == 0) === (a == b)
        , testProperty "symmetry (Chebyshev)" $ \(a :: Coord '[Clamped 5, Periodic 5]) b ->
              coordDistance a b === coordDistance b a
        , testProperty "symmetry (Manhattan)" $ \(a :: Coord '[Clamped 5, Periodic 5]) b ->
              coordManhattan a b === coordManhattan b a
        , testProperty "triangle inequality (Chebyshev)" $ \(a :: Coord '[Clamped 5, Periodic 5]) b c ->
              coordDistance a c <= coordDistance a b + coordDistance b c
        , testProperty "triangle inequality (Manhattan)" $ \(a :: Coord '[Clamped 5, Periodic 5]) b c ->
              coordManhattan a c <= coordManhattan a b + coordManhattan b c
        , testProperty "Chebyshev never exceeds Manhattan" $ \(a :: Coord '[Clamped 5, Periodic 5]) b ->
              coordDistance a b <= coordManhattan a b
        , -- A step to an adjacent cell is one in both metrics only when it is
          -- along a single axis; the point of having both is that a diagonal
          -- costs one and two respectively.
          testProperty "every Moore neighbour is one Chebyshev step away" $ \(c :: Coord '[Clamped 5, Periodic 5]) ->
              all (\n -> coordDistance c n == 1) (neighbours c)
        , testProperty "every von Neumann neighbour is one Manhattan step away" $ \(c :: Coord '[Clamped 5, Periodic 5]) ->
              all (\n -> coordManhattan c n == 1) (vonNeumannNeighbours 1 c)
        ]

-- | 'stepsWithin' is now exported too: it is the general primitive both
-- neighbourhood functions are one-liners over, and the one a BFS-shaped
-- consumer wants because it carries the distances.
stepsWithinTests :: TestTree
stepsWithinTests =
    testGroup
        "stepsWithin is exported and carries the distances"
        [ testCase "the centre is the only entry at distance zero" $
              assertEqual
                  ""
                  [(0, hwc 2 2)]
                  [e | e@(s, _) <- stepsWithin 2 (hwc 2 2), s == 0]
        , testProperty "the recorded total is the Manhattan distance" $ \(c :: Coord '[Clamped 5, Periodic 5]) ->
              all (\(s, n) -> coordManhattan c n == s) (stepsWithin 2 c)
        , testProperty "dropping the centre gives the Moore neighbourhood" $ \(c :: Coord '[Clamped 5, Periodic 5]) ->
              [n | (s, n) <- stepsWithin 2 c, s > 0] === mooreNeighbours 2 c
        ]

-- | Seven axes: one past the ceiling the old @CoordDiff@ family imposed.
--
-- @CoordDiff@ was an open family with one hand-written @type instance@ per
-- arity, and it stopped at six. A seven-axis coord therefore had no @Diff@ at
-- all, so no 'AffineSpace' instance and no 'offsetCoord'; the only fix
-- available to a caller was an orphan instance plus a seven-tuple. 'MapDiff'
-- recurses, so this now works for the same reason two axes do, and no arity is
-- special (@sized-grid-iet@).
type Seven
     = '[ Clamped 3, Clamped 3, Clamped 3, Clamped 3, Clamped 3, Clamped 3, Clamped 3]

sevenOf :: Int -> Coord Seven
sevenOf n =
    let c = hwOf n
    in c :| c :| c :| c :| c :| c :| c :| EmptyCoord

-- | The displacement for 'Seven': a coord again, of the axes' 'Diff's.
sevenD :: Int -> Diff (Coord Seven)
sevenD d = d :| d :| d :| d :| d :| d :| d :| EmptyCoord

arityTests :: TestTree
arityTests =
    testGroup
        "a coord past the old six-axis ceiling has a working offset API"
        [ testCase "(.+^) moves every axis" $
              assertEqual "" (sevenOf 2) (sevenOf 1 .+^ sevenD 1)
        , testCase "(.-.) is the displacement that carries you back" $
              assertEqual
                  ""
                  (sevenOf 2)
                  (sevenOf 1 .+^ (sevenOf 2 .-. sevenOf 1))
        , testCase "(.-.) reads off as a coord of Diffs" $
              assertEqual "" (sevenD 1) (sevenOf 2 .-. sevenOf 1)
        , testCase "offsetCoord succeeds inside the grid" $
              assertEqual "" (Just (sevenOf 2)) (offsetCoord (sevenOf 1) (sevenD 1))
        , testCase "offsetCoord reports leaving the grid" $
              assertEqual "" Nothing (offsetCoord (sevenOf 0) (sevenD (-1)))
        , testCase "the zero displacement is the identity" $
              assertEqual "" (sevenOf 1) (sevenOf 1 .+^ zeroV)
        ]

-- | The tuple bridge kept for call sites that used to write a tuple literal
-- directly. It is arity-generic, so the same function covers a pair and a
-- seven-axis coord.
tupleBridgeTests :: TestTree
tupleBridgeTests =
    testGroup
        "coordFromTuple / coordToTuple"
        [ testCase "coordFromTuple builds the same coord as (:|)" $
              assertEqual "" (d2 1 (-2)) (coordFromTuple (1, -2))
        , testCase "coordToTuple takes one apart" $
              assertEqual "" (1, -2 :: Int) (coordToTuple (d2 1 (-2)))
        , testCase "a tuple offsets a coord through (.+^)" $
              assertEqual
                  ""
                  (hwc 3 3)
                  (hwc 2 2 .+^ coordFromTuple (1, 1))
        , testCase "seven axes go through the same function" $
              assertEqual
                  ""
                  (sevenD 1)
                  (coordFromTuple (1, 1, 1, 1, 1, 1, 1))
        ]

neighbourTests :: TestTree
neighbourTests =
    testGroup
        "Neighbours"
        [ offsetIsCoordTests
        , offsetCoordTests
        , arityTests
        , tupleBridgeTests
        , mooreTests
        , vonNeumannTests
        , ordinalTests
        , axisDistanceTests
        , metricTests
        , mixedPolicyTests
        , metricLawTests
        , stepsWithinTests
        ]
