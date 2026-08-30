{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Tests for the checked offset and the neighbourhoods built on it.
module Test.Neighbours
  ( neighbourTests,
  )
where

import Data.AdditiveGroup (zeroV)
import Data.AffineSpace (Diff, (.+^), (.-.))
import Data.Grid.Sized
import Data.List (nub, sort)
import Data.Maybe (fromJust)
import GHC.TypeLits (KnownNat, type (<=))
import Test.Arbitrary ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty, (===))

-- | The bounded space: off-grid is 'Nothing'.
hwOf :: (KnownNat n) => Int -> Clamped n
hwOf = Clamped . fromJust . numToOrdinal

-- | The torus: every offset succeeds.
peOf :: (KnownNat n) => Int -> Periodic n
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
        assertEqual "Periodic" (Just (pe 2)) (offsetIsCoord (pe 2) 0),
      testCase "an in-range offset moves" $ do
        assertEqual "Clamped" (Just (hw 3)) (offsetIsCoord (hw 2) 1)
        assertEqual "Periodic" (Just (pe 3)) (offsetIsCoord (pe 2) 1),
      testCase "Clamped fails off the low edge instead of clamping" $
        assertEqual "" Nothing (offsetIsCoord (hw 0) (-1)),
      testCase "Clamped fails off the high edge instead of clamping" $
        assertEqual "" Nothing (offsetIsCoord (hw 4) 1),
      testCase "Periodic wraps at the low edge" $
        assertEqual "" (Just (pe 4)) (offsetIsCoord (pe 0) (-1)),
      testCase "Periodic wraps at the high edge" $
        assertEqual "" (Just (pe 0)) (offsetIsCoord (pe 4) 1),
      testCase "an extreme displacement is refused, not wrapped into range" $ do
        assertEqual "maxBound" Nothing (offsetIsCoord (hw 4) maxBound)
        assertEqual "minBound" Nothing (offsetIsCoord (hw 0) minBound),
      testCase "Periodic reduces a huge displacement" $
        assertEqual
          ""
          (Just (pe 1))
          (offsetIsCoord (pe 1) (5 ^ (20 :: Int)))
    ]

axisOffsetTests :: TestTree
axisOffsetTests =
  testGroup
    "axisOffset is the lifted wrapper over offsetIsCoord"
    [ -- Checked exhaustively rather than by QuickCheck: also covers
      -- 'Ordinal', which has no 'Arbitrary' instance.
      testCase "agrees with offsetIsCoord for every value and step" $ do
        assertBool "Clamped 5" (agreesEverywhere @Clamped @5 5)
        assertBool "Periodic 5" (agreesEverywhere @Periodic @5 5)
        assertBool "Ordinal 5" (agreesEverywhere @Ordinal @5 5),
      testCase "a bounded axis refuses a step off the edge" $ do
        assertEqual "low" Nothing (axisOffset (hw 0) (-1))
        assertEqual "high" Nothing (axisOffset (hw 4) 1),
      testCase "a torus axis wraps" $
        assertEqual "" (Just (pe 0)) (axisOffset (pe 4) 1)
    ]
  where
    agreesEverywhere ::
      forall c n. (IsCoord c, Eq (c n), KnownNat n, 1 <= n) => Int -> Bool
    agreesEverywhere r =
      and
        [ axisOffset c s == offsetIsCoord c s
        | c <- allCoordLike @n @c,
          s <- [-r .. r]
        ]

hwc :: Int -> Int -> Coord '[Clamped 5, Clamped 5]
hwc r c = hw r :| hw c :| EmptyCoord

pec :: Int -> Int -> Coord '[Periodic 5, Periodic 5]
pec r c = pe r :| pe c :| EmptyCoord

-- | One bounded axis and one torus axis: the policy is read per axis.
mixc :: Int -> Int -> Coord '[Clamped 5, Periodic 5]
mixc r c = hw r :| pe c :| EmptyCoord

d2 :: Int -> Int -> Delta '[Int, Int]
d2 a b = a :^ b :^ NoDelta

offsetCoordTests :: TestTree
offsetCoordTests =
  testGroup
    "offsetCoord applies each axis's own boundary policy"
    [ testCase "a zero offset is the identity" $ do
        assertEqual "Clamped" (Just (hwc 2 2)) (offsetCoord (hwc 2 2) (d2 0 0))
        assertEqual "Periodic" (Just (pec 2 2)) (offsetCoord (pec 2 2) (d2 0 0)),
      testCase "an in-range offset moves on every axis" $
        assertEqual "" (Just (hwc 3 3)) (offsetCoord (hwc 2 2) (d2 1 1)),
      testCase "failing on the first axis fails the whole offset" $
        assertEqual "" Nothing (offsetCoord (hwc 0 0) (d2 (-1) 0)),
      testCase "failing on the second axis fails the whole offset" $
        assertEqual "" Nothing (offsetCoord (hwc 0 0) (d2 0 (-1))),
      testCase "off the high corner fails" $
        assertEqual "" Nothing (offsetCoord (hwc 4 4) (d2 1 1)),
      testCase "a torus wraps on every axis" $
        assertEqual "" (Just (pec 4 4)) (offsetCoord (pec 0 0) (d2 (-1) (-1))),
      testCase "a torus axis wraps while a bounded axis stands still" $
        assertEqual "" (Just (mixc 0 4)) (offsetCoord (mixc 0 0) (d2 0 (-1))),
      testCase "a bounded axis fails while a torus axis would have wrapped" $
        assertEqual "" Nothing (offsetCoord (mixc 0 0) (d2 (-1) (-1)))
    ]

mooreTests :: TestTree
mooreTests =
  testGroup
    "mooreNeighbours"
    [ testCase "the centre is not its own neighbour" $ do
        assertBool "Clamped" (hwc 2 2 `notElem` neighbours (hwc 2 2))
        assertBool "Periodic" (pec 2 2 `notElem` neighbours (pec 2 2)),
      testCase "a bounded grid has 3 neighbours at a corner" $
        assertEqual "" 3 (length (neighbours (hwc 0 0))),
      testCase "a bounded grid has 5 neighbours on an edge" $
        assertEqual "" 5 (length (neighbours (hwc 0 2))),
      testCase "a bounded grid has 8 neighbours in the interior" $
        assertEqual "" 8 (length (neighbours (hwc 2 2))),
      testCase "a torus has 8 neighbours everywhere, corners included" $ do
        assertEqual "corner" 8 (length (neighbours (pec 0 0)))
        assertEqual "interior" 8 (length (neighbours (pec 2 2))),
      testCase "regression: a corner yields no clamped duplicates" $ do
        let ns = neighbours (hwc 0 0)
        assertEqual "count" 3 (length ns)
        assertEqual "all distinct" 3 (length (nub ns))
        assertEqual
          "the three cells that exist"
          [hwc 0 1, hwc 1 0, hwc 1 1]
          ns,
      testCase "results are in row-major order, last axis fastest" $
        assertEqual
          ""
          [ hwc 1 1,
            hwc 1 2,
            hwc 1 3,
            hwc 2 1,
            hwc 2 3,
            hwc 3 1,
            hwc 3 2,
            hwc 3 3
          ]
          (neighbours (hwc 2 2)),
      testCase "a torus axis is ordered by offset, not by value" $
        assertEqual
          ""
          [ pec 4 4,
            pec 4 0,
            pec 4 1,
            pec 0 4,
            pec 0 1,
            pec 1 4,
            pec 1 0,
            pec 1 1
          ]
          (neighbours (pec 0 0)),
      testCase "radius 2 reaches further" $
        assertEqual "" 24 (length (mooreNeighbours 2 (hwc 2 2))),
      testCase "radius 0 is empty" $
        assertEqual "" [] (mooreNeighbours 0 (hwc 2 2)),
      testProperty "never contains duplicates" $ \(c :: Coord '[Clamped 5, Clamped 5]) ->
        length (nub (neighbours c)) === length (neighbours c),
      testProperty "a torus neighbourhood has no duplicates either" $ \(c :: Coord '[Periodic 5, Periodic 5]) ->
        length (nub (neighbours c)) === length (neighbours c),
      testProperty "never contains the centre" $ \(c :: Coord '[Clamped 5, Clamped 5]) ->
        c `notElem` neighbours c,
      testProperty "is symmetric on a bounded grid" $ \(c :: Coord '[Clamped 5, Clamped 5]) ->
        all (\c' -> c `elem` neighbours c') (neighbours c),
      testProperty "is symmetric on a torus" $ \(c :: Coord '[Periodic 5, Periodic 5]) ->
        all (\c' -> c `elem` neighbours c') (neighbours c)
    ]

-- | A radius-1 von Neumann neighbourhood has four cells in 2D but six in 3D.
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
          (vonNeumannNeighbours 1 (hwc 2 2)),
      testCase "radius 1 in 3D is six cells, not four" $
        assertEqual "" 6 (length (vonNeumannNeighbours 1 (hwc3 2 2 2))),
      testCase "a bounded corner has only the two cells that exist" $
        assertEqual "" [hwc 0 1, hwc 1 0] (vonNeumannNeighbours 1 (hwc 0 0)),
      testCase "radius 2 in 2D is the twelve cells within manhattan 2" $
        assertEqual "" 12 (length (vonNeumannNeighbours 2 (hwc 2 2))),
      testCase "radius 0 is empty" $
        assertEqual "" [] (vonNeumannNeighbours 0 (hwc 2 2)),
      testProperty "is a subset of the Moore neighbourhood" $ \(c :: Coord '[Clamped 5, Clamped 5]) ->
        all (`elem` mooreNeighbours 2 c) (vonNeumannNeighbours 2 c),
      testProperty "is a subset of the Moore neighbourhood on a torus" $ \(c :: Coord '[Periodic 5, Periodic 5]) ->
        all (`elem` mooreNeighbours 2 c) (vonNeumannNeighbours 2 c),
      testCase "a small torus counts the shorter way round" $
        assertEqual
          ""
          8
          (length (vonNeumannNeighbours 2 (pec3 0 0))),
      testCase "colliding offsets on a torus do not duplicate cells" $ do
        let ns = mooreNeighbours 2 (pec4 0 0)
        assertEqual "every other cell, once" 15 (length ns)
        assertEqual "all distinct" 15 (length (nub ns))
    ]

ordinalTests :: TestTree
ordinalTests =
  testGroup
    "Ordinal axes are supported, which moorePoints could not be"
    [ testCase "neighbours of an Ordinal corner" $
        assertEqual
          ""
          [ord 0 1, ord 1 0, ord 1 1]
          (neighbours (ord 0 0)),
      testCase "neighbours of an Ordinal interior cell" $
        assertEqual "" 8 (length (neighbours (ord 2 2))),
      testCase "an Ordinal axis is bounded, so it fails at the edge" $
        assertEqual
          ""
          Nothing
          ( offsetIsCoord
              (fromJust (numToOrdinal (0 :: Int)) :: Ordinal 5)
              (-1)
          )
    ]
  where
    ord :: Int -> Int -> Coord '[Ordinal 5, Ordinal 5]
    ord r c =
      fromJust (numToOrdinal r) :| fromJust (numToOrdinal c) :| EmptyCoord

axisDistanceTests :: TestTree
axisDistanceTests =
  testGroup
    "axisDistance agrees with the axisSteps enumeration"
    [ -- Checked exhaustively rather than by QuickCheck: also covers
      -- 'Ordinal', which has no 'Arbitrary' instance.
      testCase "every step axisSteps records is the distance" $ do
        assertBool "Clamped 5" (agreesEverywhere @Clamped @5 5)
        assertBool "Periodic 5" (agreesEverywhere @Periodic @5 5)
        assertBool "Ordinal 5" (agreesEverywhere @Ordinal @5 5)
        assertBool "Periodic 4" (agreesEverywhere @Periodic @4 4)
        assertBool "Periodic 3" (agreesEverywhere @Periodic @3 3),
      testCase "a bounded axis measures straight" $ do
        assertEqual "0 to 4" 4 (axisDistance (hw 0) (hw 4))
        assertEqual "1 to 3" 2 (axisDistance (hw 1) (hw 3)),
      testCase "a torus axis takes the shorter way round" $ do
        assertEqual "0 to 4 is one step back" 1 (axisDistance (pe 0) (pe 4))
        assertEqual "0 to 3 is two steps back" 2 (axisDistance (pe 0) (pe 3))
        assertEqual "0 to 2 is two steps forward" 2 (axisDistance (pe 0) (pe 2)),
      testCase "on a 3-cycle every other cell is one step away" $ do
        assertEqual "0 to 1" 1 (axisDistance (peOf 0 :: Periodic 3) (peOf 1))
        assertEqual "0 to 2" 1 (axisDistance (peOf 0 :: Periodic 3) (peOf 2)),
      testProperty "a distance is zero only from a value to itself" $ \(c :: Clamped 5) ->
        axisDistance c c === 0,
      testProperty "is symmetric on a torus" $ \(a :: Periodic 5) b ->
        axisDistance a b === axisDistance b a
    ]
  where
    agreesEverywhere ::
      forall c n. (IsCoord c, KnownNat n, 1 <= n) => Int -> Bool
    agreesEverywhere r =
      and
        [ axisDistance c v == d
        | c <- allCoordLike @n @c,
          (d, v) <- axisSteps r c
        ]

metricTests :: TestTree
metricTests =
  testGroup
    "coordDistance and coordManhattan are the neighbourhood metrics"
    [ testProperty "mooreNeighbours r is the coordDistance ball" $ \(c :: Coord '[Clamped 5, Clamped 5]) ->
        ballAgrees coordDistance mooreNeighbours 2 c,
      testProperty "on a torus too" $ \(c :: Coord '[Periodic 5, Periodic 5]) ->
        ballAgrees coordDistance mooreNeighbours 2 c,
      testProperty "and on mixed axes" $ \(c :: Coord '[Clamped 5, Periodic 5]) ->
        ballAgrees coordDistance mooreNeighbours 2 c,
      testProperty "vonNeumannNeighbours r is the coordManhattan ball" $ \(c :: Coord '[Clamped 5, Clamped 5]) ->
        ballAgrees coordManhattan vonNeumannNeighbours 2 c,
      testProperty "on a torus too" $ \(c :: Coord '[Periodic 5, Periodic 5]) ->
        ballAgrees coordManhattan vonNeumannNeighbours 2 c,
      testProperty "and on mixed axes" $ \(c :: Coord '[Clamped 5, Periodic 5]) ->
        ballAgrees coordManhattan vonNeumannNeighbours 2 c
    ]
  where
    -- @f r c@ is every coord whose distance from @c@ is in @[1, r]@.
    ballAgrees ::
      (IsCoordList cs) =>
      (Coord cs -> Coord cs -> Int) ->
      (Int -> Coord cs -> [Coord cs]) ->
      Int ->
      Coord cs ->
      Bool
    ballAgrees dist f r c =
      sort (f r c)
        == sort [c' | c' <- allCoord, let d = dist c c', d > 0, d <= r]

mixedPolicyTests :: TestTree
mixedPolicyTests =
  testGroup
    "each axis applies its own boundary policy"
    [ testCase "the torus axis wraps, the bounded axis does not" $ do
        -- Row 0 -> 4 on Clamped is 4 steps; column 0 -> 4 on Periodic is
        -- 1 step back across the seam.
        assertEqual "per axis" [4, 1] (axisDistances (mixc 0 0) (mixc 4 4))
        assertEqual "chebyshev" 4 (coordDistance (mixc 0 0) (mixc 4 4))
        assertEqual "manhattan" 5 (coordManhattan (mixc 0 0) (mixc 4 4)),
      testCase "the all-bounded coord measures straight on both axes" $ do
        assertEqual "chebyshev" 4 (coordDistance (hwc 0 0) (hwc 4 4))
        assertEqual "manhattan" 8 (coordManhattan (hwc 0 0) (hwc 4 4)),
      testCase "the all-torus coord wraps on both axes" $ do
        assertEqual "chebyshev" 1 (coordDistance (pec 0 0) (pec 4 4))
        assertEqual "manhattan" 2 (coordManhattan (pec 0 0) (pec 4 4)),
      testCase "a diagonal is one Chebyshev step and two Manhattan steps" $ do
        assertEqual "chebyshev" 1 (coordDistance (hwc 2 2) (hwc 3 3))
        assertEqual "manhattan" 2 (coordManhattan (hwc 2 2) (hwc 3 3))
    ]

-- | 'coordDistance' and 'coordManhattan' fold the axes directly through
-- 'npMaxDistance'\/'npSumDistance' rather than over the list 'axisDistances'
-- builds. These say the two still agree, in particular on a 'Periodic' axis,
-- whose wrap-around distance is the one that could drift apart.
fusedMetricTests :: TestTree
fusedMetricTests =
  testGroup
    "the fused metrics agree with axisDistances"
    [ testProperty "Chebyshev is the largest per-axis distance" $ \(a :: Coord '[Clamped 5, Periodic 5]) b ->
        coordDistance a b === foldl' max 0 (axisDistances a b),
      testProperty "Manhattan is their sum" $ \(a :: Coord '[Clamped 5, Periodic 5]) b ->
        coordManhattan a b === sum (axisDistances a b),
      testProperty "on axes mixing every policy (Chebyshev)" $ \(a :: Coord '[Clamped 5, Periodic 4, Reflective 3, Reflect101 5]) b ->
        coordDistance a b === foldl' max 0 (axisDistances a b),
      testProperty "on axes mixing every policy (Manhattan)" $ \(a :: Coord '[Clamped 5, Periodic 4, Reflective 3, Reflect101 5]) b ->
        coordManhattan a b === sum (axisDistances a b),
      testCase "both are zero on the empty coord, as the empty fold is" $ do
        assertEqual "chebyshev" 0 (coordDistance EmptyCoord EmptyCoord)
        assertEqual "manhattan" 0 (coordManhattan EmptyCoord EmptyCoord)
    ]

-- | Worth stating because the torus case is where a hand-rolled distance
-- stops satisfying the laws.
metricLawTests :: TestTree
metricLawTests =
  testGroup
    "both distances are metrics"
    [ testProperty "identity: zero only to itself (Chebyshev)" $ \(c :: Coord '[Clamped 5, Periodic 5]) ->
        coordDistance c c === 0,
      testProperty "identity: zero only to itself (Manhattan)" $ \(c :: Coord '[Clamped 5, Periodic 5]) ->
        coordManhattan c c === 0,
      testProperty "zero distance means equal" $ \(a :: Coord '[Clamped 5, Periodic 5]) b ->
        (coordManhattan a b == 0) === (a == b),
      testProperty "symmetry (Chebyshev)" $ \(a :: Coord '[Clamped 5, Periodic 5]) b ->
        coordDistance a b === coordDistance b a,
      testProperty "symmetry (Manhattan)" $ \(a :: Coord '[Clamped 5, Periodic 5]) b ->
        coordManhattan a b === coordManhattan b a,
      testProperty "triangle inequality (Chebyshev)" $ \(a :: Coord '[Clamped 5, Periodic 5]) b c ->
        coordDistance a c <= coordDistance a b + coordDistance b c,
      testProperty "triangle inequality (Manhattan)" $ \(a :: Coord '[Clamped 5, Periodic 5]) b c ->
        coordManhattan a c <= coordManhattan a b + coordManhattan b c,
      testProperty "Chebyshev never exceeds Manhattan" $ \(a :: Coord '[Clamped 5, Periodic 5]) b ->
        coordDistance a b <= coordManhattan a b,
      testProperty "every Moore neighbour is one Chebyshev step away" $ \(c :: Coord '[Clamped 5, Periodic 5]) ->
        all (\n -> coordDistance c n == 1) (neighbours c),
      testProperty "every von Neumann neighbour is one Manhattan step away" $ \(c :: Coord '[Clamped 5, Periodic 5]) ->
        all (\n -> coordManhattan c n == 1) (vonNeumannNeighbours 1 c)
    ]

-- | The general primitive both neighbourhood functions are built from.
stepsWithinTests :: TestTree
stepsWithinTests =
  testGroup
    "stepsWithin is exported and carries the distances"
    [ testCase "the centre is the only entry at distance zero" $
        assertEqual
          ""
          [(0, hwc 2 2)]
          [e | e@(s, _) <- stepsWithin 2 (hwc 2 2), s == 0],
      testProperty "the recorded total is the Manhattan distance" $ \(c :: Coord '[Clamped 5, Periodic 5]) ->
        all (\(s, n) -> coordManhattan c n == s) (stepsWithin 2 c),
      testProperty "dropping the centre gives the Moore neighbourhood" $ \(c :: Coord '[Clamped 5, Periodic 5]) ->
        [n | (s, n) <- stepsWithin 2 c, s > 0] === mooreNeighbours 2 c
    ]

-- | The odd-extent precondition on 'centreCoord' is a compile error, not a
-- runtime one, so it is demonstrated here rather than tested:
--
-- >  centreCoord :: Coord '[Clamped 4]
-- >    error: Dimension '4' must be odd to have a centre coordinate
centredTests :: TestTree
centredTests =
  testGroup
    "centreCoord"
    [ testCase "a single odd axis sits at (n - 1) / 2" $
        assertEqual
          ""
          (singleCoord (hwOf 2 :: Clamped 5))
          (centreCoord :: Coord '[Clamped 5]),
      testCase "every axis of the same size is centred independently" $
        assertEqual "" (hwc 2 2) (centreCoord :: Coord '[Clamped 5, Clamped 5]),
      testCase "axes of different odd sizes each get their own middle" $
        assertEqual
          ""
          (hwOf 2 :| hwOf 1 :| EmptyCoord :: Coord '[Clamped 5, Clamped 3])
          centreCoord,
      testCase "a torus axis is centred the same way as a bounded one" $
        assertEqual
          ""
          (singleCoord (peOf 3 :: Periodic 7))
          (centreCoord :: Coord '[Periodic 7]),
      testCase "mixed boundary policies each centre their own axis" $
        assertEqual
          ""
          (hwOf 2 :| peOf 3 :| EmptyCoord :: Coord '[Clamped 5, Periodic 7])
          centreCoord,
      testCase "the empty coord centres to itself" $
        assertEqual "" EmptyCoord (centreCoord :: Coord '[]),
      testCase "equidistant from both ends of a Clamped axis" $
        let (c :| EmptyCoord) = centreCoord :: Coord '[Clamped 9]
         in assertEqual
              ""
              (axisDistance c (hwOf 0 :: Clamped 9))
              (axisDistance c (hwOf 8 :: Clamped 9)),
      testCase "equidistant from both ends of a Periodic axis" $
        let (c :| EmptyCoord) = centreCoord :: Coord '[Periodic 9]
         in assertEqual
              ""
              (axisDistance c (peOf 0 :: Periodic 9))
              (axisDistance c (peOf 8 :: Periodic 9))
    ]

puncturedTests :: TestTree
puncturedTests =
  testGroup
    "PuncturedCoord"
    [ testCase "as many as MaxCoordSize minus one, on a square window" $
        assertEqual
          ""
          (coordSpaceSize @'[Clamped 5, Clamped 5] - 1)
          (length (allPunctured @'[Clamped 5, Clamped 5])),
      testCase "a single-cell window has none" $
        assertEqual "" [] (allPunctured @'[Clamped 1]),
      testCase "never names the centre" $
        assertBool
          ""
          ( centreCoord
              `notElem` map puncturedToCoord (allPunctured @'[Clamped 5, Clamped 5])
          ),
      testCase "names every coordinate distinctly" $
        let cs = map puncturedToCoord (allPunctured @'[Clamped 5, Clamped 5])
         in assertEqual "" (length cs) (length (nub cs)),
      testCase "is allCoord with the centre left out, in the same order" $
        assertEqual
          ""
          (filter (/= centreCoord) (allCoord @'[Clamped 5, Clamped 5]))
          (map puncturedToCoord (allPunctured @'[Clamped 5, Clamped 5])),
      testCase "agrees on a mixed-axis, non-square window too" $
        assertEqual
          ""
          (filter (/= centreCoord) (allCoord @'[Clamped 3, Periodic 5]))
          (map puncturedToCoord (allPunctured @'[Clamped 3, Periodic 5]))
    ]

-- | Seven axes: one past the old six-axis ceiling.
type Seven =
  '[Clamped 3, Clamped 3, Clamped 3, Clamped 3, Clamped 3, Clamped 3, Clamped 3]

sevenOf :: Int -> Coord Seven
sevenOf n =
  let c = hwOf n
   in c :| c :| c :| c :| c :| c :| c :| EmptyCoord

sevenD :: Int -> Diff (Coord Seven)
sevenD d = d :^ d :^ d :^ d :^ d :^ d :^ d :^ NoDelta

arityTests :: TestTree
arityTests =
  testGroup
    "a coord past the old six-axis ceiling has a working offset API"
    [ testCase "(.+^) moves every axis" $
        assertEqual "" (sevenOf 2) (sevenOf 1 .+^ sevenD 1),
      testCase "(.-.) is the displacement that carries you back" $
        assertEqual
          ""
          (sevenOf 2)
          (sevenOf 1 .+^ (sevenOf 2 .-. sevenOf 1)),
      testCase "(.-.) reads off as a coord of Diffs" $
        assertEqual "" (sevenD 1) (sevenOf 2 .-. sevenOf 1),
      testCase "offsetCoord succeeds inside the grid" $
        assertEqual "" (Just (sevenOf 2)) (offsetCoord (sevenOf 1) (sevenD 1)),
      testCase "offsetCoord reports leaving the grid" $
        assertEqual "" Nothing (offsetCoord (sevenOf 0) (sevenD (-1))),
      testCase "the zero displacement is the identity" $
        assertEqual "" (sevenOf 1) (sevenOf 1 .+^ zeroV)
    ]

-- | Arity-generic: the same pair of functions covers a two-axis coord and a
-- seven-axis one, on both sides of the position\/displacement split --
-- 'coordFromTuple' for a 'Coord', 'deltaFromTuple' for a 'Delta'.
tupleBridgeTests :: TestTree
tupleBridgeTests =
  testGroup
    "coordFromTuple / coordToTuple / deltaFromTuple / deltaToTuple"
    [ testCase "deltaFromTuple builds the same displacement as (:^)" $
        assertEqual "" (d2 1 (-2)) (deltaFromTuple (1, -2)),
      testCase "deltaToTuple takes one apart" $
        assertEqual "" (1, -2 :: Int) (deltaToTuple (d2 1 (-2))),
      testCase "coordFromTuple builds the same coord as (:|)" $
        assertEqual "" (hwc 1 2) (coordFromTuple (toEnum 1, toEnum 2)),
      testCase "coordToTuple takes one apart" $
        assertEqual
          ""
          (toEnum 1, toEnum 2 :: Clamped 5)
          (coordToTuple (hwc 1 2)),
      testCase "a tuple offsets a coord through (.+^)" $
        assertEqual
          ""
          (hwc 3 3)
          (hwc 2 2 .+^ deltaFromTuple (1, 1)),
      testCase "seven axes go through the same function" $
        assertEqual
          ""
          (sevenD 1)
          (deltaFromTuple (1, 1, 1, 1, 1, 1, 1))
    ]

neighbourTests :: TestTree
neighbourTests =
  testGroup
    "Neighbours"
    [ offsetIsCoordTests,
      axisOffsetTests,
      offsetCoordTests,
      arityTests,
      tupleBridgeTests,
      mooreTests,
      vonNeumannTests,
      ordinalTests,
      axisDistanceTests,
      metricTests,
      mixedPolicyTests,
      fusedMetricTests,
      metricLawTests,
      stepsWithinTests,
      centredTests,
      puncturedTests
    ]
