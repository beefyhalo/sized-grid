module Test.Ray
  ( rayTests
  ) where

import           Data.Grid.Sized
import           Test.Arbitrary        ()

import           Data.Maybe            (fromJust)
import           GHC.TypeLits          (KnownNat)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty, (===), (==>))

-- | The bounded space: a walk along it ends.
hwOf :: KnownNat n => Int -> Clamped n
hwOf = Clamped . fromJust . numToOrdinal

-- | The torus: a walk along it never does.
peOf :: KnownNat n => Int -> Periodic n
peOf = Periodic . fromJust . numToOrdinal

hw :: Int -> Clamped 5
hw = hwOf

pe :: Int -> Periodic 5
pe = peOf

hwc :: Int -> Int -> Coord '[Clamped 5, Clamped 5]
hwc r c = hw r :| hw c :| EmptyCoord

pec :: Int -> Int -> Coord '[Periodic 5, Periodic 5]
pec r c = pe r :| pe c :| EmptyCoord

-- | One bounded axis and one torus axis: the interesting case, because the
-- bounded axis decides when the walk ends while the torus axis keeps wrapping.
mixc :: Int -> Int -> Coord '[Clamped 5, Periodic 5]
mixc r c = hw r :| pe c :| EmptyCoord

-- | A two-dimensional displacement, the shape 'Diff' gives every @Coord@ above.
d2 :: Int -> Int -> Delta '[Int, Int]
d2 a b = a :^ b :^ NoDelta

offsetCoordUpToTests :: TestTree
offsetCoordUpToTests =
    testGroup
        "offsetCoordUpTo reports where a walk stopped"
        [ testCase "no steps at all is the identity" $
              assertEqual "" (Right (hwc 2 2)) (offsetCoordUpTo 0 (hwc 2 2) (d2 1 1))
        , testCase "a walk that stays inside returns its endpoint" $
              assertEqual "" (Right (hwc 2 2)) (offsetCoordUpTo 2 (hwc 0 0) (d2 1 1))
        , testCase "a walk that leaves reports the last cell and the steps taken" $
              assertEqual
                  ""
                  (Left (OffGrid (hwc 4 4) 2))
                  (offsetCoordUpTo 5 (hwc 2 2) (d2 1 1))
        , testCase "a walk that cannot even start reports the start and zero steps" $
              assertEqual
                  ""
                  (Left (OffGrid (hwc 0 0) 0))
                  (offsetCoordUpTo 3 (hwc 0 0) (d2 (-1) 0))
        , testCase "a torus walk never leaves, however far it goes" $
              assertEqual "" (Right (pec 2 2)) (offsetCoordUpTo 12 (pec 0 0) (d2 1 1))
        , testCase "the bounded axis of a mixed coord is what ends the walk" $
              assertEqual
                  ""
                  (Left (OffGrid (mixc 4 4) 4))
                  (offsetCoordUpTo 6 (mixc 0 0) (d2 1 1))
        , testCase "the steps taken are the steps that succeeded" $
              assertEqual
                  ""
                  (Left (OffGrid (hwc 0 4) 4))
                  (offsetCoordUpTo 99 (hwc 0 0) (d2 0 1))
        ]

offsetCoordTests :: TestTree
offsetCoordTests =
    testGroup
        "offsetCoord is offsetCoordUpTo with the edge forgotten"
        [ testProperty "on a bounded coord" $ \(c :: Coord '[Clamped 5, Clamped 5]) (a, b) ->
              offsetCoord c (d2 a b) ===
              either (const Nothing) Just (offsetCoordUpTo 1 c (d2 a b))
        , testProperty "on a torus" $ \(c :: Coord '[Periodic 5, Periodic 5]) (a, b) ->
              offsetCoord c (d2 a b) ===
              either (const Nothing) Just (offsetCoordUpTo 1 c (d2 a b))
        , testProperty "on a coord mixing the two" $ \(c :: Coord '[Clamped 5, Periodic 5]) (a, b) ->
              offsetCoord c (d2 a b) ===
              either (const Nothing) Just (offsetCoordUpTo 1 c (d2 a b))
        ]

coordRayTests :: TestTree
coordRayTests =
    testGroup
        "coordRay walks until the grid runs out"
        [ testCase "a bounded ray stops at the edge" $
              assertEqual
                  ""
                  [hwc 1 1, hwc 2 2, hwc 3 3, hwc 4 4]
                  (coordRay (hwc 0 0) (d2 1 1))
        , testCase "the start is not part of the ray" $
              assertEqual "" [hwc 1 0] (take 1 (coordRay (hwc 0 0) (d2 1 0)))
        , testCase "a ray that cannot take its first step is empty" $
              assertEqual "" [] (coordRay (hwc 0 0) (d2 (-1) (-1)))
        , testCase "a torus ray never ends, it comes back round" $
              assertEqual
                  ""
                  [pec 0 1, pec 0 2, pec 0 3, pec 0 4, pec 0 0, pec 0 1, pec 0 2]
                  (take 7 (coordRay (pec 0 0) (d2 0 1)))
        , testCase "a mixed ray ends with the bounded axis while the torus wraps" $
              assertEqual
                  ""
                  [mixc 1 4, mixc 2 0, mixc 3 1, mixc 4 2]
                  (coordRay (mixc 0 3) (d2 1 1))
        ]

agreementTests :: TestTree
agreementTests =
    testGroup
        "offsetCoordUpTo and coordRay are the same walk"
        [ testProperty "the endpoint is the last of the prefix the ray takes" $ \(c :: Coord '[Clamped 5, Clamped 5]) (a, b) n ->
              let steps = abs n `mod` 8
                  prefix = take steps (coordRay c (d2 a b))
               in offsetCoordUpTo steps c (d2 a b) ===
                  (if length prefix == steps
                       then Right (last (c : prefix))
                       else Left (OffGrid (last (c : prefix)) (length prefix)))
        , -- Zero displacement excluded: it makes the ray infinite even on a
          -- bounded coord, since standing still never leaves the grid.
          testProperty "a ray is as long as the walk that fails first" $ \(c :: Coord '[Clamped 5, Clamped 5]) (a, b) ->
              (a, b) /= (0, 0) ==>
              let ray = coordRay c (d2 a b)
               in either stepsTaken (const (length ray)) (offsetCoordUpTo 99 c (d2 a b)) ===
                  length ray
        , testProperty "every cell of a bounded ray is on the grid, and the last is at an edge" $ \(c :: Coord '[Clamped 5, Clamped 5]) ->
              let ray = coordRay c (d2 1 0)
               in not (null ray) ==> onBoundary (last ray) === True
        ]

lastInsideTests :: TestTree
lastInsideTests =
    testGroup
        "lastInside names the edge a unit walk met"
        [ testCase "walking up ends at the top" $
              assertEqual
                  ""
                  (Left (OffGrid (hwc 4 0) 4))
                  (offsetCoordUpTo 9 (hwc 0 0) (d2 1 0))
        , testCase "walking down ends at the bottom" $
              assertEqual
                  ""
                  (Left (OffGrid (hwc 0 0) 4))
                  (offsetCoordUpTo 9 (hwc 4 0) (d2 (-1) 0))
        , testCase "the edge it met is the one axisBoundaries reports" $
              assertEqual
                  ""
                  [Just AtMax, Just AtMin]
                  (either (axisBoundaries . lastInside) (const []) (offsetCoordUpTo 9 (hwc 0 0) (d2 1 0)))
        , -- Starts in the middle column: column zero is on the boundary the
          -- whole way along and would pass for the wrong reason.
          testCase "a wide step stops at the last cell inside, edge or not" $
              assertEqual
                  ""
                  (Left (OffGrid (hwc 3 2) 1))
                  (offsetCoordUpTo 4 (hwc 0 2) (d2 3 0))
        , testCase "and that cell need not be on the boundary" $
              assertEqual
                  ""
                  False
                  (either (onBoundary . lastInside) (const True) (offsetCoordUpTo 4 (hwc 0 2) (d2 3 0)))
        ]

torusTests :: TestTree
torusTests =
    testGroup
        "a torus never reports an edge"
        [ testProperty "every walk on an all-Periodic coord succeeds" $ \(c :: Coord '[Periodic 5, Periodic 5]) (a, b) n ->
              let steps = abs n `mod` 20
               in either (const False) (const True) (offsetCoordUpTo steps c (d2 a b)) === True
        , testProperty "and its ray is infinite" $ \(c :: Coord '[Periodic 5, Periodic 5]) (a, b) ->
              length (take 50 (coordRay c (d2 a b))) === 50
        , testCase "a zero displacement stands still forever" $
              assertEqual "" (replicate 4 (hwc 2 2)) (take 4 (coordRay (hwc 2 2) (d2 0 0)))
        ]

threeStepTests :: TestTree
threeStepTests =
    testGroup
        "three steps in a direction, or nothing"
        [ testCase "a diagonal with room for three" $
              assertEqual
                  ""
                  (Just [hwc 1 1, hwc 2 2, hwc 3 3])
                  (threeFrom (hwc 0 0) (d2 1 1))
        , testCase "a diagonal without room stops short" $
              assertEqual "" Nothing (threeFrom (hwc 3 3) (d2 1 1))
        , testCase "and off the near edge there is no room at all" $
              assertEqual "" Nothing (threeFrom (hwc 0 0) (d2 (-1) (-1)))
        ]
  where
    threeFrom c d =
        case take 3 (coordRay c d) of
            r@[_, _, _] -> Just r
            _           -> Nothing

rayTests :: TestTree
rayTests =
    testGroup
        "Rays and partial offsets"
        [ offsetCoordUpToTests
        , offsetCoordTests
        , coordRayTests
        , agreementTests
        , lastInsideTests
        , torusTests
        , threeStepTests
        ]
