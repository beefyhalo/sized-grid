{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for 'Path', the order-dependent counterpart to a displacement.
module Test.Path
  ( pathTests
  ) where

import           Data.Grid.Sized
import           Test.Arbitrary        ()

import           Data.Maybe            (fromJust)
import           GHC.TypeLits          (KnownNat)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty, (===))

hwOf :: KnownNat n => Int -> Clamped n
hwOf = Clamped . fromJust . numToOrdinal

hw :: Int -> Clamped 5
hw = hwOf

hwc :: Int -> Int -> Coord '[Clamped 5, Clamped 5]
hwc r c = hw r :| hw c :| EmptyCoord

d2 :: Int -> Int -> Delta '[Int, Int]
d2 a b = a :^ b :^ NoDelta

emptyPathTests :: TestTree
emptyPathTests =
    testGroup
        "the empty path is standing still"
        [ testCase "walking mempty is the identity" $
              assertEqual "" (Just (hwc 2 2)) (walkPath (hwc 2 2) mempty)
        , testCase "mempty sums to no displacement" $
              assertEqual
                  ""
                  (d2 0 0)
                  (pathOffset (mempty :: Path '[Clamped 5, Clamped 5]))
        ]

singleStepTests :: TestTree
singleStepTests =
    testGroup
        "a one-step path is offsetCoord"
        [ testProperty "walking it agrees with offsetCoord" $
              \(c :: Coord '[Clamped 5, Clamped 5]) (a, b) ->
                  walkPath c (Path [d2 a b]) === offsetCoord c (d2 a b)
        , testProperty "and its offset is the step itself" $ \(a, b) ->
              pathOffset (Path [d2 a b] :: Path '[Clamped 5, Clamped 5]) === d2 a b
        ]

collapseTests :: TestTree
collapseTests =
    testGroup
        "walking a path agrees with offsetting by its sum, on a torus"
        [ -- A torus never refuses a step, so nothing can block a route the
          -- combined displacement would otherwise reach.
          testProperty "any sequence of steps" $
              \(c :: Coord '[Periodic 5, Periodic 5]) (steps :: [(Int, Int)]) ->
                  let p = Path (map (uncurry d2) steps)
                   in walkPath c p === Just (fromJust (offsetCoord c (pathOffset p)))
        , testProperty "reordering the steps changes neither endpoint" $
              \(c :: Coord '[Periodic 5, Periodic 5]) (steps :: [(Int, Int)]) reordered ->
                  let p1 = Path (map (uncurry d2) steps)
                      p2 = Path (map (uncurry d2) (permute reordered steps))
                   in walkPath c p1 === walkPath c p2
        ]
  where
    -- Not a uniform shuffle, but enough to reorder a list of more than one element.
    permute :: Int -> [a] -> [a]
    permute _ [] = []
    permute n xs =
        let k = abs n `mod` length xs
            (front, back) = splitAt k xs
         in back ++ front

totalTests :: TestTree
totalTests =
    testGroup
        "walkPathTotal agrees with walkPath, on an all-Boundaryless coord"
        [ testProperty "any sequence of steps" $
              \(c :: Coord '[Periodic 5, Periodic 7]) (steps :: [(Int, Int)]) ->
                  let p = Path (map (uncurry d2) steps)
                   in walkPath c p === Just (walkPathTotal c p)
        ]

wallTests :: TestTree
wallTests =
    testGroup
        "a wall blocks a route even when it does not block the destination"
        [ -- Two steps that cancel still fail if the first one alone would have left
          -- the grid; this is why 'walkPath' is not simply 'offsetCoord' at the
          -- summed 'pathOffset'.
          testCase "out and back across the low edge fails, though it sums to zero" $
              assertEqual
                  ""
                  Nothing
                  (walkPath (hwc 0 0) (Path [d2 (-1) 0, d2 1 0]))
        , testCase "the combined displacement would have succeeded trivially" $
              assertEqual
                  ""
                  (Just (hwc 0 0))
                  (offsetCoord
                       (hwc 0 0)
                       (pathOffset
                            (Path [d2 (-1) 0, d2 1 0] :: Path '[Clamped 5, Clamped 5])))
        , testCase "the same two steps the other way round both succeed" $
              assertEqual
                  ""
                  (Just (hwc 0 0))
                  (walkPath (hwc 0 0) (Path [d2 1 0, d2 (-1) 0]))
        ]

pathTests :: TestTree
pathTests =
    testGroup
        "Path"
        [emptyPathTests, singleStepTests, collapseTests, totalTests, wallTests]
