{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- | Tests for 'traceOffset', 'tracePath' and 'walkEverywhere', the plain
-- functions sized-grid-yws chose over a @ComonadTraced@ instance.
module Test.Focused
  ( focusedTests
  ) where

import           Data.Grid.Sized
import           Test.Arbitrary        ()

import           Control.Comonad       (extract)
import           Data.Functor.Rep      (index, tabulate)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty, (===))

-- | A two-dimensional displacement, the shape a single 'Path' step or a
-- 'traceOffset' argument takes on a two-axis grid.
d2 :: Int -> Int -> Coord '[Int, Int]
d2 a b = a :| b :| EmptyCoord

type FG = FocusedGrid '[ Clamped 5, Clamped 5] Int

traceOffsetTests :: TestTree
traceOffsetTests =
    testGroup
        "traceOffset"
        [ testProperty "the zero displacement reads the focus" $
              \(fg :: FG) -> traceOffset (d2 0 0) fg === Just (extract fg)
        , testProperty "agrees with offsetCoord and index at the focus" $
              \(fg :: FG) (a, b) ->
                  traceOffset (d2 a b) fg ===
                  (index (focusedGrid fg) <$>
                   offsetCoord (focusedGridPosition fg) (d2 a b))
        , testCase "a step off the edge is Nothing" $
              let fg = FocusedGrid (tabulate (const (0 :: Int)))
                                    (hw 0 :| hw 0 :| EmptyCoord) :: FG
               in assertEqual "" Nothing (traceOffset (d2 (-1) 0) fg)
        ]
  where
    hw :: Int -> Clamped 5
    hw = Clamped . maybe (error "in range") id . numToOrdinal

tracePathTests :: TestTree
tracePathTests =
    testGroup
        "tracePath"
        [ testProperty "the empty path reads the focus" $
              \(fg :: FG) -> tracePath mempty fg === Just (extract fg)
        , testProperty "agrees with walkPath and index at the focus" $
              \(fg :: FG) (steps :: [(Int, Int)]) ->
                  let p = Path (map (uncurry d2) steps)
                   in tracePath p fg ===
                      (index (focusedGrid fg) <$>
                       walkPath (focusedGridPosition fg) p)
        , -- The wall sized-grid-ghj and 'Test.Path.wallTests' both open with:
          -- two steps that cancel still fail if the first alone leaves the
          -- grid, so 'tracePath' cannot be replaced by 'traceOffset' at the
          -- path's summed 'pathOffset' in general.
          testCase "a route a wall interrupts differs from its summed offset" $
              let fg =
                      FocusedGrid
                          (tabulate (const (0 :: Int)))
                          (hw 0 :| hw 0 :| EmptyCoord) :: FG
                  p = Path [d2 (-1) 0, d2 1 0]
               in do
                    assertEqual "walked" Nothing (tracePath p fg)
                    assertEqual
                        "summed"
                        (Just 0)
                        (traceOffset (pathOffset p) fg)
        ]
  where
    hw :: Int -> Clamped 5
    hw = Clamped . maybe (error "in range") id . numToOrdinal

walkEverywhereTests :: TestTree
walkEverywhereTests =
    testGroup
        "walkEverywhere"
        [ testProperty "extract agrees with tracePath from the original focus" $
              \(fg :: FG) (steps :: [(Int, Int)]) ->
                  let p = Path (map (uncurry d2) steps)
                   in extract (walkEverywhere p fg) === tracePath p fg
        , -- The point of 'walkEverywhere': every cell of the result is the
          -- answer 'tracePath' would have given a walker that started there,
          -- not just at the original focus.
          testProperty "every cell is tracePath from a walker starting there" $
              \(fg :: FG) (steps :: [(Int, Int)]) (q :: Coord '[ Clamped 5, Clamped 5]) ->
                  let p = Path (map (uncurry d2) steps)
                   in index (focusedGrid (walkEverywhere p fg)) q ===
                      tracePath p (FocusedGrid (focusedGrid fg) q)
        ]

focusedTests :: TestTree
focusedTests =
    testGroup "Focused" [traceOffsetTests, tracePathTests, walkEverywhereTests]
