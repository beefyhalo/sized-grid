{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Focused
  ( focusedTests,
  )
where

import Control.Comonad (extract)
import Data.AffineSpace ((.+^))
import Data.Functor.Rep (index, tabulate)
import Data.Grid.Sized
import Data.Maybe (fromMaybe)
import Test.Arbitrary ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty, (===))

-- | A two-dimensional displacement, the shape a single 'Path' step or a
-- 'traceOffset' argument takes on a two-axis grid.
d2 :: Int -> Int -> Delta '[Int, Int]
d2 a b = a :^ b :^ NoDelta

type FG = FocusedGrid '[Clamped 5, Clamped 5] Int

traceOffsetTests :: TestTree
traceOffsetTests =
  testGroup
    "traceOffset"
    [ testProperty "the zero displacement reads the focus" $
        \(fg :: FG) -> traceOffset (d2 0 0) fg === Just (extract fg),
      testProperty "agrees with offsetCoord and index at the focus" $
        \(fg :: FG) (a, b) ->
          traceOffset (d2 a b) fg
            === ( index (focusedGrid fg)
                    <$> offsetCoord (focusedGridPosition fg) (d2 a b)
                ),
      testCase "a step off the edge is Nothing" $
        let fg =
              FocusedGrid
                (tabulate (const (0 :: Int)))
                (hw 0 :| hw 0 :| EmptyCoord) ::
                FG
         in assertEqual "" Nothing (traceOffset (d2 (-1) 0) fg)
    ]
  where
    hw :: Int -> Clamped 5
    hw = Clamped . fromMaybe (error "in range") . numToOrdinal

tracePathTests :: TestTree
tracePathTests =
  testGroup
    "tracePath"
    [ testProperty "the empty path reads the focus" $
        \(fg :: FG) -> tracePath mempty fg === Just (extract fg),
      testProperty "agrees with walkPath and index at the focus" $
        \(fg :: FG) (steps :: [(Int, Int)]) ->
          let p = Path (map (uncurry d2) steps)
           in tracePath p fg
                === ( index (focusedGrid fg)
                        <$> walkPath (focusedGridPosition fg) p
                    ),
      testCase "a route a wall interrupts differs from its summed offset" $
        let fg =
              FocusedGrid
                (tabulate (const (0 :: Int)))
                (hw 0 :| hw 0 :| EmptyCoord) ::
                FG
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
    hw = Clamped . fromMaybe (error "in range") . numToOrdinal

walkEverywhereTests :: TestTree
walkEverywhereTests =
  testGroup
    "walkEverywhere"
    [ testProperty "extract agrees with tracePath from the original focus" $
        \(fg :: FG) (steps :: [(Int, Int)]) ->
          let p = Path (map (uncurry d2) steps)
           in extract (walkEverywhere p fg) === tracePath p fg,
      testProperty "every cell is tracePath from a walker starting there" $
        \(fg :: FG) (steps :: [(Int, Int)]) (q :: Coord '[Clamped 5, Clamped 5]) ->
          let p = Path (map (uncurry d2) steps)
           in index (focusedGrid (walkEverywhere p fg)) q
                === tracePath p (FocusedGrid (focusedGrid fg) q)
    ]

type RG = FocusedGrid '[Reflective 5, Reflective 5] Int

rf :: Int -> Reflective 5
rf = Reflective . fromMaybe (error "in range") . numToOrdinal

stepWalkerTests :: TestTree
stepWalkerTests =
  testGroup
    "stepWalker"
    [ testProperty "agrees with transportCoord on a Clamped grid" $
        \(fg :: FG) (a, b) ->
          let { w = Walker fg (d2 a b) False; (p', h') = transportCoord (focusedGridPosition fg) (d2 a b) }
           in stepWalker w === Walker (FocusedGrid (focusedGrid fg) p') h' (stepFrameFlips (focusedGridPosition fg) (d2 a b)),
      testProperty "agrees with transportCoord on a bounce grid" $
        \(fg :: RG) (a, b) ->
          let { w = Walker fg (d2 a b) False; (p', h') = transportCoord (focusedGridPosition fg) (d2 a b) }
           in stepWalker w === Walker (FocusedGrid (focusedGrid fg) p') h' (stepFrameFlips (focusedGridPosition fg) (d2 a b)),
      testProperty "a Clamped wall never turns the heading" $
        \(fg :: FG) (a, b) -> walkerHeading (stepWalker (Walker fg (d2 a b) False)) === d2 a b,
      testCase "a bounce wall reverses the heading on the axis it hit" $
        let { g = tabulate (const (0 :: Int)); w = Walker (FocusedGrid g (rf 0 :| rf 2 :| EmptyCoord)) (d2 (-1) 1) False; w' = stepWalker w }
         in do
             assertEqual
               "position"
               ((rf 0 .+^ (-1)) :| (rf 2 .+^ 1) :| EmptyCoord)
               (focusedGridPosition (walkerGrid w'))
             assertEqual "heading" (d2 1 1) (walkerHeading w')
             assertEqual "frame flip" True (walkerFrameFlips w')
    ]

partitionFocusTests :: TestTree
partitionFocusTests =
  testGroup
    "partitionFocus"
    [ testProperty "the first half is the value at the centre" $
        \(g :: Grid '[Clamped 5, Clamped 5] Int) ->
          fst (partitionFocus g) === index g centreCoord,
      testProperty "the second half agrees with index at every PuncturedCoord" $
        \(g :: Grid '[Clamped 5, Clamped 5] Int) ->
          let (_, neighbourAt) = partitionFocus g
           in all
                (\pc -> neighbourAt pc == index g (puncturedToCoord pc))
                allPunctured,
      testProperty "the second half's values are allCoord's, centre left out" $
        \(g :: Grid '[Clamped 5, Clamped 5] Int) ->
          let (_, neighbourAt) = partitionFocus g
           in map neighbourAt allPunctured
                === [index g c | c <- allCoord, c /= centreCoord]
    ]

focusedTests :: TestTree
focusedTests =
  testGroup
    "Focused"
    [ traceOffsetTests,
      tracePathTests,
      walkEverywhereTests,
      stepWalkerTests,
      partitionFocusTests
    ]
