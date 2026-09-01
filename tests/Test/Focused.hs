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
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Test.Arbitrary ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (property, testProperty, (===))

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

-- | The position-preserving lifting the pointing family was missing
-- (sized-grid-qbal). 'traceOffset' is now its @'fmap' 'extract'@, so the two
-- must agree, and a moved focus must carry the same grid.
offsetFocusTests :: TestTree
offsetFocusTests =
  testGroup
    "offsetFocus"
    [ testProperty "the zero displacement is the identity" $
        \(fg :: FG) -> offsetFocus (d2 0 0) fg === Just fg,
      testProperty "moves the focus by offsetCoord and keeps the grid" $
        \(fg :: FG) (a, b) ->
          offsetFocus (d2 a b) fg
            === ( FocusedGrid (focusedGrid fg)
                    <$> offsetCoord (focusedGridPosition fg) (d2 a b)
                ),
      testProperty "traceOffset is fmap extract . offsetFocus" $
        \(fg :: FG) (a, b) ->
          traceOffset (d2 a b) fg === (extract <$> offsetFocus (d2 a b) fg),
      testCase "a step off the edge is Nothing" $
        let fg =
              FocusedGrid
                (tabulate (const (0 :: Int)))
                (hw 0 :| hw 0 :| EmptyCoord) ::
                FG
         in assertBool "" (isNothing (offsetFocus (d2 (-1) 0) fg))
    ]
  where
    hw :: Int -> Clamped 5
    hw = Clamped . fromMaybe (error "in range") . numToOrdinal

walkFocusTests :: TestTree
walkFocusTests =
  testGroup
    "walkFocus"
    [ testProperty "the empty path is the identity" $
        \(fg :: FG) -> walkFocus mempty fg === Just fg,
      testProperty "moves the focus by walkPath and keeps the grid" $
        \(fg :: FG) (steps :: [(Int, Int)]) ->
          let p = Path (map (uncurry d2) steps)
           in walkFocus p fg
                === ( FocusedGrid (focusedGrid fg)
                        <$> walkPath (focusedGridPosition fg) p
                    ),
      testProperty "tracePath is fmap extract . walkFocus" $
        \(fg :: FG) (steps :: [(Int, Int)]) ->
          let p = Path (map (uncurry d2) steps)
           in tracePath p fg === (extract <$> walkFocus p fg),
      testCase "a route a wall interrupts fails even though its steps cancel" $
        let fg =
              FocusedGrid
                (tabulate (const (0 :: Int)))
                (hw 0 :| hw 0 :| EmptyCoord) ::
                FG
         in assertBool "" (isNothing (walkFocus (Path [d2 (-1) 0, d2 1 0]) fg))
    ]
  where
    hw :: Int -> Clamped 5
    hw = Clamped . fromMaybe (error "in range") . numToOrdinal

focusRayTests :: TestTree
focusRayTests =
  testGroup
    "focusRay"
    [ testProperty "its cells are coordRay's cells, grid unchanged" $
        \(fg :: FG) (a, b) ->
          -- a bounded prefix: the zero displacement makes both rays infinite.
          take 12 (map extract (focusRay (d2 a b) fg))
            === take
              12
              ( map
                  (index (focusedGrid fg))
                  (coordRay (focusedGridPosition fg) (d2 a b))
              ),
      testCase "does not include the starting focus" $
        let fg = focusedAtZero (tabulate (const (0 :: Int))) :: FG
         in case focusRay (d2 0 1) fg of
              [] -> assertFailure "the ray is empty"
              (first : _) ->
                assertBool "first cell is a step along, not the focus itself" $
                  focusedGridPosition first /= focusedGridPosition fg
    ]

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
          let w = Walker fg (d2 a b) identityFrame; (p', h') = transportCoord (focusedGridPosition fg) (d2 a b)
           in stepWalker w === Walker (FocusedGrid (focusedGrid fg) p') h' (frameAfterStep (focusedGridPosition fg) (d2 a b) identityFrame),
      testProperty "agrees with transportCoord on a bounce grid" $
        \(fg :: RG) (a, b) ->
          let w = Walker fg (d2 a b) identityFrame; (p', h') = transportCoord (focusedGridPosition fg) (d2 a b)
           in stepWalker w === Walker (FocusedGrid (focusedGrid fg) p') h' (frameAfterStep (focusedGridPosition fg) (d2 a b) identityFrame),
      testProperty "stepWalker composes the step into the incoming frame" $
        \(fg :: RG) (a, b) (u, v) ->
          let fr = frameFromReversals [u, v]
           in walkerFrame (stepWalker (Walker fg (d2 a b) fr))
                === frameAfterStep (focusedGridPosition fg) (d2 a b) fr,
      testProperty "a Clamped wall never turns the heading" $
        \(fg :: FG) (a, b) -> walkerHeading (stepWalker (Walker fg (d2 a b) identityFrame)) === d2 a b,
      testCase "a bounce wall reverses the heading on the axis it hit" $
        let g = tabulate (const (0 :: Int)); w = Walker (FocusedGrid g (rf 0 :| rf 2 :| EmptyCoord)) (d2 (-1) 1) identityFrame; w' = stepWalker w
         in do
              assertEqual
                "position"
                ((rf 0 .+^ (-1)) :| (rf 2 .+^ 1) :| EmptyCoord)
                (focusedGridPosition (walkerGrid w'))
              assertEqual "heading" (d2 1 1) (walkerHeading w')
              assertEqual "frame flip" True (walkerFrameFlips w')
    ]

-- | The 3x3 grid @[[0,1,2],[3,4,5],[6,7,8]]@ on two 'Ordinal' axes --- the
-- shape a window has, and the one 'stepWalker' cannot be written on.
ordinalBoard :: Grid '[Ordinal 3, Ordinal 3] Int
ordinalBoard = fromJust (gridFromList [[0, 1, 2], [3, 4, 5], [6, 7, 8]])

o3 :: Int -> Ordinal 3
o3 = fromMaybe (error "in range") . numToOrdinal

cl :: Int -> Clamped 5
cl = Clamped . fromMaybe (error "in range") . numToOrdinal

-- | The checked, position-preserving walker step (sized-grid-qbal). Unlike
-- 'stepWalker' it needs only 'IsCoordList', so it runs on an 'Ordinal' axis.
stepWalkerWithinTests :: TestTree
stepWalkerWithinTests =
  testGroup
    "stepWalkerWithin"
    [ testCase "the spike's ordinalWalk: crosses an Ordinal grid, stops at the edge" $
        let w0 =
              Walker
                (FocusedGrid ordinalBoard (o3 1 :| o3 0 :| EmptyCoord))
                (d2 0 1)
                identityFrame
         in assertEqual
              ""
              [3, 4, 5]
              (map (extract . walkerGrid) (walkerTrail w0)),
      testCase "a Clamped wall is reported with Nothing, not clamped onto" $
        let g = tabulate (const (0 :: Int)) :: Grid '[Clamped 5, Clamped 5] Int
            atWall = Walker (FocusedGrid g (cl 4 :| cl 0 :| EmptyCoord)) (d2 1 0) identityFrame
         in assertBool "" (isNothing (stepWalkerWithin atWall)),
      testProperty "the heading and the frame pass through unchanged" $
        \(fg :: FG) (a, b) (u, v) ->
          let fr = frameFromReversals [u, v]
           in case stepWalkerWithin (Walker fg (d2 a b) fr) of
                Nothing -> property True
                Just w' -> (walkerHeading w', walkerFrame w') === (d2 a b, fr),
      testProperty "agrees with stepWalker wherever the step stays in bounds" $
        \(fg :: FG) (a, b) ->
          let w = Walker fg (d2 a b) identityFrame
           in case stepWalkerWithin w of
                Nothing -> property True
                Just w' ->
                  ( focusedGridPosition (walkerGrid w'),
                    walkerHeading w'
                  )
                    === ( focusedGridPosition (walkerGrid (stepWalker w)),
                          walkerHeading (stepWalker w)
                        )
    ]

-- | The walker's own trail, and that it terminates where a checked step does.
walkerTrailTests :: TestTree
walkerTrailTests =
  testGroup
    "walkerTrail"
    [ testProperty "starts with the walker itself" $
        \(fg :: FG) (a, b) ->
          let w = Walker fg (d2 a b) identityFrame
           in take 1 (walkerTrail w) === [w],
      testProperty "each step is stepWalkerWithin of the one before" $
        \(fg :: FG) (a, b) ->
          let w = Walker fg (d2 a b) identityFrame
              t = take 6 (walkerTrail w)
           in and (zipWith (\x y -> stepWalkerWithin x == Just y) t (drop 1 t)),
      testCase "is finite on a Clamped grid and ends at the wall" $
        let g = tabulate (const (0 :: Int)) :: Grid '[Clamped 5, Clamped 5] Int
            w = Walker (FocusedGrid g (cl 0 :| cl 0 :| EmptyCoord)) (d2 0 1) identityFrame
            t = walkerTrail w
         in do
              assertEqual "one walker per cell up to the edge" 5 (length t)
              case drop 4 t of
                (wLast : _) ->
                  assertBool "the last cannot step again" $
                    isNothing (stepWalkerWithin wLast)
                [] -> assertFailure "the trail is shorter than the axis"
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
      offsetFocusTests,
      walkFocusTests,
      focusRayTests,
      walkEverywhereTests,
      stepWalkerTests,
      stepWalkerWithinTests,
      walkerTrailTests,
      partitionFocusTests
    ]
