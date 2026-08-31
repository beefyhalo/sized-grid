{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | 'Frame' --- the accumulated axis-reflection element a walker carries ---
-- and the two operations that read a step and a heading through it. The group
-- laws are checked by the 'quickcheck-classes' bundles in "Main"; this suite
-- is the behaviour those bundles do not reach.
module Test.Frame
  ( frameTests,
  )
where

import Data.Grid.Sized
import Data.Maybe (fromMaybe)
import Test.Arbitrary ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty, (===))

-- | A two-dimensional displacement, as in "Test.Focused".
d2 :: Int -> Int -> Delta '[Int, Int]
d2 a b = a :^ b :^ NoDelta

type C2 = Coord '[Reflective 5, Reflective 5]

rf :: Int -> Reflective 5
rf = Reflective . fromMaybe (error "in range") . numToOrdinal

cl :: Int -> Clamped 5
cl = Clamped . fromMaybe (error "in range") . numToOrdinal

-- | @frameFromReversals@ over a 2-axis space, spelled out for the properties.
f2 :: Bool -> Bool -> Frame '[Reflective 5, Reflective 5]
f2 u v = frameFromReversals [u, v]

reversalsTests :: TestTree
reversalsTests =
  testGroup
    "reversal bits"
    [ testCase "identityFrame has no axis reversed" $
        assertEqual
          ""
          [False, False]
          (frameReversals (identityFrame :: Frame '[Reflective 5, Reflective 5])),
      testProperty "frameReversals inverts frameFromReversals" $
        \(u, v) -> frameReversals (f2 u v) === [u, v],
      testProperty "every element is its own inverse" $
        \(u, v) -> f2 u v <> f2 u v === identityFrame,
      testProperty "parity counts the reversed axes" $
        \(u, v) -> frameParity (f2 u v) === odd (length (filter id [u, v]))
    ]

afterStepTests :: TestTree
afterStepTests =
  testGroup
    "frameAfterStep"
    [ testProperty "accumulating agrees with composing" $
        \(c :: C2) (a, b) (u, v) ->
          frameAfterStep c (d2 a b) (f2 u v)
            === f2 u v <> frameAfterStep c (d2 a b) identityFrame,
      testProperty "a non-reflecting space never flips a frame" $
        \(c :: Coord '[Clamped 5, Periodic 5]) (a, b) ->
          frameAfterStep c (a :^ b :^ NoDelta) identityFrame
            === (identityFrame :: Frame '[Clamped 5, Periodic 5]),
      testCase "a bounce on the first axis reverses the first axis only" $
        assertEqual
          ""
          [True, False]
          ( frameReversals $
              frameAfterStep
                (rf 0 :| cl 2 :| EmptyCoord :: Coord '[Reflective 5, Clamped 5])
                (d2 (-1) 0)
                identityFrame
          ),
      testCase "a bounce on the second axis reverses the second axis only" $
        assertEqual
          ""
          [False, True]
          ( frameReversals $
              frameAfterStep
                (cl 2 :| rf 0 :| EmptyCoord :: Coord '[Clamped 5, Reflective 5])
                (d2 0 (-1))
                identityFrame
          )
    ]

throughFrameTests :: TestTree
throughFrameTests =
  testGroup
    "throughFrame"
    [ testProperty "the identity frame is transparent" $
        \(a, b) ->
          throughFrame (identityFrame :: Frame '[Reflective 5, Reflective 5]) (d2 a b)
            === d2 a b,
      testProperty "is its own inverse" $
        \(u, v) (a, b) ->
          throughFrame (f2 u v) (throughFrame (f2 u v) (d2 a b)) === d2 a b,
      testCase "reverses exactly the component of a reversed axis" $
        assertEqual "" (d2 (-3) 4) (throughFrame (f2 True False) (d2 3 4)),
      testCase "the two odd-parity frames a bit cannot tell apart act differently" $
        -- Both f2 True False and f2 False True have odd parity, so
        -- walkerFrameFlips is True for each; throughFrame separates them.
        assertBool
          "distinct action on (1,1)"
          ( throughFrame (f2 True False) (d2 1 1)
              /= throughFrame (f2 False True) (d2 1 1)
          )
    ]

frameTests :: TestTree
frameTests =
  testGroup
    "Frame"
    [ reversalsTests,
      afterStepTests,
      throughFrameTests
    ]
