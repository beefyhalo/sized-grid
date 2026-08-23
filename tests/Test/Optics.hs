{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Optics
  ( opticTests
  ) where

import           Control.Lens
import           Data.Grid.Sized
import           Data.Maybe          (isNothing)
import           Generics.SOP        (NP)
import           Test.Arbitrary      ()
import           Test.Tasty
import           Test.Tasty.QuickCheck (chooseInt, forAll, oneof, testProperty,
                                        (===))
import           Test.Utils          (isoLaws, lensLaws)

type Coord2 = Coord '[Ordinal 5, Ordinal 7]
type Grid2 = Grid '[Ordinal 5, Ordinal 7] Int
type Focused2 = FocusedGrid '[Ordinal 5, Ordinal 7] Int

coordOpticTests :: TestTree
coordOpticTests =
  testGroup
    "Coordinate optics"
    [ isoLaws "_WrappedCoord"
        (_WrappedCoord :: Iso' Coord2 (NP I '[Ordinal 5, Ordinal 7]))
    , lensLaws "coordHead" (coordHead :: Lens' Coord2 (Ordinal 5))
    , lensLaws "coordTail" (coordTail :: Lens' Coord2 (Coord '[Ordinal 7]))
    ]

deltaOpticTests :: TestTree
deltaOpticTests =
  testGroup
    "Delta optics"
    [ isoLaws "_WrappedDelta"
        (_WrappedDelta :: Iso' (Delta '[Int, Int]) (NP I '[Int, Int]))
    , lensLaws "deltaHead"
        (deltaHead :: Lens' (Delta '[Int, Int]) Int)
    , lensLaws "deltaTail"
        (deltaTail :: Lens' (Delta '[Int, Int]) (Delta '[Int]))
    ]

focusedOpticTests :: TestTree
focusedOpticTests =
  testGroup
    "Focused-grid optics"
    [ isoLaws "_FocusedGrid"
        (_FocusedGrid :: Iso' Focused2 (Grid2, Coord2))
    , lensLaws "focus" (focus :: Lens' Focused2 Coord2)
    , lensLaws "unfocused" (unfocused :: Lens' Focused2 Grid2)
    , lensLaws "asGrid" (asGrid :: Lens' Focused2 Grid2)
    , lensLaws "gridIndex" (gridIndex (zeroCoord :: Coord2) :: Lens' Grid2 Int)
    ]

gridOpticTests :: TestTree
gridOpticTests =
  testGroup
    "Grid optics"
    [ isoLaws "_SplitGrid"
        (_SplitGrid :: Iso' Grid2 (Grid '[Ordinal 5] (Grid '[Ordinal 7] Int)))
    , lensLaws "cell" (cell (zeroCoord :: Coord2) :: Lens' Grid2 Int)
    , lensLaws "slice" (slice 1 2 :: Lens' (Grid '[Ordinal 5] Int) (Grid '[Ordinal 2] Int))
    , lensLaws "prefix" (prefix 2 :: Lens' (Grid '[Ordinal 5] Int) (Grid '[Ordinal 2] Int))
    , lensLaws "suffix" (suffix 2 :: Lens' (Grid '[Ordinal 5] Int) (Grid '[Ordinal 3] Int))
    ]

ordinalOpticTests :: TestTree
ordinalOpticTests =
  testGroup
    "Ordinal prism"
    [ testProperty "preview after review is Just" $ \(value :: Ordinal 5) ->
        preview (_Ordinal :: Prism' Int (Ordinal 5)) (review _Ordinal value) === Just value
    , testProperty "review preserves every valid Int" $
        forAll (chooseInt (0, 4)) $ \value ->
          review (_Ordinal :: Prism' Int (Ordinal 5)) (unsafeOrdinal @5 value) === value
    , testProperty "preview rejects every invalid Int" $
        forAll (oneof [chooseInt (-100, -1), chooseInt (5, 100)]) $ \value ->
          isNothing (preview (_Ordinal :: Prism' Int (Ordinal 5)) value)
    ]

opticTests :: TestTree
opticTests =
  testGroup
    "Optic laws"
    [ coordOpticTests
    , deltaOpticTests
    , focusedOpticTests
    , gridOpticTests
    , ordinalOpticTests
    ]