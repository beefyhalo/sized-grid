{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Optics
  ( opticTests
  ) where

import           Control.Lens
import           Data.Grid.Sized
import           Data.Maybe          (isNothing)
import qualified Data.Vector         as V
import           Generics.SOP        (NP)
import           Test.Arbitrary      ()
import           Test.Tasty
import           Test.Tasty.QuickCheck (chooseInt, testProperty)
import           Test.Utils          (isoLaws, lensLaws, prismLaws, prismLawsFrom,
                                      traversalLaws)

type Coord2 = Coord '[Ordinal 5, Ordinal 7]
type Grid2 = Grid '[Ordinal 5, Ordinal 7] Int
type Focused2 = FocusedGrid '[Ordinal 5, Ordinal 7] Int

coordOpticTests :: TestTree
coordOpticTests =
  testGroup
    "Coordinate optics"
    [ isoLaws "_CoordAxes"
      (_CoordAxes :: Iso' Coord2 (NP I '[Ordinal 5, Ordinal 7]))
    , prismLaws "_Position" (_Position :: Prism' Int Coord2)
    , prismLaws "_Strengthened"
      (_Strengthened :: Prism' (Ordinal 7) (Ordinal 5))
    , prismLaws "_Weakened"
      (_Weakened :: Prism' (Clamped 7) (Clamped 5))
    , prismLaws "_WeakenedCoord"
      (_WeakenedCoord :: Prism' (Coord '[Ordinal 7, Ordinal 9]) Coord2)
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
    , testGroup "_GridVector is a prism"
      [ prismLawsFrom (V.replicate 35 <$> chooseInt (-100, 100))
          "_GridVector" (_GridVector :: Prism' (V.Vector Int) Grid2)
      , testProperty "preview rejects the wrong length" $
        isNothing $ preview (_GridVector :: Prism' (V.Vector Int) Grid2)
          (V.replicate 34 0)
      ]
    , testGroup "lowerDim"
      [ traversalLaws
        (lowerDim :: Traversal' Grid2 (Grid '[Ordinal 7] Int))
      ]
    , lensLaws "cell" (cell (zeroCoord :: Coord2) :: Lens' Grid2 Int)
    , lensLaws "slice" (slice 1 2 :: Lens' (Grid '[Ordinal 5] Int) (Grid '[Ordinal 2] Int))
    , lensLaws "prefix" (prefix 2 :: Lens' (Grid '[Ordinal 5] Int) (Grid '[Ordinal 2] Int))
    , lensLaws "suffix" (suffix 2 :: Lens' (Grid '[Ordinal 5] Int) (Grid '[Ordinal 3] Int))
    ]

ordinalOpticTests :: TestTree
ordinalOpticTests =
  testGroup
    "Ordinal prism"
    [ prismLaws "_Ordinal" (_Ordinal :: Prism' Int (Ordinal 5))
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