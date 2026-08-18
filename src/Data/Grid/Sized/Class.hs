module Data.Grid.Sized.Class
  ( IsGrid(..)
  ) where

import           Data.Grid.Sized.Coord
import           Data.Grid.Sized.Focused
import           Data.Grid.Sized.Internal.Grid (Grid, gridVector)
import           Data.Grid.Sized.Unsafe        (unsafeGridFromVector)

import           Control.Lens           hiding (index)
import           Data.Functor.Rep

-- | Conversion between `Grid` and `FocusedGrid` and access grids at a `Coord`
class IsGrid cs grid | grid -> cs where
  gridIndex :: Coord cs -> Lens' (grid a) a
  asGrid :: Lens' (grid a) (Grid cs a)
  asFocusedGrid :: Lens' (grid a) (FocusedGrid cs a)

instance (AllSizedKnown cs, IsCoordList cs) =>
         IsGrid cs (Grid cs) where
    gridIndex coord =
        lens
            (`index` coord)
            (\g a ->
                 unsafeGridFromVector
                     (gridVector g & ix (coordPosition coord) .~ a))
    asGrid = id
    asFocusedGrid = lens (`FocusedGrid` zeroCoord) (const focusedGrid)

instance (AllSizedKnown cs, IsCoordList cs) =>
         IsGrid cs (FocusedGrid cs) where
    gridIndex c = (\f (FocusedGrid g p) -> (`FocusedGrid` p) <$> f g) . gridIndex c
    asGrid = lens focusedGrid (\(FocusedGrid _ p) g -> FocusedGrid g p)
    asFocusedGrid = id
