module Data.Grid.Sized.Grid.Class
  ( IsGrid(..)
  ) where

import           Data.Grid.Sized.Coord
import           Data.Grid.Sized.Grid.Focused
import           Data.Grid.Sized.Grid
-- (& ix .~) replaces one element, so the length is unchanged. That is the whole
-- of the obligation unsafeGridFromVector carries, discharged here by inspection.
import           Data.Grid.Sized.Grid.Unsafe  (unsafeGridFromVector)

import           Control.Lens           hiding (index)
import           Data.Functor.Rep

-- | Conversion between `Grid` and `FocusedGrid` and access grids at a `Coord`
class IsGrid cs grid | grid -> cs where
  -- | Get the element at a grid location. This is a lens because we know it must exist
  gridIndex :: Coord cs -> Lens' (grid a) a
  -- | Convert to, or run a function over, a `Grid`
  asGrid :: Lens' (grid a) (Grid cs a)
  -- | Convert to, or run a function over, a `FocusedGrid`
  asFocusedGrid :: Lens' (grid a) (FocusedGrid cs a)

instance (AllSizedKnown cs, IsCoordList cs) =>
         IsGrid cs (Grid cs) where
    gridIndex coord =
        lens
            (\g -> index g coord)
            (\g a ->
                 unsafeGridFromVector
                     (gridVector g & ix (coordPosition coord) .~ a))
    asGrid = id
    asFocusedGrid =
        lens (\g -> FocusedGrid g zeroCoord) (\_ fg -> focusedGrid fg)

instance (AllSizedKnown cs, IsCoordList cs) =>
         IsGrid cs (FocusedGrid cs) where
    gridIndex c =
        (\f (FocusedGrid g p) -> (\g' -> FocusedGrid g' p) <$> f g) .
        gridIndex c
    asGrid = lens focusedGrid (\(FocusedGrid _ p) g -> FocusedGrid g p)
    asFocusedGrid = id
