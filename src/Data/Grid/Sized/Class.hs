module Data.Grid.Sized.Class
  ( IsGrid(..)
  ) where

import           Data.Grid.Sized.Coord
import           Data.Grid.Sized.Focused
import           Data.Grid.Sized.Internal.Grid (Grid, gridVector)
import           Data.Grid.Sized.Unsafe        (unsafeGridFromVector)

import           Control.Lens           hiding (index)
import           Data.Functor.Rep
import qualified Data.Vector                   as V

-- | Conversion between `Grid` and `FocusedGrid` and access grids at a `Coord`
class IsGrid cs grid | grid -> cs where
  gridIndex :: Coord cs -> Lens' (grid a) a
  asGrid :: Lens' (grid a) (Grid cs a)
  asFocusedGrid :: Lens' (grid a) (FocusedGrid cs a)

instance (AllSizedKnown cs, IsCoordList cs) =>
         IsGrid cs (Grid cs) where
    -- 'V.unsafeUpd' rather than the @ix (coordPosition coord)@ traversal this
    -- used to be, for the reason `Data.Grid.Sized.indexGrid` drops its bounds
    -- check: the position is in range by the 'Data.Grid.Sized.Ordinal.Ordinal'
    -- invariant and the vector has exactly that many elements by the grid's own
    -- size invariant, so the traversal could only ever have matched. Writing it
    -- as a traversal also said the lens might miss its target, which is the one
    -- thing the type of 'gridIndex' promises it does not.
    gridIndex coord =
        lens
            (`index` coord)
            (\g a ->
                 unsafeGridFromVector
                     (V.unsafeUpd (gridVector g) [(coordPosition coord, a)]))
    asGrid = id
    asFocusedGrid = lens (`FocusedGrid` zeroCoord) (const focusedGrid)

instance (AllSizedKnown cs, IsCoordList cs) =>
         IsGrid cs (FocusedGrid cs) where
    gridIndex c = (\f (FocusedGrid g p) -> (`FocusedGrid` p) <$> f g) . gridIndex c
    asGrid = lens focusedGrid (\(FocusedGrid _ p) g -> FocusedGrid g p)
    asFocusedGrid = id
