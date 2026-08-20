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

-- | Access grids at a `Coord`, and the `Grid` a `Grid` or `FocusedGrid`
-- contains. There is no @asFocusedGrid@: a `Grid` has no focus to hand back,
-- so no `Lens'` from it to `FocusedGrid` can be lawful (see sized-grid-3au).
-- Building a `FocusedGrid` from a `Grid` needs a `Coord` supplied -- write
-- @FocusedGrid g p@, or go through `_FocusedGrid`.
class IsGrid cs grid | grid -> cs where
  gridIndex :: Coord cs -> Lens' (grid a) a
  asGrid :: Lens' (grid a) (Grid cs a)

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

instance (AllSizedKnown cs, IsCoordList cs) =>
         IsGrid cs (FocusedGrid cs) where
    gridIndex c = (\f (FocusedGrid g p) -> (`FocusedGrid` p) <$> f g) . gridIndex c
    asGrid = unfocused
