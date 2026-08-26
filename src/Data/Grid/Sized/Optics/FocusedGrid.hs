-- | Optics for a grid paired with a distinguished coordinate.
--
-- All three are views of the same pair, so 'focus' and 'unfocused' are
-- '_FocusedGrid' composed with a tuple lens rather than separate definitions.
module Data.Grid.Sized.Optics.FocusedGrid
  ( _FocusedGrid
  , focus
  , unfocused
  ) where

import           Data.Grid.Sized.Coord         (Coord)
import           Data.Grid.Sized.Focused       (FocusedGrid (..))
import           Data.Grid.Sized.Internal.Grid (Grid)

import           Control.Lens

_FocusedGrid :: Iso (FocusedGrid cs a) (FocusedGrid cs b)
                    (Grid cs a, Coord cs) (Grid cs b, Coord cs)
_FocusedGrid = iso (\(FocusedGrid g p) -> (g, p)) (uncurry FocusedGrid)

focus :: Lens' (FocusedGrid cs a) (Coord cs)
focus = _FocusedGrid . _2

unfocused :: Lens (FocusedGrid cs a) (FocusedGrid cs b) (Grid cs a) (Grid cs b)
unfocused = _FocusedGrid . _1
