-- | The optics for sized coordinates, grids, and focused grids.
--
-- This module gathers the optic-facing API in one place. The definitions stay
-- with the operations and types they describe; importing this module provides
-- a discoverable, explicit optics surface without duplicating implementations.
module Data.Grid.Sized.Optics
  ( -- * Coordinates
    _WrappedCoord
  , coordHead
  , coordTail
  , _WrappedDelta
  , deltaHead
  , deltaTail
    -- * Ordinals
  , _Ordinal
    -- * Grids
  , _SplitGrid
  , cell
  , gridIndex
  , asGrid
  , slice
  , prefix
  , suffix
  , lowerDim
    -- * Focused grids
  , _FocusedGrid
  , focus
  , unfocused
  ) where

import           Data.Grid.Sized.Class       (IsGrid (..))
import           Data.Grid.Sized.Coord       (coordHead, coordTail,
                                              _WrappedCoord)
import           Data.Grid.Sized.Coord.Delta (deltaHead, deltaTail,
                                               _WrappedDelta)
import           Data.Grid.Sized.Focused    (_FocusedGrid, focus, unfocused)
import           Data.Grid.Sized.Internal.Grid (_SplitGrid, cell, lowerDim,
                                                prefix, slice, suffix)
import           Data.Grid.Sized.Ordinal    (_Ordinal)
