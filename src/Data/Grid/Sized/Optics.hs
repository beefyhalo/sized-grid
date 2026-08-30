-- | The optics for sized coordinates, grids, and focused grids.
--
-- A facade. Nothing is defined here; each group below is re-exported from the
-- submodule that owns it:
--
--   * "Data.Grid.Sized.Optics.Coordinate" --- coordinates, displacements,
--     ordinals, and the @Field1@ ... @Field5@ orphan instances.
--   * "Data.Grid.Sized.Optics.Grid" --- grids.
--   * "Data.Grid.Sized.Optics.FocusedGrid" --- focused grids.
module Data.Grid.Sized.Optics
  ( -- * Coordinates
    _CoordAxes,
    _TransposedCoord,
    _CoordTuple,
    _CoordCons,
    _SingleCoord,
    _EmptyCoord,
    _Position,
    _Strengthened,
    _Weakened,
    _WeakenedCoord,
    translated,
    coordHead,
    coordTail,
    _WrappedDelta,
    _DeltaTuple,
    _DeltaCons,
    deltaHead,
    deltaTail,

    -- * Ordinals
    _Ordinal,

    -- * Grids
    _GridVector,
    permuted,
    _Transposed,
    _SplitGrid,
    _SplitHigherDim,
    _CollapsedGrid,
    cell,
    gridIndex,
    asGrid,
    slice,
    prefix,
    suffix,
    lowerDim,
    axisFold,

    -- * Focused grids
    _FocusedGrid,
    focus,
    unfocused,
  )
where

import Data.Grid.Sized.Optics.Coordinate
import Data.Grid.Sized.Optics.FocusedGrid
import Data.Grid.Sized.Optics.Grid
