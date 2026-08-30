-- | The `GridOf` representation and everything defined over it.
--
-- This module is an aggregator. Nothing is defined here; it re-exports, in the
-- groups below, from four modules that each own one concern:
--
--   * "Data.Grid.Sized.Internal.Grid.Core" --- the representation, its
--     instances, and the bulk operations that ignore the axis list.
--   * "Data.Grid.Sized.Internal.Grid.Shape" --- the shape algebra: split,
--     join, take, drop, slice, permute, and the lower-dimension maps.
--   * "Data.Grid.Sized.Internal.Grid.Axis" --- reaching one named axis by
--     position, and walking its fibres.
--   * "Data.Grid.Sized.Internal.Grid.Windows" --- shrinking, tiling and
--     sliding windows.
--
-- A fifth, "Data.Grid.Sized.Internal.Grid.Nest", holds the axis-list recursion
-- behind `collapseGrid`, `gridFromList` and the JSON instances; it exists to
-- keep the vector parameter out of that recursion, and its module header says
-- why that matters.
module Data.Grid.Sized.Internal.Grid
  ( -- * Representation
    GridOf (..),
    Grid,
    unsafeGridFromVector,

    -- * Construction and access
    gridVector,
    gridFromVector,
    gridFromList,
    collapseGrid,

    -- * Single-cell access
    cellLens,

    -- * Bulk operations
    tabulateGrid,
    indexGrid,
    mapGrid,
    imapGrid,
    zipWithGrid,
    foldlGrid',
    scanl1Grid,

    -- * Type-level machinery
    CollapseGrid,

    -- * Rearranging
    permuteGrid,
    transposeGrid,
    splitGrid,
    combineGrid,
    combineHigherDim,
    splitHigherDim,
    dropGrid,
    takeGrid,
    sliceGrid,
    mapLowerDim,
    zipLowerDim,
    MapAxis (..),
    mapAxis,
    axisFibres,
    axis,
    scanAxis,
    DropAxis,
    foldAxis',
    reduceAxis,

    -- * Windows and tiles
    ShrinkableGrid (..),
    gridTiles,
    tiles,
    gridWindows,
    windows,

    -- * Vector helpers
    splitVectorBySize,
  )
where

import Data.Grid.Sized.Internal.Grid.Axis
import Data.Grid.Sized.Internal.Grid.Core
import Data.Grid.Sized.Internal.Grid.Shape
import Data.Grid.Sized.Internal.Grid.Windows
