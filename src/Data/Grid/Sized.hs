{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | The `Grid` type, the safe operations over it, and a re-export of the rest
-- of the public API.
module Data.Grid.Sized
  ( -- * The grid type
    Grid,
    GridOf,

    -- * Construction
    gridFromVector,
    gridFromList,

    -- * Access
    gridVector,
    collapseGrid,

    -- * Bulk operations
    tabulateGrid,
    indexGrid,
    mapGrid,
    imapGrid,
    zipWithGrid,
    foldlGrid',
    mapMaybeGrid,
    filterGrid,
    catMaybesGrid,
    witherGrid,

    -- * Type-level machinery
    CollapseGrid,

    -- * Rearranging

    --
    -- $rearranging
    permuteGrid,
    transposeGrid,
    splitGrid,
    combineGrid,
    combineHigherDim,
    splitHigherDim,
    dropGrid,
    takeGrid,
    mapLowerDim,
    zipLowerDim,
    scanl1Grid,
    AxisAt,
    AxisSize,
    AxisStride,
    KnownAxis,
    mapAxis,
    axisFibres,
    axis,
    scanAxis,
    DropAxis,
    foldAxis',
    reduceAxis,

    -- * Windows and tiles

    --
    -- $windows
    ShrinkableGrid (..),
    gridTiles,
    tiles,
    gridWindows,
    windows,

    -- * Vector helpers
    splitVectorBySize,

    -- * The rest of the public API
    module X,
    module Fixture,
    module Optics,

    -- * Rexported for generics-sop
    All,
    SListI,
    Compose,
    I (..),
  )
where

import Data.Grid.Sized.Class as X hiding (asGrid, gridIndex)
import Data.Grid.Sized.Coord as X
import Data.Grid.Sized.Coord.Clamped as X
import Data.Grid.Sized.Coord.Class as X
import Data.Grid.Sized.Coord.Periodic as X
import Data.Grid.Sized.Coord.Reflect101 as X
import Data.Grid.Sized.Coord.Reflective as X
import Data.Grid.Sized.Fixture as Fixture
import Data.Grid.Sized.Focused as X
-- The `Grid` type is defined in the hidden "Data.Grid.Sized.Internal.Grid", and
-- this module publishes the safe half of it -- hence the explicit export list
-- above rather than a `module X` re-export, which would carry the constructor
-- and `sliceGrid` out with everything else.
import Data.Grid.Sized.Internal.Grid
import Data.Grid.Sized.Optics as Optics
import Data.Grid.Sized.Ordinal as X
import Data.Grid.Sized.Stencil as X
import Generics.SOP

-- $rearranging
--
-- Sorted by the rule in the \"Windows and tiles\" section below, which governs
-- this group too: 'takeGrid', 'dropGrid' and 'splitHigherDim' /restrict/ the
-- outermost axis and so return 'Ordinal' along it whatever the source\'s
-- policy was, while 'transposeGrid', 'splitGrid', 'combineGrid', 'mapLowerDim'
-- and 'zipLowerDim' keep every axis at its size and so keep every policy.
-- 'combineHigherDim' and 'permuteGrid' are /constructions/: the policy is the
-- caller\'s to assert, so split-then-recombine does not give back the axis it
-- started from. See the header of @Data.Grid.Sized.Internal.Grid.Windows@ for
-- the worked example.

-- $windows
--
-- These /restrict/: each narrows a grid's extent and keeps no position in the
-- source. The narrowed axis comes back as 'Ordinal' whatever the source's
-- axis type was --- no walls, no wrap, an off-grid step of 'Nothing' rather
-- than an invented answer --- because a sub-window that kept the source's
-- policy would describe a seam that is not in the space it is a view of. Axes
-- left at full width keep their policies, and the offsets are 'Ordinal' too.
-- See the header of @Data.Grid.Sized.Internal.Grid.Windows@ for the worked
-- example and the reasoning.
--
-- A window is still a grid a caller can move around inside:
-- 'Data.Grid.Sized.Coord.offsetCoord', 'Data.Grid.Sized.Coord.coordRay',
-- 'Data.Grid.Sized.Coord.walkPath' and their
-- 'Data.Grid.Sized.Focused.FocusedGrid' liftings all work on it, because
-- checked movement asks only for a bounds check per axis. Total movement ---
-- @('Data.AffineSpace..+^')@, 'Data.Grid.Sized.Focused.stepWalker' --- still
-- refuses 'Ordinal': an axis that cannot leave its interval licenses no way
-- back.
--
-- The counterpart rule --- /a pointing preserves the boundary policy/ --- is
-- what 'Data.Grid.Sized.Focused.FocusedGrid' does instead. The same
-- restriction rule governs the narrowing half of the \"Rearranging\" group
-- above and the 'Data.Grid.Sized.Optics.slice' \/
-- 'Data.Grid.Sized.Optics.prefix' \/ 'Data.Grid.Sized.Optics.suffix' lenses.
