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
    MapAxis (..),
    mapAxis,
    axisFibres,
    axis,
    scanAxis,

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
-- this group too.
--
-- 'takeGrid', 'dropGrid' and 'splitHigherDim' /restrict/: each narrows the
-- outermost axis, so each returns 'Ordinal' along it whatever the source\'s
-- policy was. 'transposeGrid', 'splitGrid', 'combineGrid', 'mapLowerDim' and
-- 'zipLowerDim' keep every axis at its own size and so keep every policy;
-- 'splitGrid' is worth naming because it looks like a restriction and is not.
--
-- 'combineHigherDim' and 'permuteGrid' are /constructions/, where the policy
-- is the caller\'s to declare: they are asserting a topology rather than
-- reading one off. So split-then-recombine does not give back the axis it
-- started from --- a @Periodic 9@ splits into two runs of cells and gluing
-- them yields @Ordinal 9@ --- and that is the honest answer, because whether
-- cell 8 is adjacent to cell 0 is a fact about the space they were cut from
-- and not about either run. A caller who wants it back asserts it, with
-- 'permuteGrid' or by rebuilding through 'gridFromVector'.

-- $windows
--
-- These /restrict/: each narrows a grid's extent and keeps no position in the
-- source. The rule they obey is that the narrowed axis comes back as
-- 'Ordinal' whatever the source's axis type was --- the policy-free axis,
-- with no walls and no wrap, whose off-grid step is 'Nothing' rather than an
-- invented answer. Axes left at full width keep their policies, because they
-- have not been restricted, and the offsets are 'Ordinal' too, an offset
-- being an index rather than a position in a space.
--
-- A window that kept its source\'s policy would describe a seam that is not in
-- the space it is a view of: periodicity is a property of a whole axis, so a
-- proper sub-window of a periodic axis is not periodic, and \"clamped\" means
-- stepping off the edge stays at the edge, which is a claim about a wall the
-- window\'s edge does not have. See the header of
-- @Data.Grid.Sized.Internal.Grid.Windows@ for the worked example. The
-- counterpart rule --- /a pointing preserves the boundary policy/ --- is what
-- 'Data.Grid.Sized.Focused.FocusedGrid' does instead.
--
-- The same rule governs the narrowing half of the \"Rearranging\" group above
-- and the 'Data.Grid.Sized.Optics.slice' \/
-- 'Data.Grid.Sized.Optics.prefix' \/ 'Data.Grid.Sized.Optics.suffix' lenses.
