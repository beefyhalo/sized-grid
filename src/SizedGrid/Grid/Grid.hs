-- |
-- Module      :  SizedGrid.Grid.Grid
-- License     :  MIT -style (see the file LICENSE)
--
-- The `Grid` type and the safe operations over it.
--
-- `Grid` is exported abstractly: you get the type, not the constructor. A
-- @Grid cs a@ is a vector of exactly @MaxCoordSize cs@ elements, and that
-- equality is the invariant the rest of the library relies on -- `index` reads
-- a position it has computed from the type, and ('<*>') zips two vectors it
-- assumes are the same length. A public constructor makes both of those
-- assumptions the caller's problem, silently.
--
-- To get a grid: `gridFromVector` or `gridFromList` (both checked), `tabulate`,
-- `pure`, or any of the shape-changing functions here. To read one: `gridVector`,
-- `index`, or the `Foldable` instance. If you genuinely need to assert a length
-- the compiler cannot see -- the usual case is applying a length-preserving
-- vector function the library does not wrap -- "SizedGrid.Grid.Unsafe" has the
-- escape hatch, and it is not re-exported by "SizedGrid".
module SizedGrid.Grid.Grid
  ( -- * The grid type
    Grid
    -- * Construction
  , gridFromVector
  , gridFromList
    -- * Access
  , gridVector
  , collapseGrid
    -- * Type-level machinery
  , Head
  , Tail
  , CollapseGrid
  , AllGridSizeKnown(..)
  , GridSizeProof(..)
    -- * Rearranging
  , transposeGrid
  , splitGrid
  , combineGrid
  , combineHigherDim
  , splitHigherDim
  , dropGrid
  , takeGrid
  , mapLowerDim
  , zipLowerDim
  , scanl1Grid
    -- * Windows and tiles
  , ShrinkableGrid(..)
  , gridTiles
    -- * Vector helpers
  , splitVectorBySize
  ) where

import           SizedGrid.Internal.Grid
