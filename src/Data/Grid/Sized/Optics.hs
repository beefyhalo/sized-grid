-- | The optics for sized coordinates, grids, and focused grids.
--
-- A facade. Nothing is defined here; each submodule's own export list is the
-- single source of truth for the names it owns:
--
--   * "Data.Grid.Sized.Optics.Coordinate" --- coordinates, displacements,
--     ordinals, and the @Field1@ ... @Field5@ orphan instances.
--   * "Data.Grid.Sized.Optics.Grid" --- grids.
--   * "Data.Grid.Sized.Optics.FocusedGrid" --- focused grids.
module Data.Grid.Sized.Optics
  ( module Data.Grid.Sized.Optics.Coordinate,
    module Data.Grid.Sized.Optics.Grid,
    module Data.Grid.Sized.Optics.FocusedGrid,
  )
where

import Data.Grid.Sized.Optics.Coordinate
import Data.Grid.Sized.Optics.FocusedGrid
import Data.Grid.Sized.Optics.Grid
