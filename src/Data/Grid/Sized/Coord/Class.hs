-- | The two halves of what an axis list means, re-exported as one module.
--
-- This module is a facade. Nothing is defined here; its export list is
-- unchanged, and it re-exports from two modules that each own one concern:
--
--   * "Data.Grid.Sized.Coord.Class.Axis" -- what one axis\'s boundary policy
--     means: 'IsCoord', 'IsCoordLifted', 'Boundaryless', the index
--     conversions, and the size predicates.
--   * "Data.Grid.Sized.Coord.Class.List" -- the row-major fold over the axis
--     list: 'IsCoordList' and its two instances, plus 'IsCoordListF',
--     'MapDiff', 'MapStep' and 'AllDiffSame'.
--
-- The second is built on the first and the first does not mention the second,
-- so the split is one edge with no cycle.
module Data.Grid.Sized.Coord.Class
  ( IsCoord (..),
    IsCoordLifted (..),
    IsCoordList (..),
    IsCoordListF,
    Boundaryless,
    MapDiff,
    MapStep,
    AllDiffSame,
    Extremum (..),
    Even,
    Odd,
    OddC,
    maxCoordSize,
    allCoordLike,
    axisSteps,
    axisStepsIx,
    toAxisIndex,
    unsafeFromAxisIndex,
  )
where

import Data.Grid.Sized.Coord.Class.Axis
import Data.Grid.Sized.Coord.Class.List
