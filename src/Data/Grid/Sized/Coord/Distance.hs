-- | How far apart two coordinates are, by the shorter route where an axis
-- offers more than one.
--
-- The two whole-coordinate metrics fold in the class method rather than over
-- 'axisDistances', so neither builds the per-axis list only to consume it.
module Data.Grid.Sized.Coord.Distance
  ( axisDistance
  , axisDistances
  , coordDistance
  , coordManhattan
  ) where

import           Data.Grid.Sized.Coord.Class
import           Data.Grid.Sized.Coord.Internal

-- | The number of steps between two values on a single axis, by the shorter route if the axis offers more than one.
axisDistance :: forall x. IsCoordLifted x => x -> x -> Int
axisDistance = axisDistanceIsCoord @(CoordContainer x) @(CoordNat x)

-- | The per-axis distances between two coords, first axis first.
axisDistances :: forall cs. IsCoordList cs => Coord cs -> Coord cs -> [Int]
axisDistances (Coord a) (Coord b) = posDistances @cs a b

-- | The Chebyshev distance: the largest per-axis distance. Folded by the
-- 'posMaxDistance' method rather than over 'axisDistances', so the @['Int']@
-- that was built only to be consumed immediately is gone (measured: 135 MB to
-- 60 MB over 360,000 calls).
coordDistance :: forall cs. IsCoordList cs => Coord cs -> Coord cs -> Int
coordDistance (Coord a) (Coord b) = posMaxDistance @cs a b

-- | The Manhattan distance: the per-axis distances summed, likewise without
-- the intermediate list.
coordManhattan :: forall cs. IsCoordList cs => Coord cs -> Coord cs -> Int
coordManhattan (Coord a) (Coord b) = posSumDistance @cs a b
