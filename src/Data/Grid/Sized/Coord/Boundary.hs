-- | Where a coordinate sits relative to the ends of its own axes.
--
-- A 'Data.Grid.Sized.Coord.Periodic.Periodic' axis has no ends, so it never
-- reports an 'Extremum' and never makes a coordinate a corner.
module Data.Grid.Sized.Coord.Boundary
  ( axisBoundary,
    axisBoundaries,
    onBoundary,
    isCorner,
    interiorCoords,
  )
where

import Data.Grid.Sized.Coord.Class
import Data.Grid.Sized.Coord.Internal
import Generics.SOP (SList (..), sList)

-- | Which end of its axis a single coordinate sits at, or 'Nothing' if interior.
axisBoundary :: forall x. (IsCoordLifted x) => x -> Maybe Extremum
axisBoundary = axisBoundaryIsCoord @(CoordContainer x) @(CoordNat x)

-- | Where each axis of a coord sits relative to its own ends, first axis first.
axisBoundaries ::
  forall cs.
  (IsCoordList cs) =>
  Coord cs ->
  [Maybe Extremum]
axisBoundaries (Coord p) = posBoundaries @cs p

-- | Whether any axis is at one of its ends. 'False' on a coord with no axes.
onBoundary :: forall cs. (IsCoordList cs) => Coord cs -> Bool
onBoundary (Coord p) = posAnyBoundary @cs p

-- | Whether every axis is at one of its ends. 'False' on any coord with a torus axis, and 'False' rather than a vacuous 'True' on the empty coord.
--
-- The 'SList' match is what keeps the empty coord 'False': 'posAllBoundary' is
-- a fold and so vacuously 'True' there. It replaces a match on the coord's own
-- @Nil@, which there is no longer a spine to perform --- but the emptiness of
-- @cs@ is a property of the type, so 'SList' answers it without one.
isCorner :: forall cs. (IsCoordList cs) => Coord cs -> Bool
isCorner (Coord p) =
  case sList :: SList cs of
    SNil -> False
    SCons -> posAllBoundary @cs p

-- | Every coordinate that is not 'onBoundary', in 'allCoord' order.
interiorCoords :: (IsCoordList cs) => [Coord cs]
interiorCoords = filter (not . onBoundary) allCoord
