-- | Grid coordinates indexed by a type-level list of axes.
--
-- == The representation
--
-- A @'Coord' cs@ /is/ its row-major position: one 'Int' in
-- @[0, 'MaxCoordSize' cs)@ and nothing else (sized-grid-adr.16). The axis list
-- still indexes the type and still carries the boundary policy --
-- @Coord '[Clamped 5, Periodic 3]@ means exactly what it meant -- but there is
-- no longer a spine of boxes behind it, so 'coordPosition' is free and every
-- operation that used to build a coordinate only to collapse it to an 'Int'
-- now works on the 'Int' directly.
--
-- The invariant is that the position is in range, maintained by the
-- constructors: the same trade 'Data.Grid.Sized.Ordinal.Ordinal' made when it
-- stopped being a GADT, and the guard on every axis value is still paid on the
-- way in.
--
-- @(':|')@ and 'EmptyCoord' are still the interface, and still @COMPLETE@;
-- they carry the 'IsCoordList' evidence they need to divide and multiply by
-- the axis strides.
--
-- == Displacements live elsewhere
--
-- @'Diff' ('Coord' cs)@ is 'Delta' @('MapDiff' cs)@, not a @Coord@ any more: a
-- displacement is unbounded and signed and so cannot be a position. See
-- "Data.Grid.Sized.Coord.Delta", which this module re-exports.
--
-- The distinction is not pedantry, and @('Data.AffineSpace..-.')@ is not a way
-- to ask where a cell is. @c '.-.' 'zeroCoord'@ is the /route/ from the origin
-- to @c@, by the shortest signed way each axis offers, which on a
-- 'Data.Grid.Sized.Coord.Periodic.Periodic' axis of 60 makes cell 59 into
-- @-1@. To ask where a cell is, ask 'coordIndices' (or 'coordIndices2' at two
-- axes), which reports each axis's own index and never a negative number.
--
-- == Two displacement lists, one for each half of the movement table
--
-- @'MapDiff' cs@ is the affine one, and it is what @('Data.AffineSpace..+^')@,
-- @('Data.AffineSpace..-.')@ and 'transportCoord' take: those are /total/, and
-- being total is something each axis type has to license by having an
-- 'Data.AffineSpace.AffineSpace' instance.
--
-- @'MapStep' cs@ is the checked one --- one signed 'Int' per axis --- and it is
-- what 'offsetCoord', 'coordRay', 'offsetCoordUpTo' and 'Path' take. A checked
-- step needs no affine action, only a per-axis bounds check, so it is available
-- on every axis including 'Data.Grid.Sized.Ordinal.Ordinal', which has no
-- affine action to give and so is stuck under @MapDiff@. The two lists are the
-- same list wherever both reduce.
--
-- == Where the pieces live
--
-- This module is a facade. Nothing is defined here; each group below is
-- re-exported from the submodule that owns it, and importing that submodule
-- directly gets a narrower interface with the same meaning:
--
--   * "Data.Grid.Sized.Coord.Delta" --- displacements.
--   * "Data.Grid.Sized.Coord.Torus" --- the finite displacement group.
--   * "Data.Grid.Sized.Coord.Neighbourhood" --- stepping, Moore, von Neumann.
--   * "Data.Grid.Sized.Coord.Path" --- rays and ordered walks.
--   * "Data.Grid.Sized.Coord.Distance" --- Chebyshev and Manhattan.
--   * "Data.Grid.Sized.Coord.Boundary" --- edges, corners, interior.
--   * "Data.Grid.Sized.Coord.Transform" --- reflection frames, weaken\/strengthen.
--   * "Data.Grid.Sized.Coord.Centre" --- centred and punctured coordinates.
module Data.Grid.Sized.Coord
  ( -- * Coordinates
    Coord,
    unCoord,
    pattern (:|),
    pattern EmptyCoord,
    coordSplit,

    -- * Displacements
    Delta (..),
    pattern (:^),
    pattern NoDelta,
    deltaSplit,
    singleDelta,
    appendDelta,
    deltaFromTuple,
    deltaToTuple,

    -- * Building and taking apart
    singleCoord,
    appendCoord,
    coordFromTuple,
    coordToTuple,
    transposeCoord,
    zeroCoord,
    allCoord,
    coordPosition,
    coordIndices,
    coordIndices2,
    coordFromIndices,
    coordFromPosition,
    unsafeCoordFromPosition,
    coordSpaceSize,
    axisCount,

    -- * Boundaryless displacement group
    TorusCoord (..),
    torusCoordFromDelta,
    torusCoordToDelta,
    allTorusCoords,

    -- * Centred coordinates
    CentredAxis,
    centreCoord,

    -- * Punctured coordinates
    PuncturedCoord,
    puncturedToCoord,
    allPunctured,

    -- * Neighbourhoods
    offsetCoord,
    axisOffset,
    neighbours,
    mooreNeighbours,
    vonNeumannNeighbours,
    axisSteps,
    stepsWithin,

    -- * Rays
    OffGrid (..),
    offsetCoordUpTo,
    coordRay,

    -- * Paths
    Path (..),
    walkPath,
    walkPathTotal,
    pathOffset,

    -- * Distance
    axisDistance,
    axisDistances,
    coordDistance,
    coordManhattan,

    -- * Boundaries
    axisBoundary,
    axisBoundaries,
    onBoundary,
    isCorner,
    interiorCoords,

    -- * Frame transform
    axisFrameFlips,
    transportCoord,
    TransportCoordList,

    -- * Changing the size of a coord
    WeakenCoord (..),
    StrengthenCoord (..),

    -- * Type-level machinery
    Length,
    MaxCoordSize,
    MapDiff,
    MapStep,
    AffineCoordList,
    AllDiffSame,
    AllSizedKnown (..),
    SizeProof (..),
    IsCoordList,
    IsCoordLifted (..),
  )
where

import Data.Grid.Sized.Coord.Boundary
import Data.Grid.Sized.Coord.Centre
import Data.Grid.Sized.Coord.Class
import Data.Grid.Sized.Coord.Delta
import Data.Grid.Sized.Coord.Distance
import Data.Grid.Sized.Coord.Internal
import Data.Grid.Sized.Coord.Neighbourhood
import Data.Grid.Sized.Coord.Path
import Data.Grid.Sized.Coord.Torus
import Data.Grid.Sized.Coord.Transform
