-- | Stepping a coordinate, and the sets of coordinates reachable in a bounded
-- number of steps.
--
-- Every function here is a wrapper over one of the per-axis folds
-- 'Data.Grid.Sized.Coord.Class.IsCoordList' supplies, which is what keeps them
-- working on the flat position rather than rebuilding a coordinate per axis.
module Data.Grid.Sized.Coord.Neighbourhood
  ( offsetCoord,
    stepsWithin,
    neighbours,
    mooreNeighbours,
    vonNeumannNeighbours,
  )
where

import Data.Grid.Sized.Coord.Class
import Data.Grid.Sized.Coord.Delta
import Data.Grid.Sized.Coord.Internal

-- | The checked counterpart of 'Data.AffineSpace..+^': succeeds only if every axis's own
-- boundary policy allows the step, so a torus axis can wrap while a bounded
-- axis in the same coord refuses.
--
-- The displacement is a @'Delta' ('MapStep' cs)@ -- one signed step count per
-- axis -- and not the @'Data.AffineSpace.Diff' ('Coord' cs)@ an affine move
-- takes. Asking for a 'Diff' asked every axis for an affine action this
-- function never uses, which shut out the one axis type that has none:
-- @offsetCoord@ now works on 'Data.Grid.Sized.Ordinal.Ordinal', and so inside
-- a window (sized-grid-i0ob.2). At every axis list where both types exist
-- they are the same type, so a call site cannot tell the difference.
offsetCoord ::
  forall cs.
  (IsCoordList cs) =>
  Coord cs ->
  Delta (MapStep cs) ->
  Maybe (Coord cs)
offsetCoord (Coord p) (Delta d) = Coord <$> posOffset @cs p d

-- | Every coordinate within @r@ steps on each axis, paired with its total step count; the centre is the only entry with total zero.
stepsWithin ::
  forall cs.
  (IsCoordList cs) =>
  Int ->
  Coord cs ->
  [(Int, Coord cs)]
stepsWithin r (Coord p) = fmap Coord <$> posStepsWithin @cs r p
{-# INLINE stepsWithin #-}

-- | The Moore neighbourhood: every coordinate within @r@ steps on each axis independently, excluding the centre.
--
-- Reads 'posStepsWithin' directly rather than going through 'stepsWithin'.
-- The two differ by one intermediate list -- 'stepsWithin' has to rebuild
-- every @(steps, position)@ pair as a @(steps, 'Coord')@ one to honour its own
-- type, and this then drops the steps again. Worth 995 us \/ 8.0 MB to
-- 648 us \/ 2.6 MB on the 50x50 neighbour sweep -- the difference between
-- half of the allocation win sized-grid-adr.8 measured the ceiling at and all
-- of it (adr.8: 16.4 MB to 2.6 MB, 2.6x; this reaches 2.58x).
mooreNeighbours :: forall cs. (IsCoordList cs) => Int -> Coord cs -> [Coord cs]
mooreNeighbours r (Coord p) =
  [Coord n | (s, n) <- posStepsWithin @cs r p, s > 0]
{-# INLINE mooreNeighbours #-}

-- | The von Neumann neighbourhood: coordinates whose per-axis distances sum to at most @r@, excluding the centre.
vonNeumannNeighbours :: forall cs. (IsCoordList cs) => Int -> Coord cs -> [Coord cs]
vonNeumannNeighbours r (Coord p) =
  [Coord n | (s, n) <- posStepsWithin @cs r p, s > 0, s <= r]
{-# INLINE vonNeumannNeighbours #-}

-- | 'mooreNeighbours' at radius one: the surrounding cells, diagonals included.
neighbours :: (IsCoordList cs) => Coord cs -> [Coord cs]
neighbours = mooreNeighbours 1
{-# INLINE neighbours #-}
