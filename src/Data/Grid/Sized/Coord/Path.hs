-- | Walks: a step repeated, and an ordered sequence of steps.
--
-- Both go through 'offsetCoord' one step at a time rather than summing their
-- displacements first, because a boundary policy need not be separable: a
-- route that leaves the grid and comes back is not the same as its net
-- displacement. 'pathOffset' and 'walkPathTotal' are the two places that
-- distinction provably does not arise.
module Data.Grid.Sized.Coord.Path
  ( -- * Rays
    OffGrid (..),
    offsetCoordUpTo,
    coordRay,

    -- * Paths
    Path (..),
    walkPath,
    walkPathTotal,
    pathOffset,
  )
where

import Control.Monad (foldM)
import Data.AdditiveGroup
import Data.AffineSpace
import Data.Grid.Sized.Coord.Class
import Data.Grid.Sized.Coord.Delta (Delta)
import Data.Grid.Sized.Coord.Internal
import Data.Grid.Sized.Coord.Neighbourhood (offsetCoord)
import Data.Grid.Sized.Internal.Type (requiring)
import Data.List (unfoldr)
import GHC.Generics (Generic)
import Generics.SOP (All)

-- | Where a walk left the grid: the last coordinate still on it, and how many whole steps it took to get there.
data OffGrid cs = OffGrid
  { lastInside :: Coord cs,
    stepsTaken :: Int
  }
  deriving stock (Generic, Eq)

deriving stock instance (IsCoordList cs, All Show cs) => Show (OffGrid cs)

-- | Take up to @n@ steps of @d@ from @c@: 'Right' the coordinate @n@ steps away, or 'Left' how far the walk got before the grid ran out.
offsetCoordUpTo ::
  (IsCoordList cs) =>
  Int ->
  Coord cs ->
  Delta (MapStep cs) ->
  Either (OffGrid cs) (Coord cs)
offsetCoordUpTo n c d = go n c 0
  where
    go k x s
      | k <= 0 = Right x
      | otherwise =
          case offsetCoord x d of
            Nothing -> Left (OffGrid x s)
            Just y -> go (k - 1) y (s + 1)

-- | The ray from @c@ in direction @d@, not including @c@ itself; infinite on a torus or with a zero displacement.
coordRay ::
  (IsCoordList cs) =>
  Coord cs ->
  Delta (MapStep cs) ->
  [Coord cs]
coordRay c d = unfoldr (\x -> (\y -> (y, y)) <$> offsetCoord x d) c

-- | An ordered sequence of steps, kept separate rather than summed into one
-- displacement: only matters where a boundary policy is not separable per
-- axis.
--
-- Its steps are @'Delta' ('MapStep' cs)@ for the reason 'offsetCoord'\'s
-- displacement is: a path is walked by the checked step, so it must exist
-- wherever the checked step does -- an 'Data.Grid.Sized.Ordinal.Ordinal' axis
-- included.
newtype Path cs = Path
  { pathSteps :: [Delta (MapStep cs)]
  }

deriving newtype instance (Eq (Delta (MapStep cs))) => Eq (Path cs)

deriving newtype instance (Show (Delta (MapStep cs))) => Show (Path cs)

instance Semigroup (Path cs) where
  Path a <> Path b = Path (a <> b)

instance Monoid (Path cs) where
  mempty = Path []

-- | Walk a 'Path' one step at a time through 'offsetCoord', stopping with 'Nothing' as soon as a step would leave the grid, so a route can fail even when its steps cancel out net.
walkPath ::
  (IsCoordList cs) =>
  Coord cs ->
  Path cs ->
  Maybe (Coord cs)
walkPath c (Path ds) = foldM offsetCoord c ds

-- | The single displacement a 'Path'\'s steps sum to, forgetting their order.
pathOffset :: (All AdditiveGroup (MapStep cs)) => Path cs -> Delta (MapStep cs)
pathOffset (Path ds) = foldl' (^+^) zeroV ds

-- | The total counterpart of 'walkPath': on a coord where every axis is
-- 'Boundaryless', a step can never leave the grid, so there is no 'Maybe'
-- for the caller to discharge. Agrees with 'walkPath' wherever both
-- typecheck: @walkPath c p == Just (walkPathTotal c p)@.
-- The @'MapDiff' cs ~ 'MapStep' cs@ is the seam between the two halves of the
-- movement table: a 'Path' is a sequence of /checked/ steps, and @('.+^')@
-- takes an /affine/ displacement. The equality holds at every axis list in
-- this library -- both sides are a list of 'Int' -- and reduces to nothing at
-- a concrete one, but it has to be said, because at an abstract @cs@ neither
-- family reduces and GHC cannot see it. It is not a further restriction in
-- practice: @'All' 'Boundaryless' cs@ already excludes the only axis type
-- that makes the two differ.
walkPathTotal ::
  forall cs.
  ( All Boundaryless cs,
    AffineCoordList cs,
    All AdditiveGroup (MapStep cs),
    MapDiff cs ~ MapStep cs
  ) =>
  Coord cs ->
  Path cs ->
  Coord cs
walkPathTotal c p = requiring @(All Boundaryless cs) $ c .+^ pathOffset p
