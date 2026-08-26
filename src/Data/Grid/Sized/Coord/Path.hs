-- | Walks: a step repeated, and an ordered sequence of steps.
--
-- Both go through 'offsetCoord' one step at a time rather than summing their
-- displacements first, because a boundary policy need not be separable: a
-- route that leaves the grid and comes back is not the same as its net
-- displacement. 'pathOffset' and 'walkPathTotal' are the two places that
-- distinction provably does not arise.
module Data.Grid.Sized.Coord.Path
  ( -- * Rays
    OffGrid(..)
  , offsetCoordUpTo
  , coordRay
    -- * Paths
  , Path(..)
  , walkPath
  , walkPathTotal
  , pathOffset
  ) where

import           Data.Grid.Sized.Coord.Class
import           Data.Grid.Sized.Coord.Internal
import           Data.Grid.Sized.Coord.Neighbourhood (offsetCoord)
import           Data.Grid.Sized.Internal.Type       (requiring)

import           Control.Monad               (foldM)
import           Data.AdditiveGroup
import           Data.AffineSpace
import           Data.List                   (unfoldr)
import           Generics.SOP                (All)
import           GHC.Generics                (Generic)

-- | Where a walk left the grid: the last coordinate still on it, and how many whole steps it took to get there.
data OffGrid cs = OffGrid
    { lastInside :: Coord cs
    , stepsTaken :: Int
  } deriving stock (Generic, Eq)

deriving stock instance (IsCoordList cs, All Show cs) => Show (OffGrid cs)

-- | Take up to @n@ steps of @d@ from @c@: 'Right' the coordinate @n@ steps away, or 'Left' how far the walk got before the grid ran out.
offsetCoordUpTo ::
       ( IsCoordList cs
       , AllDiffSame Int cs
       )
    => Int
    -> Coord cs
    -> Diff (Coord cs)
    -> Either (OffGrid cs) (Coord cs)
offsetCoordUpTo n c d = go n c 0
  where
    go k x s
        | k <= 0 = Right x
        | otherwise =
            case offsetCoord x d of
                Nothing -> Left (OffGrid x s)
                Just y  -> go (k - 1) y (s + 1)

-- | The ray from @c@ in direction @d@, not including @c@ itself; infinite on a torus or with a zero displacement.
coordRay ::
       ( IsCoordList cs
       , AllDiffSame Int cs
       )
    => Coord cs
    -> Diff (Coord cs)
    -> [Coord cs]
coordRay c d = unfoldr (\x -> (\y -> (y, y)) <$> offsetCoord x d) c

-- | An ordered sequence of displacements, kept separate rather than summed into one 'Diff': only matters where a boundary policy is not separable per axis.
newtype Path cs = Path
    { pathSteps :: [Diff (Coord cs)]
  }

deriving newtype instance Eq (Diff (Coord cs)) => Eq (Path cs)

deriving newtype instance Show (Diff (Coord cs)) => Show (Path cs)

instance Semigroup (Path cs) where
    Path a <> Path b = Path (a <> b)

instance Monoid (Path cs) where
    mempty = Path []

-- | Walk a 'Path' one step at a time through 'offsetCoord', stopping with 'Nothing' as soon as a step would leave the grid, so a route can fail even when its steps cancel out net.
walkPath ::
       ( IsCoordList cs
       , AllDiffSame Int cs
       )
    => Coord cs
    -> Path cs
    -> Maybe (Coord cs)
walkPath c (Path ds) = foldM offsetCoord c ds

-- | The single displacement a 'Path'\'s steps sum to, forgetting their order.
pathOffset :: All AdditiveGroup (MapDiff cs) => Path cs -> Diff (Coord cs)
pathOffset (Path ds) = foldl' (^+^) zeroV ds

-- | The total counterpart of 'walkPath': on a coord where every axis is
-- 'Boundaryless', a step can never leave the grid, so there is no 'Maybe'
-- for the caller to discharge. Agrees with 'walkPath' wherever both
-- typecheck: @walkPath c p == Just (walkPathTotal c p)@.
walkPathTotal ::
       forall cs.
       ( All Boundaryless cs
       , AffineCoordList cs
       , All AdditiveGroup (MapDiff cs)
       )
    => Coord cs
    -> Path cs
    -> Coord cs
walkPathTotal c p = requiring @(All Boundaryless cs) $ c .+^ pathOffset p
