-- | Coordinates read as the elements of a finite displacement group.
--
-- A 'Coord' is a position and so has no group structure of its own on a
-- bounded axis. When every axis is 'Boundaryless' it does, and 'TorusCoord' is
-- the newtype that says so: the residues of 'Delta' modulo the shape.
module Data.Grid.Sized.Coord.Torus
  ( TorusCoord (..),
    torusCoordFromDelta,
    torusCoordToDelta,
    allTorusCoords,
  )
where

import Control.DeepSeq (NFData)
import Data.AdditiveGroup
import Data.AffineSpace
import Data.Finitary (Finitary (..))
import Data.Grid.Sized.Coord.Class
import Data.Grid.Sized.Coord.Delta
import Data.Grid.Sized.Coord.Internal
import Data.Group (Abelian, Group (..))
import Data.Hashable (Hashable (..))
import Data.Kind (Type)
import Data.Universe.Class (universe, universeF)
import Data.Universe.Class qualified as U
import Generics.SOP (All)

-- | A displacement modulo the shape of an all-'Boundaryless' coordinate.
-- Unlike 'Delta', this is bounded: its values are the elements of the finite
-- product group represented by the coordinate axes.
newtype TorusCoord (cs :: [Type]) = TorusCoord (Coord cs)
  deriving newtype (Eq, Ord, NFData)

type role TorusCoord nominal

instance (IsCoordList cs, All Show cs) => Show (TorusCoord cs) where
  show (TorusCoord c) = "TorusCoord " ++ show c

-- | The four instances that are exactly 'Coord'\'s, unwrapped and rewrapped.
-- Each was written out as that delegation, with the same context the instance
-- it delegated to has, so @deriving newtype@ says it once and cannot drift.
--
-- Standalone rather than in the @newtype@\'s own @deriving@ clause so the
-- contexts stay written down and stay checkable against 'Coord'\'s.
--
-- What is /not/ derived is the point of the type, and none of it should be:
--
--   * 'Bounded' below deliberately means position @0 .. size - 1@, the
--     residues, where 'Coord'\'s means each axis's own @minBound@\/@maxBound@.
--   * 'AdditiveGroup', 'Group' and 'Abelian' deliberately narrow to
--     @All 'Boundaryless' cs@ -- that narrowing is the whole reason
--     'TorusCoord' exists.
--   * 'Show' prints the wrapper, and 'U.Universe', 'U.Finite' and 'Finitary'
--     are their own.
deriving newtype instance (IsCoordList cs) => Hashable (TorusCoord cs)

deriving newtype instance (IsCoordList cs) => Enum (TorusCoord cs)

deriving newtype instance
  (IsCoordList cs, All Semigroup cs) =>
  Semigroup (TorusCoord cs)

deriving newtype instance
  (IsCoordList cs, All Semigroup cs, All Monoid cs) =>
  Monoid (TorusCoord cs)

-- | The residues @0 .. size - 1@, not 'Coord'\'s per-axis extremes: on a
-- torus every position is a group element and the bounds are the range of the
-- flat position.
instance (IsCoordList cs) => Bounded (TorusCoord cs) where
  minBound = TorusCoord (Coord 0)
  maxBound = TorusCoord (Coord (coordListSize @cs - 1))

instance (IsCoordList cs) => U.Universe (TorusCoord cs) where
  universe = allTorusCoords

instance (IsCoordList cs) => U.Finite (TorusCoord cs) where
  universeF = allTorusCoords

instance (IsCoordList cs, AllSizedKnown cs) => Finitary (TorusCoord cs) where
  type Cardinality (TorusCoord cs) = MaxCoordSize cs
  toFinite (TorusCoord c) = fromIntegral (coordPosition c)
  fromFinite = TorusCoord . unsafeCoordFromPosition . fromIntegral

-- | Convert an unbounded displacement to its residue in the finite torus.
torusCoordFromDelta ::
  forall cs.
  ( AffineCoordList cs,
    All AdditiveGroup (MapDiff cs)
  ) =>
  Delta (MapDiff cs) ->
  TorusCoord cs
torusCoordFromDelta d = TorusCoord (zeroCoord @cs .+^ d)

-- | Recover the representative displacement from the zero coordinate.
torusCoordToDelta ::
  forall cs.
  ( AffineCoordList cs,
    All AdditiveGroup (MapDiff cs)
  ) =>
  TorusCoord cs ->
  Delta (MapDiff cs)
torusCoordToDelta (TorusCoord c) = c .-. zeroCoord @cs

-- | Every residue in row-major order.
allTorusCoords :: forall cs. (IsCoordList cs) => [TorusCoord cs]
allTorusCoords = TorusCoord <$> allCoord @cs

instance
  ( IsCoordList cs,
    All AdditiveGroup cs,
    All Boundaryless cs
  ) =>
  AdditiveGroup (TorusCoord cs)
  where
  zeroV = TorusCoord (zeroV :: Coord cs)
  TorusCoord a ^+^ TorusCoord b = TorusCoord (a ^+^ b)
  negateV (TorusCoord a) = TorusCoord (negateV a)
  TorusCoord a ^-^ TorusCoord b = TorusCoord (a ^-^ b)

instance
  ( IsCoordList cs,
    All AdditiveGroup cs,
    All Semigroup cs,
    All Monoid cs,
    All Group cs,
    All Boundaryless cs,
    Monoid (TorusCoord cs)
  ) =>
  Group (TorusCoord cs)
  where
  invert (TorusCoord a) = TorusCoord (invert a)

instance
  ( IsCoordList cs,
    All AdditiveGroup cs,
    All Semigroup cs,
    All Monoid cs,
    All Group cs,
    All Boundaryless cs,
    All Abelian cs
  ) =>
  Abelian (TorusCoord cs)
