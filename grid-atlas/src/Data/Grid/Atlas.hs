-- | 'gridTiles', restored as one addressable @(ChartId, Coord cs)@ structure.
module Data.Grid.Atlas
  ( Atlas
  , AtlasCoord
  , atlasFromTiles
  , atlasFromVector
  , atlasIndex
  , atlasOffsetHead
  ) where

import           Data.Grid.Sized

import           Control.Lens    (review, view)
import           Data.Functor.Rep (index)
import           Data.Kind       (Type)
import           Data.Proxy      (Proxy (..))
import qualified Data.Vector     as V
import           GHC.TypeLits

-- | The constructor is not exported: the vector holding exactly @k@ charts
-- is an invariant every other function here relies on.
newtype Atlas (cs :: [Type]) (k :: Nat) a = Atlas (V.Vector (Grid cs a))

-- | Which chart, and a position local to it.
type AtlasCoord cs k = (Ordinal k, Coord cs)

-- | The charts come back 'Ordinal'-axed, because 'gridTiles' does: a tile is
-- a proper sub-window of the source axis, and a sub-window of a periodic axis
-- is not periodic. That is what this module is for: the topology the tiling
-- destroys is restored /across/ the charts by 'atlasOffsetHead', which knows
-- which chart is next, rather than left inside each chart to make claims
-- about its own edges that the source does not support.
--
-- The chart size is given as the 'Nat' @n@ rather than as a whole axis type,
-- following 'gridTiles': there is no policy left to choose. @atlasFromTiles
-- \@(Ordinal 3)@ becomes @atlasFromTiles \@3@.
atlasFromTiles ::
       forall n big rest a.
       ( KnownNat (MaxCoordSize (Ordinal n ': rest))
       , CoordNat big `Mod` n ~ 0
       )
    => Grid (big ': rest) a
    -> Atlas (Ordinal n ': rest) (Div (CoordNat big) n) a
atlasFromTiles = Atlas . V.fromList . gridTiles @n

-- | 'Nothing' if the vector's length does not match @k@.
atlasFromVector ::
       forall cs k a. KnownNat k
    => V.Vector (Grid cs a)
    -> Maybe (Atlas cs k a)
atlasFromVector v
    | V.length v == fromIntegral (natVal (Proxy @k)) = Just (Atlas v)
    | otherwise = Nothing

-- | Read a single cell. Total: every 'AtlasCoord' names a chart and a
-- coordinate already known valid, so 'V.unsafeIndex' needs no bounds check.
atlasIndex ::
       (IsCoordList cs, AllSizedKnown cs)
    => Atlas cs k a
    -> AtlasCoord cs k
    -> a
atlasIndex (Atlas charts) (chart, c) =
    index (V.unsafeIndex charts (ordinalToInt chart)) c

-- | Step the head axis of an atlas coordinate by a signed displacement,
-- crossing into the neighbouring chart if it would leave the current one.
-- 'Nothing' at the atlas's own two ends.
atlasOffsetHead ::
       forall headAxis rest k a.
       (IsCoordLifted headAxis, IsCoordList rest, KnownNat k)
    => Atlas (headAxis ': rest) k a
    -> AtlasCoord (headAxis ': rest) k
    -> Int
    -> Maybe (AtlasCoord (headAxis ': rest) k)
atlasOffsetHead _atlas (chart, hc :| rest) d = do
    newChart <- numToOrdinal (ordinalToInt chart + chartDelta)
    pure (newChart, hc' :| rest)
  where
    size = ordinalSize @(CoordNat headAxis)
    p = ordinalToInt (view (asOrdinal @(CoordContainer headAxis) @(CoordNat headAxis)) hc)
    (chartDelta, localPos) = (p + d) `divMod` size
    hc' =
        review
            (asOrdinal @(CoordContainer headAxis) @(CoordNat headAxis))
            (unsafeOrdinal @(CoordNat headAxis) localPos)
