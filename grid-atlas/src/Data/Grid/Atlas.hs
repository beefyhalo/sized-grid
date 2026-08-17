-- | The degenerate atlas (sized-grid-fh2's first slice): 'gridTiles', restored
-- as one addressable @(ChartId, Coord cs)@ structure instead of a disjoint
-- list.
--
-- An atlas glues charts along seams, and this one's seams are all identity:
-- crossing the boundary between adjacent tiles lands on the same position on
-- every axis except the one that was tiled, exactly the way it would if the
-- tiles had never been cut apart. That degeneracy is deliberate --- see
-- sized-grid-fh2's own framing --- and it is what the property tests in
-- @grid-atlas:tests@ check: this module is observationally equal to indexing
-- the untiled 'Grid' directly.
--
-- What is deliberately absent: heterogeneous chart shapes, a general
-- seam-transition table, and any frame transform. Nothing here needs one ---
-- a tiling never flips or permutes an axis --- and inventing the
-- representation before a second, genuinely non-separable atlas (the cube
-- map) exists to justify its shape would be speculation. See sized-grid-1bm
-- for why that second case (an axis-permuting seam) is a structurally
-- different problem from this one (a per-axis chart index).
module Data.Grid.Atlas
  ( Atlas
  , AtlasCoord
  , atlasFromTiles
  , atlasIndex
  , atlasOffsetHead
  ) where

import           Data.Grid.Sized

import           Control.Lens    (review, view)
import           Data.Functor.Rep (index)
import           Data.Kind       (Type)
import qualified Data.Vector     as V
import           GHC.TypeLits

-- | @k@ charts of shape @cs@, addressable as one structure instead of
-- 'gridTiles'\'s disjoint list. The constructor is not exported, for the same
-- reason 'Grid'\'s is not: the vector holding exactly @k@ charts is an
-- invariant 'atlasFromTiles' establishes and every other function here
-- relies on, not something worth re-checking on every read.
newtype Atlas (cs :: [Type]) (k :: Nat) a = Atlas (V.Vector (Grid cs a))

-- | A coordinate into an atlas: which chart, and a position local to it.
-- 'Ordinal' rather than a bespoke @ChartId@ newtype --- a chart index is
-- exactly a bounded index with no boundary policy of its own, which is what
-- 'Ordinal' already is, and reusing it is free: 'Eq', 'Ord', 'Enum',
-- 'Bounded' and the JSON instances all come along.
type AtlasCoord cs k = (Ordinal k, Coord cs)

-- | Build the degenerate atlas: 'gridTiles', restored as one indexed
-- structure. @k@ is fixed to @CoordNat big \`Div\` CoordNat small@, the tile
-- count 'gridTiles' itself produces but never names at the type level,
-- because nothing before this consumed it as anything but a list length.
atlasFromTiles ::
       forall small big rest a.
       ( KnownNat (MaxCoordSize (small ': rest))
       , CoordNat big `Mod` CoordNat small ~ 0
       )
    => Grid (big ': rest) a
    -> Atlas (small ': rest) (Div (CoordNat big) (CoordNat small)) a
atlasFromTiles = Atlas . V.fromList . gridTiles @small

-- | Read a single cell. Total: every 'AtlasCoord' names a chart in range (an
-- 'Ordinal k' has no other kind of value) and a coordinate valid within it
-- ('Coord' is the same guarantee), so there is nothing left to check.
atlasIndex ::
       (IsCoordList cs, AllSizedKnown cs)
    => Atlas cs k a
    -> AtlasCoord cs k
    -> a
atlasIndex (Atlas charts) (chart, c) = index (charts V.! ordinalToInt chart) c

-- | Step the head axis of an atlas coordinate by a signed displacement,
-- crossing into the neighbouring chart --- identically to the neighbouring
-- cell within a chart --- if it would leave the current one. 'Nothing' at the
-- atlas's own two ends, the same shape 'offsetIsCoord' has at a single
-- chart's edge.
--
-- Always the head axis, never a runtime-selected one, for the same reason
-- 'gridTiles' itself only ever tiles the outermost axis: 'Coord'\'s axes are
-- heterogeneous, indexed by position in the type list rather than by a
-- runtime 'Int', so there is no axis @i@ to reach for without either fixing
-- @i@ in the type (what this does) or building the runtime-indexed
-- machinery 'IsCoordList' deliberately does not have. 'zipLowerDim
-- atlasFromTiles' reaches a different axis the same way 'zipLowerDim
-- gridTiles' does.
--
-- The position arithmetic works below @headAxis@\'s own boundary policy, not
-- through it: it reads and rebuilds the raw 0..n-1 position via 'asOrdinal'
-- rather than calling 'offsetIsCoord', because the policy this function
-- implements --- carry the excess into the next chart --- is a different
-- policy than whatever @headAxis@ itself names, the same way 'gridTiles'
-- does not care what 'IsCoord' instance @small@ uses either.
atlasOffsetHead ::
       forall headAxis rest k a. (IsCoordLifted headAxis, KnownNat k)
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
