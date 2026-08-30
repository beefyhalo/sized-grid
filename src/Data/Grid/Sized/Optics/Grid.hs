{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Optics for grids: the whole vector, the shape algebra as isomorphisms, and
-- the lenses that reach a cell, a slice or a lower dimension.
--
-- Which optic each operation gets is not a matter of taste. 'lowerDim' is a
-- 'Traversal' because the sub-grids partition the source; 'axisFold' is only a
-- 'Fold' because offering write-back would make it retain every fibre. The
-- Haddock on each says which fact it rests on.
module Data.Grid.Sized.Optics.Grid
  ( _GridVector,
    permuted,
    _Transposed,
    _SplitGrid,
    _SplitHigherDim,
    _CollapsedGrid,
    cell,
    gridIndex,
    asGrid,
    slice,
    prefix,
    suffix,
    lowerDim,
    axisFold,
  )
where

import Control.Lens
import Data.Grid.Sized.Class (IsGrid (..))
import Data.Grid.Sized.Coord (AllSizedKnown, Coord, MaxCoordSize, unsafeCoordFromPosition)
import Data.Grid.Sized.Coord.Class (CoordNat, IsCoord, IsCoordList)
import Data.Grid.Sized.Internal.Grid
  ( CollapseGrid,
    Grid,
    GridOf (..),
    MapAxis (..),
    axisFibres,
    cellLens,
    collapseGrid,
    combineGrid,
    combineHigherDim,
    dropGrid,
    gridFromList,
    gridFromVector,
    gridVector,
    mapLowerDim,
    permuteGrid,
    sliceGrid,
    splitGrid,
    splitHigherDim,
  )
import Data.Grid.Sized.Internal.Type (requiring)
import Data.Grid.Sized.Optics.Coordinate (_TransposedCoord)
import Data.Grid.Sized.Ordinal (Ordinal)
import Data.Proxy (Proxy (..))
import Data.Vector.Generic (Vector)
import Data.Vector.Generic qualified as VG
import GHC.TypeLits
  ( KnownNat,
    natVal,
    type (+),
    type (-),
    type (<=),
    type Div,
  )

-- | Lift a bijective coordinate optic to a grid permutation. Each direction
-- builds its own index vector when applied.
permuted ::
  (VG.Vector v a, VG.Vector v Int, IsCoordList cs, IsCoordList ds) =>
  Iso' (Coord cs) (Coord ds) ->
  Iso' (GridOf v ds a) (GridOf v cs a)
permuted i = iso (permuteGrid (view i)) (permuteGrid (review i))

-- | Transpose a two-dimensional grid as an 'Iso'.
_Transposed ::
  ( VG.Vector v a,
    VG.Vector v Int,
    IsCoord h,
    IsCoord w,
    KnownNat x,
    KnownNat y,
    1 <= y,
    1 <= x
  ) =>
  Iso' (GridOf v '[w x, h y] a) (GridOf v '[h y, w x] a)
_Transposed = permuted _TransposedCoord

_GridVector ::
  (VG.Vector v a, AllSizedKnown cs) =>
  Prism' (v a) (GridOf v cs a)
_GridVector =
  prism
    gridVector
    (\v -> maybe (Left v) Right (gridFromVector v))

_SplitGrid ::
  forall v c cs a.
  (Vector v a, AllSizedKnown cs) =>
  Iso' (GridOf v (c ': cs) a) (Grid '[c] (GridOf v cs a))
_SplitGrid = iso splitGrid combineGrid

-- | The outermost axis is 'Ordinal' on all three sides, and that is the only
-- place this is an isomorphism at all.
--
-- 'splitHigherDim' is a restriction, so it destroys the boundary policy: its
-- two halves come back 'Ordinal'-axed whatever the source's axis was
-- (sized-grid-pnws). 'combineHigherDim' is a construction, and takes the
-- result's policy from the halves it is given, so gluing two 'Ordinal' runs
-- yields an 'Ordinal' axis and not the @Periodic 9@ they may have been cut
-- from. The round trip therefore returns to its starting /type/ exactly when
-- that type had no policy to lose.
--
-- Stated at a general @c x@ this would be an @'Iso'' @ claiming a round trip
-- the library does not have: the cells return, the wrap does not. A caller
-- windowing a grid with a real boundary policy uses 'splitHigherDim' itself,
-- which says in its result type what was lost.
_SplitHigherDim ::
  forall v as x y a.
  (VG.Vector v a, KnownNat y, y <= x, AllSizedKnown as) =>
  Iso'
    (GridOf v (Ordinal x ': as) a)
    (GridOf v (Ordinal y ': as) a, GridOf v (Ordinal (x - y) ': as) a)
_SplitHigherDim = requiring @(y <= x) $ iso splitHigherDim (uncurry combineHigherDim)

_CollapsedGrid ::
  (VG.Vector v a, AllSizedKnown cs) =>
  Prism' (CollapseGrid cs a) (GridOf v cs a)
_CollapsedGrid = prism' collapseGrid gridFromList

-- | The cell at a coordinate. @'Data.Grid.Sized.Internal.Grid.cellLens'@ under
-- the optics name, which is also what @'ix'@ is; the two used to be the same
-- body written twice.
--
-- The @IsCoordList cs@ is not needed (see @cellLens@) and is kept only because
-- it is the published signature; `requiring` is what stops it reading as a
-- redundant constraint. Both arguments are named rather than left implicit
-- because `requiring` takes a monotype and `Lens'` is not one.
cell :: forall v cs a. (Vector v a, IsCoordList cs) => Coord cs -> Lens' (GridOf v cs a) a
cell c f = requiring @(IsCoordList cs) (cellLens c f)
{-# INLINE cell #-}

-- | The window at @off@, read or replaced.
--
-- The focus is 'Ordinal'-axed whatever the source's axis was, per
-- 'Data.Grid.Sized.Internal.Grid.Shape.sliceGrid'. That is right in both
-- directions, which is what made it worth checking rather than assuming:
-- reading gives a run of cells whose ends are cuts and not edges, and writing
-- takes @len@ cells to splice in, whose own policy the splice never consults
-- --- so demanding the replacement carry the source's policy would be
-- demanding the caller invent one.
slice ::
  forall v m c x.
  forall off len ->
  (Vector v x, KnownNat off, KnownNat len, off + len <= m) =>
  Lens' (GridOf v '[c m] x) (GridOf v '[Ordinal len] x)
slice off len =
  lens
    (sliceGrid @v @m @c @x off len)
    ( \(Grid source) (Grid replacement) ->
        Grid $
          VG.take (fromIntegral $ natVal (Proxy @off)) source
            VG.++ replacement
            VG.++ VG.drop
              ( fromIntegral (natVal (Proxy @off))
                  + fromIntegral (natVal (Proxy @len))
              )
              source
    )
{-# INLINEABLE slice #-}

prefix ::
  forall v m c x.
  forall n ->
  (Vector v x, KnownNat n, n <= m) =>
  Lens' (GridOf v '[c m] x) (GridOf v '[Ordinal n] x)
prefix n = slice @v @m @c @x 0 n
{-# INLINE prefix #-}

suffix ::
  forall v m c x.
  forall n ->
  (Vector v x, KnownNat n, n <= m) =>
  Lens' (GridOf v '[c m] x) (GridOf v '[Ordinal (m - n)] x)
suffix n =
  lens
    (dropGrid n)
    ( \(Grid source) (Grid replacement) ->
        Grid $ VG.take (fromIntegral (natVal (Proxy @n))) source VG.++ replacement
    )
{-# INLINE suffix #-}

lowerDim ::
  forall v x y as bs c.
  (Vector v x, Vector v y, AllSizedKnown as) =>
  IndexedTraversal
    (Coord '[Ordinal (CoordNat c)])
    (GridOf v (c ': as) x)
    (GridOf v (c ': bs) y)
    (GridOf v as x)
    (GridOf v bs y)
lowerDim =
  reindexed (unsafeCoordFromPosition @'[Ordinal (CoordNat c)])
    . indexing
    $ traversal mapLowerDim

-- | Read the fibres along one named axis without offering write-back. The
-- index is the policy-free ordinal position in the enumeration of fibres.
--
-- The fibres are disjoint, so a 'Traversal' could provide the same read
-- direction, but its applicative write path would have to retain every
-- transformed fibre before scattering them. A 'Fold' keeps the useful
-- read-only direction lazy and retains at most the foci demanded by its
-- consumer.
axisFold ::
  forall v cs a c.
  forall n ->
  (MapAxis n cs c, VG.Vector v a) =>
  IndexedFold
    (Coord '[Ordinal (MaxCoordSize cs `Div` CoordNat c)])
    (GridOf v cs a)
    (GridOf v '[c] a)
axisFold n =
  reindexed (unsafeCoordFromPosition @'[Ordinal (MaxCoordSize cs `Div` CoordNat c)])
    . indexing
    $ folding (axisFibres n)
{-# INLINEABLE axisFold #-}
