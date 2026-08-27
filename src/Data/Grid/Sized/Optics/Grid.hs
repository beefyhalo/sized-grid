{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Optics for grids: the whole vector, the shape algebra as isomorphisms, and
-- the lenses that reach a cell, a slice or a lower dimension.
--
-- Which optic each operation gets is not a matter of taste. 'lowerDim' is a
-- 'Traversal' because the sub-grids partition the source; 'axisFold' is only a
-- 'Fold' because offering write-back would make it retain every fibre. The
-- Haddock on each says which fact it rests on.
module Data.Grid.Sized.Optics.Grid
  ( _GridVector
  , permuted
  , _Transposed
  , _SplitGrid
  , _SplitHigherDim
  , _CollapsedGrid
  , cell
  , gridIndex
  , asGrid
  , slice
  , prefix
  , suffix
  , lowerDim
  , axisFold
  ) where

import           Data.Grid.Sized.Class             (IsGrid (..))
import           Data.Grid.Sized.Coord             (AllSizedKnown, Coord)
import           Data.Grid.Sized.Coord.Class       (IsCoord, IsCoordList)
import           Data.Grid.Sized.Internal.Grid     (CollapseGrid, Grid,
                                                    GridOf (..), MapAxis (..),
                                                    axisFibres, cellLens,
                                                    collapseGrid, combineGrid,
                                                    combineHigherDim, dropGrid,
                                                    gridFromList,
                                                    gridFromVector, gridVector,
                                                    mapLowerDim, permuteGrid,
                                                    sliceGrid, splitGrid,
                                                    splitHigherDim)
import           Data.Grid.Sized.Internal.Type     (requiring)
import           Data.Grid.Sized.Optics.Coordinate (_TransposedCoord)

import           Control.Lens
import           Data.Proxy                        (Proxy (..))
import           Data.Vector.Generic               (Vector)
import qualified Data.Vector.Generic               as VG
import           GHC.TypeLits                      (KnownNat, natVal, type (+),
                                                    type (-), type (<=))


-- | Lift a bijective coordinate optic to a grid permutation. Each direction
-- builds its own index vector when applied.
permuted :: (VG.Vector v a, VG.Vector v Int, IsCoordList cs, IsCoordList ds)
     => Iso' (Coord cs) (Coord ds)
     -> Iso' (GridOf v ds a) (GridOf v cs a)
permuted i = iso (permuteGrid (view i)) (permuteGrid (review i))

-- | Transpose a two-dimensional grid as an 'Iso'.
_Transposed :: ( VG.Vector v a
               , VG.Vector v Int
               , IsCoord h
               , IsCoord w
               , KnownNat x
               , KnownNat y
               , 1 <= y
               , 1 <= x
               )
            => Iso' (GridOf v '[w x, h y] a) (GridOf v '[h y, w x] a)
_Transposed = permuted _TransposedCoord

_GridVector :: (VG.Vector v a, AllSizedKnown cs)
      => Prism' (v a) (GridOf v cs a)
_GridVector = prism
  gridVector
  (\v -> maybe (Left v) Right (gridFromVector v))

_SplitGrid ::
  forall v c cs a. (Vector v a, AllSizedKnown cs)
  => Iso' (GridOf v (c ': cs) a) (Grid '[ c] (GridOf v cs a))
_SplitGrid = iso splitGrid combineGrid

_SplitHigherDim :: forall v c as x y a.
       (VG.Vector v a, KnownNat y, y <= x, AllSizedKnown as)
    => Iso' (GridOf v (c x ': as) a)
      (GridOf v (c y ': as) a, GridOf v (c (x - y) ': as) a)
_SplitHigherDim = requiring @(y <= x) $ iso splitHigherDim (uncurry combineHigherDim)

_CollapsedGrid :: (VG.Vector v a, AllSizedKnown cs)
         => Prism' (CollapseGrid cs a) (GridOf v cs a)
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

slice :: forall v m c x. forall off len ->
  ( Vector v x, KnownNat off, KnownNat len, off + len <= m )
     => Lens' (GridOf v '[ c m] x) (GridOf v '[ c len] x)
slice off len = lens (sliceGrid @v @m @c @x off len)
  (\(Grid source) (Grid replacement) ->
     Grid $ VG.take (fromIntegral $ natVal (Proxy @off)) source
        VG.++ replacement
        VG.++ VG.drop (fromIntegral (natVal (Proxy @off))
                       + fromIntegral (natVal (Proxy @len))) source)
{-# INLINABLE slice #-}

prefix :: forall v m c x. forall n ->
       (Vector v x, KnownNat n, n <= m)
     => Lens' (GridOf v '[ c m] x) (GridOf v '[ c n] x)
prefix n = slice @v @m @c @x 0 n
{-# INLINE prefix #-}

suffix :: forall v m c x. forall n ->
       (Vector v x, KnownNat n, n <= m)
     => Lens' (GridOf v '[ c m] x) (GridOf v '[ c (m - n)] x)
suffix n = lens (dropGrid n)
  (\(Grid source) (Grid replacement) ->
     Grid $ VG.take (fromIntegral (natVal (Proxy @n))) source VG.++ replacement)
{-# INLINE suffix #-}

lowerDim :: (Vector v x, Vector v y, AllSizedKnown as)
         => Traversal (GridOf v (c ': as) x) (GridOf v (c ': bs) y)
                      (GridOf v as x) (GridOf v bs y)
lowerDim = mapLowerDim

-- | Read the fibres along one named axis without offering write-back.
--
-- The fibres are disjoint, so a 'Traversal' could provide the same read
-- direction, but its applicative write path would have to retain every
-- transformed fibre before scattering them. A 'Fold' keeps the useful
-- read-only direction lazy and retains at most the foci demanded by its
-- consumer.
axisFold ::
     forall v cs a c. forall n -> (MapAxis n cs c, VG.Vector v a)
  => Fold (GridOf v cs a) (GridOf v '[c] a)
axisFold n = folding (axisFibres n)
{-# INLINABLE axisFold #-}
