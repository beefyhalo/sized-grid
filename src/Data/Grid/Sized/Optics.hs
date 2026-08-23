{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The optics for sized coordinates, grids, and focused grids.
module Data.Grid.Sized.Optics
  ( -- * Coordinates
    _WrappedCoord
  , coordHead
  , coordTail
  , _WrappedDelta
  , deltaHead
  , deltaTail
    -- * Ordinals
  , _Ordinal
    -- * Grids
  , _SplitGrid
  , cell
  , gridIndex
  , asGrid
  , slice
  , prefix
  , suffix
  , lowerDim
    -- * Focused grids
  , _FocusedGrid
  , focus
  , unfocused
  ) where

import           Data.Grid.Sized.Class            (IsGrid (..))
import           Data.Grid.Sized.Coord            (AllSizedKnown, Coord,
                                                   coordPosition,
                                                   unCoord,
                                                   unsafeCoordFromPosition)
import           Data.Grid.Sized.Coord.Class      (IsCoordLifted,
                                                   IsCoordList (..),
                                                   coordListSize,
                                                   toAxisIndex,
                                                   unsafeFromAxisIndex)
import           Data.Grid.Sized.Coord.Delta      (Delta (..))
import           Data.Grid.Sized.Focused          (FocusedGrid (..))
import           Data.Grid.Sized.Internal.Grid   (Grid, GridOf (..),
                                                   combineGrid, dropGrid,
                                                   mapLowerDim, splitGrid,
                                                   sliceGrid)
import           Data.Grid.Sized.Internal.Type   (requiring)
import           Data.Grid.Sized.Ordinal         (Ordinal, ordinalToNum,
                                                   numToOrdinal)

import           Control.Lens
import           Data.Vector.Generic              (Vector)
import qualified Data.Vector.Generic             as VG
import           Generics.SOP                     (I (..), NP (..))
import           Data.Proxy                       (Proxy (..))
import           GHC.TypeLits                    (KnownNat, natVal, type (+),
                                                   type (-), type (<=))

_WrappedCoord :: forall cs. IsCoordList cs => Iso' (Coord cs) (NP I cs)
_WrappedCoord = iso unCoord (unsafeCoordFromPosition . npToPosition @cs)

coordHead ::
       forall a a' as. (IsCoordLifted a, IsCoordLifted a', IsCoordList as)
    => Lens (Coord (a ': as)) (Coord (a' ': as)) a a'
coordHead f c =
    case coordPosition c `quotRem` stride of
        (i, r) ->
            (\a' -> unsafeCoordFromPosition (toAxisIndex a' * stride + r))
                <$> f (unsafeFromAxisIndex @a i)
  where
    stride = coordListSize @as

coordTail ::
       forall a as as'. (IsCoordList as, IsCoordList as')
    => Lens (Coord (a ': as)) (Coord (a ': as')) (Coord as) (Coord as')
coordTail f c =
    case coordPosition c `quotRem` coordListSize @as of
        (i, r) ->
            (\tailCoord -> unsafeCoordFromPosition
                (i * coordListSize @as' + coordPosition tailCoord))
                <$> f (unsafeCoordFromPosition r)

instance (IsCoordLifted a, IsCoordLifted a', IsCoordList cs) =>
         Field1 (Coord (a ': cs)) (Coord (a' ': cs)) a a' where
  _1 = coordHead

instance (IsCoordLifted a, IsCoordLifted b, IsCoordLifted b', IsCoordList cs) =>
         Field2 (Coord (a ': b ': cs)) (Coord (a ': b' ': cs)) b b' where
  _2 = coordTail . _1

instance ( IsCoordLifted a, IsCoordLifted b, IsCoordLifted c,
           IsCoordLifted c', IsCoordList cs ) =>
         Field3 (Coord (a ': b ': c ': cs)) (Coord (a ': b ': c' ': cs)) c c' where
  _3 = coordTail . _2

instance ( IsCoordLifted a, IsCoordLifted b, IsCoordLifted c,
           IsCoordLifted d, IsCoordLifted d', IsCoordList cs ) =>
         Field4 (Coord (a ': b ': c ': d ': cs)) (Coord (a ': b ': c ': d' ': cs)) d d' where
  _4 = coordTail . _3

instance ( IsCoordLifted a, IsCoordLifted b, IsCoordLifted c,
           IsCoordLifted d, IsCoordLifted e, IsCoordLifted e',
           IsCoordList cs ) =>
         Field5 (Coord (a ': b ': c ': d ': e ': cs)) (Coord (a ': b ': c ': d ': e' ': cs)) e e' where
  _5 = coordTail . _4

_WrappedDelta :: Iso' (Delta ds) (NP I ds)
_WrappedDelta = dimap unDelta (fmap Delta)

deltaHead :: Lens (Delta (a ': as)) (Delta (a' ': as)) a a'
deltaHead f (Delta (I a :* as)) = (\a' -> Delta (I a' :* as)) <$> f a

deltaTail :: Lens (Delta (a ': as)) (Delta (a ': as')) (Delta as) (Delta as')
deltaTail f (Delta (a :* as)) = (\(Delta as') -> Delta (a :* as')) <$> f (Delta as)

instance Field1 (Delta (a ': ds)) (Delta (a' ': ds)) a a' where
  _1 = deltaHead

instance Field2 (Delta (a ': b ': ds)) (Delta (a ': b' ': ds)) b b' where
  _2 = deltaTail . _1

instance Field3 (Delta (a ': b ': c ': ds)) (Delta (a ': b ': c' ': ds)) c c' where
  _3 = deltaTail . _2

instance Field4 (Delta (a ': b ': c ': d ': ds)) (Delta (a ': b ': c ': d' ': ds)) d d' where
  _4 = deltaTail . _3

instance Field5 (Delta (a ': b ': c ': d ': e ': ds)) (Delta (a ': b ': c ': d ': e' ': ds)) e e' where
  _5 = deltaTail . _4

_Ordinal :: (KnownNat n, Integral a) => Prism' a (Ordinal n)
_Ordinal = prism' ordinalToNum numToOrdinal

_SplitGrid ::
  forall v c cs a. (Vector v a, AllSizedKnown cs)
  => Iso' (GridOf v (c ': cs) a) (Grid '[ c] (GridOf v cs a))
_SplitGrid = iso splitGrid combineGrid

cell :: forall v cs a. (Vector v a, IsCoordList cs) => Coord cs -> Lens' (GridOf v cs a) a
cell c = requiring @(IsCoordList cs) $ lens getter setter
  where
    position = coordPosition c
    getter (Grid v) = VG.unsafeIndex v position
    setter (Grid v) value = Grid (v VG.// [(position, value)])
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

_FocusedGrid :: Iso (FocusedGrid cs a) (FocusedGrid cs b)
                    (Grid cs a, Coord cs) (Grid cs b, Coord cs)
_FocusedGrid = iso (\(FocusedGrid g p) -> (g, p)) (uncurry FocusedGrid)

focus :: Lens' (FocusedGrid cs a) (Coord cs)
focus = _FocusedGrid . _2

unfocused :: Lens (FocusedGrid cs a) (FocusedGrid cs b) (Grid cs a) (Grid cs b)
unfocused = _FocusedGrid . _1
