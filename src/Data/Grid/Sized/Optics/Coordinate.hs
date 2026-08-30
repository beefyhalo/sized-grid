{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Optics for the things that index a grid: coordinates, the displacements
-- between them, and the ordinals an axis is built from.
--
-- The @Field1@ ... @Field5@ instances here are orphans on purpose: they attach
-- @lens@'s classes to "Data.Grid.Sized.Coord"'s types, and neither package can
-- own both sides. That is why this module carries @-Wno-orphans@ and why
-- "Data.Grid.Sized.Optics" re-exports it --- a consumer that gets the optics at
-- all gets the instances with them.
module Data.Grid.Sized.Optics.Coordinate
  ( -- * Coordinates
    _CoordAxes,
    _TransposedCoord,
    _CoordTuple,
    _CoordCons,
    _SingleCoord,
    _EmptyCoord,
    _Position,
    _Strengthened,
    _Weakened,
    _WeakenedCoord,
    translated,
    coordHead,
    coordTail,

    -- * Displacements
    _WrappedDelta,
    _DeltaTuple,
    _DeltaCons,
    deltaHead,
    deltaTail,

    -- * Ordinals
    _Ordinal,
  )
where

import Control.Lens
import Data.AdditiveGroup (AdditiveGroup (..))
import Data.AffineSpace (Diff, (.+^))
import Data.Grid.Sized.Coord
  ( AffineCoordList,
    Coord,
    MapDiff,
    StrengthenCoord,
    WeakenCoord,
    appendCoord,
    coordFromPosition,
    coordFromTuple,
    coordPosition,
    coordSplit,
    coordToTuple,
    singleCoord,
    strengthenCoord,
    transposeCoord,
    unCoord,
    unsafeCoordFromPosition,
    weakenCoord,
    pattern EmptyCoord,
  )
import Data.Grid.Sized.Coord.Class
  ( Boundaryless,
    IsCoord,
    IsCoordLifted,
    IsCoordList (..),
    coordListSize,
    strengthenIsCoord,
    toAxisIndex,
    unsafeFromAxisIndex,
    weakenIsCoord,
  )
import Data.Grid.Sized.Coord.Delta
  ( Delta (..),
    appendDelta,
    deltaFromTuple,
    deltaSplit,
    deltaToTuple,
  )
import Data.Grid.Sized.Internal.Type (requiring)
import Data.Grid.Sized.Ordinal
  ( Ordinal,
    numToOrdinal,
    ordinalToNum,
    strengthenOrdinal,
    weakenOrdinal,
  )
import GHC.TypeLits (KnownNat, type (<=))
import Generics.SOP
  ( All,
    I (..),
    IsProductType,
    NP (..),
  )

-- | An axis-wise view of a coordinate. This decodes the flat position and
-- therefore costs one 'quotRem' per axis; it is not a representation coercion.
_CoordAxes :: forall cs. (IsCoordList cs) => Iso' (Coord cs) (NP I cs)
_CoordAxes = iso unCoord (unsafeCoordFromPosition . npToPosition @cs)

-- | The coordinate transpose, which is its own inverse.
_TransposedCoord ::
  (IsCoordLifted a, IsCoordLifted b) =>
  Iso' (Coord '[a, b]) (Coord '[b, a])
_TransposedCoord = iso transposeCoord transposeCoord

_CoordTuple :: (IsProductType t xs, IsCoordList xs) => Iso' (Coord xs) t
_CoordTuple = iso coordToTuple coordFromTuple

_CoordCons ::
  (IsCoordLifted c, IsCoordList cs) =>
  Iso' (Coord (c ': cs)) (c, Coord cs)
_CoordCons = iso coordSplit (uncurry appendCoord)

_SingleCoord :: (IsCoordLifted c) => Iso' (Coord '[c]) c
_SingleCoord = iso (fst . coordSplit) singleCoord

_EmptyCoord :: Iso' (Coord '[]) ()
_EmptyCoord = iso (const ()) (const EmptyCoord)

-- | The checked conversion between a flat position and a coordinate.
_Position :: (IsCoordList cs) => Prism' Int (Coord cs)
_Position = prism' coordPosition coordFromPosition

-- | A checked change from a smaller ordinal bound to a larger one.
_Strengthened ::
  (KnownNat n, KnownNat m, n <= m) =>
  Prism' (Ordinal m) (Ordinal n)
_Strengthened = prism' strengthenOrdinal weakenOrdinal

-- | A checked change from a larger axis bound to a smaller one.
_Weakened ::
  (IsCoord c, KnownNat n, KnownNat m, n <= m) =>
  Prism' (c m) (c n)
_Weakened = prism' strengthenIsCoord weakenIsCoord

-- | A checked embedding from a coordinate list into a larger coordinate list.
_WeakenedCoord ::
  (StrengthenCoord as bs, WeakenCoord bs as) =>
  Prism' (Coord bs) (Coord as)
_WeakenedCoord = prism' strengthenCoord weakenCoord

-- | Translate a boundaryless coordinate by a displacement. The inverse moves
-- by the additive inverse, so this is total exactly when every axis is
-- 'Boundaryless'.
translated ::
  forall cs.
  ( All Boundaryless cs,
    AffineCoordList cs,
    All AdditiveGroup (MapDiff cs)
  ) =>
  Diff (Coord cs) ->
  Iso' (Coord cs) (Coord cs)
translated d = requiring @(All Boundaryless cs) $ iso (.+^ d) (.+^ negateV d)

coordHead ::
  forall a a' as.
  (IsCoordLifted a, IsCoordLifted a', IsCoordList as) =>
  Lens (Coord (a ': as)) (Coord (a' ': as)) a a'
coordHead f c =
  case coordPosition c `quotRem` stride of
    (i, r) ->
      (\a' -> unsafeCoordFromPosition (toAxisIndex a' * stride + r))
        <$> f (unsafeFromAxisIndex @a i)
  where
    stride = coordListSize @as

coordTail ::
  forall a as as'.
  (IsCoordList as, IsCoordList as') =>
  Lens (Coord (a ': as)) (Coord (a ': as')) (Coord as) (Coord as')
coordTail f c =
  case coordPosition c `quotRem` coordListSize @as of
    (i, r) ->
      ( \tailCoord ->
          unsafeCoordFromPosition
            (i * coordListSize @as' + coordPosition tailCoord)
      )
        <$> f (unsafeCoordFromPosition r)

instance
  (IsCoordLifted a, IsCoordLifted a', IsCoordList cs) =>
  Field1 (Coord (a ': cs)) (Coord (a' ': cs)) a a'
  where
  _1 = coordHead

instance
  (IsCoordLifted a, IsCoordLifted b, IsCoordLifted b', IsCoordList cs) =>
  Field2 (Coord (a ': b ': cs)) (Coord (a ': b' ': cs)) b b'
  where
  _2 = coordTail . _1

instance
  ( IsCoordLifted a,
    IsCoordLifted b,
    IsCoordLifted c,
    IsCoordLifted c',
    IsCoordList cs
  ) =>
  Field3 (Coord (a ': b ': c ': cs)) (Coord (a ': b ': c' ': cs)) c c'
  where
  _3 = coordTail . _2

instance
  ( IsCoordLifted a,
    IsCoordLifted b,
    IsCoordLifted c,
    IsCoordLifted d,
    IsCoordLifted d',
    IsCoordList cs
  ) =>
  Field4 (Coord (a ': b ': c ': d ': cs)) (Coord (a ': b ': c ': d' ': cs)) d d'
  where
  _4 = coordTail . _3

instance
  ( IsCoordLifted a,
    IsCoordLifted b,
    IsCoordLifted c,
    IsCoordLifted d,
    IsCoordLifted e,
    IsCoordLifted e',
    IsCoordList cs
  ) =>
  Field5 (Coord (a ': b ': c ': d ': e ': cs)) (Coord (a ': b ': c ': d ': e' ': cs)) e e'
  where
  _5 = coordTail . _4

_WrappedDelta :: Iso' (Delta ds) (NP I ds)
_WrappedDelta = dimap unDelta (fmap Delta)

_DeltaTuple :: (IsProductType t ds) => Iso' (Delta ds) t
_DeltaTuple = iso deltaToTuple deltaFromTuple

_DeltaCons :: Iso' (Delta (d ': ds)) (d, Delta ds)
_DeltaCons = iso deltaSplit (uncurry appendDelta)

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
