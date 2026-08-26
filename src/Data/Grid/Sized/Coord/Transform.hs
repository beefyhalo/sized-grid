{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Maps between coordinates: along a reflecting axis, and between axis lists
-- of different sizes.
--
-- 'transportCoord' is the one operation that has to carry a heading as well as
-- a position, because a reflecting axis turns a walker around when it bounces;
-- 'WeakenCoord' and 'StrengthenCoord' change the shape rather than the point.
module Data.Grid.Sized.Coord.Transform
  ( -- * Frame transform
    axisFrameFlips
  , transportCoord
  , TransportCoordList(..)
    -- * Changing the size of a coord
  , WeakenCoord(..)
  , StrengthenCoord(..)
  ) where

import           Data.Grid.Sized.Coord.Class
import           Data.Grid.Sized.Coord.Delta
import           Data.Grid.Sized.Coord.Internal

import           Data.AffineSpace
import           Generics.SOP                (All, I (..), NP (..))
import           GHC.TypeLits

-- | A separate class from 'AffineCoordList': the fold needs both '.+^' and
-- 'Data.Grid.Sized.Coord.Class.axisFrameFlipsIsCoord' obligations at once,
-- which no single existing class states.
class (AffineCoordList cs, All IsCoordLifted cs) => TransportCoordList cs where
    posTransport ::
           AllDiffSame Int cs
        => Int
        -> NP I (MapDiff cs)
        -> (Int, NP I (MapDiff cs))

instance TransportCoordList '[] where
    posTransport p Nil = (p, Nil)
    {-# INLINE posTransport #-}

instance (AffineSpace x, IsCoordLifted x, TransportCoordList xs) =>
         TransportCoordList (x ': xs) where
    -- Match the displacement, as in 'posAdd', so MapDiff reduces first.
    posTransport p (I d :* ds) =
        case p `quotRem` stride of
            (i, r) ->
                case posTransport @xs r ds of
                    (r', ds') ->
                        ( toAxisIndex (x .+^ d) * stride + r'
                        , I (if axisFrameFlipsIsCoord x d
                                 then negate d
                                 else d) :*
                          ds')
              where
                x = unsafeFromAxisIndex @x i
      where
        stride = coordListSize @xs
    {-# INLINE posTransport #-}

-- | Whether stepping this axis by this displacement reverses its own sense of direction. 'False' for every axis type except 'Data.Grid.Sized.Coord.Reflective.Reflective' and 'Data.Grid.Sized.Coord.Reflect101.Reflect101'.
axisFrameFlips :: forall x. IsCoordLifted x => x -> Int -> Bool
axisFrameFlips = axisFrameFlipsIsCoord @(CoordContainer x) @(CoordNat x)

-- | Move a coordinate by a heading, and report the heading a walker facing it would have after the step.
transportCoord ::
       forall cs. (TransportCoordList cs, AllDiffSame Int cs)
    => Coord cs
    -> Diff (Coord cs)
    -> (Coord cs, Diff (Coord cs))
transportCoord (Coord c) (Delta d) =
    case posTransport @cs c d of
        (c', d') -> (Coord c', Delta d')


class WeakenCoord as bs where
  weakenCoord :: Coord as -> Maybe (Coord bs)

instance WeakenCoord '[] '[] where
  weakenCoord = Just

instance ( WeakenCoord as bs
         , IsCoordLifted (c n)
         , IsCoordLifted (c m)
         , IsCoordList as
         , IsCoordList bs
         ) =>
         WeakenCoord (c n ': as) (c m ': bs) where
    weakenCoord (a :| as) = do
        bs <- weakenCoord as
        b <- weakenIsCoord a
        return (b :| bs)

class StrengthenCoord as bs where
  strengthenCoord :: Coord as -> Coord bs

instance StrengthenCoord '[] '[] where
  strengthenCoord c = c

instance ( StrengthenCoord as bs
         , IsCoordLifted (c n)
         , IsCoordLifted (c m)
         , IsCoordList as
         , IsCoordList bs
         , n <= m
         ) =>
         StrengthenCoord (c n ': as) (c m ': bs) where
  strengthenCoord (a :| as) = strengthenIsCoord a :| strengthenCoord as
