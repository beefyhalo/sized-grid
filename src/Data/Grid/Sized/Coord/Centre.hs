{-# LANGUAGE AllowAmbiguousTypes #-}

-- | The middle of a coordinate space, and the coordinates around it.
--
-- Both exist for windows: a stencil is a grid whose centre is the cell it is
-- applied to, so its offsets are named relative to 'centreCoord' and a
-- 'PuncturedCoord' is one of them with the centre itself left out.
module Data.Grid.Sized.Coord.Centre
  ( -- * Centred coordinates
    CentredAxis
  , centreCoord
    -- * Punctured coordinates
  , PuncturedCoord
  , puncturedToCoord
  , allPunctured
  ) where

import           Data.Grid.Sized.Coord.Class
import           Data.Grid.Sized.Coord.Internal
import           Data.Grid.Sized.Ordinal

import           Data.Constraint
import           Generics.SOP                (All, I (..), Proxy (..), hcpure)
import           GHC.TypeLits

-- | An axis contributing to 'centreCoord': lifted and odd-sized, via 'Data.Grid.Sized.Coord.Class.OddC'.
class (IsCoordLifted x, OddC x) => CentredAxis x

instance (IsCoordLifted x, OddC x) => CentredAxis x

-- | The middle value of a single axis: index @(n - 1) \`div\` 2@, equidistant from both ends when @n@ is odd.
centreAxis :: forall x. IsCoordLifted x => x
centreAxis = unsafeFromAxisIndex @x ((ordinalSize @(CoordNat x) - 1) `div` 2)

-- | The coordinate sitting at the middle of every axis at once.
centreCoord :: forall cs. (IsCoordList cs, All CentredAxis cs) => Coord cs
centreCoord =
    Coord $ npToPosition @cs $ hcpure (Proxy :: Proxy CentredAxis) (I centreAxis)

-- | A coordinate other than the centre of a window: a flat position, not axis-by-axis, so recovering which neighbour it is goes through 'puncturedToCoord'.
newtype PuncturedCoord cs =
    PuncturedCoord (Ordinal (MaxCoordSize cs - 1))
    deriving (Eq, Ord)

-- | Not @deriving Show@: 'Ordinal'\'s 'Show' instance needs a @KnownNat@ this module deliberately does not require of every caller.
instance Show (PuncturedCoord cs) where
    show (PuncturedCoord o) = "PuncturedCoord " ++ show (ordinalToInt o)

-- | The 'Coord' a 'PuncturedCoord' names. Total: skips over 'centreCoord'\'s own position so every 'PuncturedCoord' names a real coordinate other than the centre.
puncturedToCoord ::
       forall cs. (IsCoordList cs, All CentredAxis cs)
    => PuncturedCoord cs
    -> Coord cs
puncturedToCoord (PuncturedCoord o) =
    case coordFromPosition flat of
        Just c -> c
        Nothing ->
            error
                "Data.Grid.Sized.Coord.puncturedToCoord: impossible: a \
                \PuncturedCoord's flat index landed outside Coord's range"
  where
    k = coordPosition (centreCoord @cs)
    i = ordinalToInt o
    flat
        | i < k = i
        | otherwise = i + 1

-- | Every axis contributes at least one value, so the coordinate space is never empty. Not exported: nothing outside this module constructs a 'PuncturedCoord'.
coordSpaceNonEmpty :: forall cs. AllSizedKnown cs => Dict (1 <= MaxCoordSize cs)
coordSpaceNonEmpty =
    case cmpNat (Proxy @1) (Proxy @(MaxCoordSize cs)) of
        LTI -> Dict
        EQI -> Dict
        GTI ->
            error
                "Data.Grid.Sized.Coord.coordSpaceNonEmpty: impossible: \
                \MaxCoordSize came out below one, though every axis \
                \contributes at least one value"

-- | Every 'PuncturedCoord', in the same row-major order 'allCoord' visits, with the centre left out.
allPunctured ::
       forall cs. (IsCoordList cs, AllSizedKnown cs)
    => [PuncturedCoord cs]
allPunctured =
    case coordSpaceNonEmpty @cs of
        Dict ->
            [PuncturedCoord (unsafeOrdinal i) | i <- [0 .. coordSpaceSize @cs - 2]]
