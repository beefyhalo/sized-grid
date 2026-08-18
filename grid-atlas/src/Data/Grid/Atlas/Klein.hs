{-# LANGUAGE DataKinds #-}

-- | A Klein bottle: a single-chart 'Atlas' glued to itself along both axes,
-- 'Twisted' reflecting 'Rolled' on crossing and 'Rolled' gluing straight
-- through. Gluing both pairs with a reflection would be the projective
-- plane, not this.
module Data.Grid.Atlas.Klein
  ( Axis(..)
  , Heading(..)
  , kleinAtlas
  , kleinSeam
  , kleinStep
  ) where

import           Data.Atlas.Topology.Seam (SeamTable (..), crossSeam)
import           Data.Grid.Atlas
import           Data.Grid.Sized

import           Data.Maybe  (fromMaybe)
import qualified Data.Vector as V
import           GHC.TypeLits

data Axis
    = Twisted
    | Rolled
    deriving (Eq, Show, Enum, Bounded)

data Heading = Heading
    { headingAxis :: Axis
    , headingSide :: Extremum
    } deriving (Eq, Show)

sideSign :: Extremum -> Int
sideSign AtMin = -1
sideSign AtMax = 1

signSide :: Int -> Extremum
signSide d
    | d < 0 = AtMin
    | otherwise = AtMax

kleinAtlas ::
       forall w h a. Grid '[ Clamped w, Clamped h] a
    -> Atlas '[ Clamped w, Clamped h] 1 a
kleinAtlas g =
    fromMaybe (error "kleinAtlas: impossible, one chart always matches k = 1") $
    atlasFromVector (V.singleton g)

kleinSeam :: SeamTable () (Axis, Extremum)
kleinSeam = SeamTable crossKleinEdge

crossKleinEdge :: () -> (Axis, Extremum) -> ((), (Axis, Extremum), Bool)
crossKleinEdge () (Twisted, AtMin) = ((), (Twisted, AtMax), True)
crossKleinEdge () (Twisted, AtMax) = ((), (Twisted, AtMin), True)
crossKleinEdge () (Rolled, AtMin)  = ((), (Rolled, AtMax), False)
crossKleinEdge () (Rolled, AtMax)  = ((), (Rolled, AtMin), False)

-- | Total, unlike 'Data.Grid.Atlas.Mobius.mobiusStep': every half-edge here
-- is glued to another, so no step can leave the surface.
kleinStep ::
       forall w h. (KnownNat w, KnownNat h)
    => AtlasCoord '[ Clamped w, Clamped h] 1
    -> Heading
    -> (AtlasCoord '[ Clamped w, Clamped h] 1, Heading)
kleinStep (chart, u :| v :| EmptyCoord) (Heading axis side) =
    let wSize = ordinalSize @w
        hSize = ordinalSize @h
        ui = ordinalToInt (unClamped u)
        vi = ordinalToInt (unClamped v)
        d = sideSign side
        (ui', vi') =
            case axis of
                Twisted -> (ui + d, vi)
                Rolled  -> (ui, vi + d)
        at a b =
            ( chart
            , Clamped (unsafeOrdinal a) :| Clamped (unsafeOrdinal b) :| EmptyCoord)
    in if ui' >= 0 && ui' < wSize && vi' >= 0 && vi' < hSize
           then (at ui' vi', Heading axis side)
           else let (_, (destAxis, destSide), reversed) =
                        crossSeam kleinSeam () (axis, side)
                    -- the crossed axis lands on the far edge; the sibling is
                    -- reflected exactly when the seam reverses along-seam
                    -- direction, which is the whole difference between the
                    -- two entries of the table
                    onEdge size =
                        case destSide of
                            AtMin -> 0
                            AtMax -> size - 1
                    reflect size i
                        | reversed = size - 1 - i
                        | otherwise = i
                    (uNew, vNew) =
                        case destAxis of
                            Twisted -> (onEdge wSize, reflect hSize vi)
                            Rolled  -> (reflect wSize ui, onEdge hSize)
                in ( at uNew vNew
                   , Heading destAxis (signSide (negate (sideSign destSide))))
