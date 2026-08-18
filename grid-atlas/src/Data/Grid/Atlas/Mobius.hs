{-# LANGUAGE DataKinds #-}

-- | A Mobius strip: a single-chart 'Atlas' glued to itself along its
-- 'Wrapped' axis, reflecting 'Straight' on crossing.
module Data.Grid.Atlas.Mobius
  ( Axis(..)
  , Heading(..)
  , mobiusAtlas
  , mobiusSeam
  , mobiusStep
  ) where

import           Data.Atlas.Topology.Seam (SeamTable (..), crossSeam)
import           Data.Grid.Atlas
import           Data.Grid.Sized

import           Data.Maybe  (fromMaybe)
import qualified Data.Vector as V
import           GHC.TypeLits

data Axis
    = Wrapped
    | Straight
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

mobiusAtlas ::
       forall w h a. Grid '[ Clamped w, Clamped h] a
    -> Atlas '[ Clamped w, Clamped h] 1 a
mobiusAtlas g =
    fromMaybe (error "mobiusAtlas: impossible, one chart always matches k = 1") $
    atlasFromVector (V.singleton g)

mobiusSeam :: SeamTable () (Axis, Extremum)
mobiusSeam = SeamTable crossMobiusEdge

crossMobiusEdge :: () -> (Axis, Extremum) -> ((), (Axis, Extremum), Bool)
crossMobiusEdge () (Wrapped, AtMin) = ((), (Wrapped, AtMax), True)
crossMobiusEdge () (Wrapped, AtMax) = ((), (Wrapped, AtMin), True)
crossMobiusEdge () (Straight, side) = ((), (Straight, side), False)

-- | 'Nothing' on a 'Straight' step off the edge: unlike a cube, this axis
-- has a genuine 'Clamped' boundary rather than another seam to resolve.
mobiusStep ::
       forall w h. (KnownNat w, KnownNat h)
    => AtlasCoord '[ Clamped w, Clamped h] 1
    -> Heading
    -> Maybe (AtlasCoord '[ Clamped w, Clamped h] 1, Heading)
mobiusStep (chart, u :| v :| EmptyCoord) (Heading axis side) =
    let wSize = ordinalSize @w
        hSize = ordinalSize @h
        ui = ordinalToInt (unClamped u)
        vi = ordinalToInt (unClamped v)
        d = sideSign side
        (ui', vi') =
            case axis of
                Wrapped  -> (ui + d, vi)
                Straight -> (ui, vi + d)
    in if ui' >= 0 && ui' < wSize && vi' >= 0 && vi' < hSize
           then Just
                    ( ( chart
                      , Clamped (unsafeOrdinal ui') :| Clamped (unsafeOrdinal vi') :|
                        EmptyCoord)
                    , Heading axis side)
           else
               case axis of
                   Straight -> Nothing
                   Wrapped ->
                       let (_, (_, destSide), reversed) =
                               crossSeam mobiusSeam () (axis, side)
                           vFixed
                               | reversed = hSize - 1 - vi
                               | otherwise = vi
                           uFixed =
                               case destSide of
                                   AtMin -> 0
                                   AtMax -> wSize - 1
                           newSign = negate (sideSign destSide)
                       in Just
                              ( ( chart
                                , Clamped (unsafeOrdinal uFixed) :|
                                  Clamped (unsafeOrdinal vFixed) :|
                                  EmptyCoord)
                              , Heading Wrapped (signSide newSign))
