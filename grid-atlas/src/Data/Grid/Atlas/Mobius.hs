{-# LANGUAGE DataKinds #-}

-- | The Mobius strip (sized-grid-00v): a single @'Grid' '[Clamped w, Clamped
-- h] a@ chart glued to itself along its own width axis --- walking off the
-- right edge at row @y@ lands back on the left edge at row @h - 1 - y@, the
-- other axis reflected.
--
-- "Data.Grid.Atlas"\'s @'Atlas' cs k a@ never requires @k > 1@: a Mobius
-- strip is the @k = 1@ case, a chart glued to itself rather than to a
-- neighbour, the same @'Data.Atlas.Topology.Seam.SeamTable'@ shape
-- sized-grid-68j's cube map uses with 'Data.Grid.Atlas.CubeMap.Face'
-- collapsed to a single chart (@()@) and its 24-equation table collapsed to
-- the one non-identity entry a self-gluing needs.
--
-- This is the first off-diagonal seam in
-- 'Data.Grid.Sized.Coord.Class.IsCoord'\'s @Z^n\/G@ sense (crossing axis 0's
-- edge transforms axis 1): no single 'Data.Grid.Sized.Coord.Class.IsCoord'
-- instance can express it (sized-grid-3u1's separable ceiling), and
-- 'Data.Grid.Sized.Coord.Class.axisFrameFlipsIsCoord' cannot either
-- (sized-grid-o1n's axis-local case only ever transforms the heading on the
-- axis it crosses, never a sibling axis).
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

-- | Which of the chart's two axes: 'Wrapped' is glued to itself (crossing
-- either end lands on the other, with 'Straight'\'s coordinate reflected);
-- 'Straight' has a genuine edge, exactly the 'Clamped' boundary it is built
-- from. Position 0 of @'[Clamped w, Clamped h]@ is 'Wrapped', position 1 is
-- 'Straight' --- a runtime tag, not a type-level axis position, the same
-- choice 'Data.Grid.Atlas.CubeMap.Axis' makes and for the same reason: both
-- axes share a type, so nothing else could say which one a heading points
-- along.
data Axis
    = Wrapped
    | Straight
    deriving (Eq, Show, Enum, Bounded)

-- | The direction a walker on the strip is currently facing: one axis of
-- the chart, and which end of it the walker is heading towards. Same shape
-- as 'Data.Grid.Atlas.CubeMap.Heading', for the same reason.
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

-- | Build the @k = 1@ atlas: one chart, glued to itself. 'atlasFromVector'
-- never returns 'Nothing' at a length-1 vector, the same reason
-- 'Data.Grid.Atlas.CubeMap.cubeAtlas' cannot fail at length 6.
mobiusAtlas ::
       forall w h a. Grid '[ Clamped w, Clamped h] a
    -> Atlas '[ Clamped w, Clamped h] 1 a
mobiusAtlas g =
    fromMaybe (error "mobiusAtlas: impossible, one chart always matches k = 1") $
    atlasFromVector (V.singleton g)

-- | The strip's one seam: crossing 'Wrapped' at either end lands on the
-- other end of the same (only) chart, flagged as orientation-reversing ---
-- the frame flip 'mobiusStep' turns into 'Straight'\'s coordinate
-- reflection. 'Straight' has no seam at all: both its extrema map to
-- themselves, unglued, the same filler @Test.Seam@\'s own @cylinder@
-- example uses for a boundary label that never crosses anything.
--
-- A 'SeamTable' (sized-grid-b15): which boundary of a chart is glued to
-- which is the same combinatorial object whatever the chart holds, so the
-- type and the law it must obey live in @atlas-topology@; only
-- 'crossMobiusEdge'\'s two equations are Mobius-specific.
mobiusSeam :: SeamTable () (Axis, Extremum)
mobiusSeam = SeamTable crossMobiusEdge

-- | 'mobiusSeam'\'s table: the one physical seam, named by both of its
-- half-edges pointing back at each other, plus the two half-edges of
-- 'Straight' left unglued --- checked by @Test.Mobius@\'s
-- @mobiusSeamPairsUp@.
crossMobiusEdge :: () -> (Axis, Extremum) -> ((), (Axis, Extremum), Bool)
crossMobiusEdge () (Wrapped, AtMin) = ((), (Wrapped, AtMax), True)
crossMobiusEdge () (Wrapped, AtMax) = ((), (Wrapped, AtMin), True)
crossMobiusEdge () (Straight, side) = ((), (Straight, side), False)

-- | Move one cell in a heading, crossing the seam --- with its frame
-- transform applied to the heading itself --- if the step would leave the
-- chart on 'Wrapped'. 'Nothing' if it would leave the chart on 'Straight'
-- instead: unlike 'Data.Grid.Atlas.CubeMap.cubeStep', a Mobius strip has a
-- genuine edge on that axis, exactly 'Clamped'\'s own boundary, so a step
-- off it is refused rather than resolved --- the same shape
-- 'Data.Grid.Atlas.atlasOffsetHead' has at an atlas's own two ends.
--
-- Only ever moves the atlas coordinate by one cell, for the same reason
-- 'Data.Grid.Atlas.CubeMap.cubeStep' does: composing several calls is what
-- carries a walker further, and doing so needs no 'Atlas' value at all ---
-- the landing coordinate is a pure function of the current one via
-- 'mobiusSeam', not a lookup into any particular atlas's contents.
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
