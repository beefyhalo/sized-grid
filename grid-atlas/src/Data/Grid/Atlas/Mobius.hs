{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}

-- | A Mobius strip: a single-chart 'Atlas' glued to itself along its
-- 'Wrapped' axis, reflecting 'Straight' on crossing.
module Data.Grid.Atlas.Mobius
  ( Axis (..),
    pattern Wrapped,
    pattern Straight,
    Heading (..),
    Crossing (..),
    crossedSeam,
    reversedFrame,
    mobiusAtlas,
    mobiusSeam,
    mobiusStep,
  )
where

import Data.Atlas.Topology.Seam (SeamTable (..), crossSeam)
import Data.Grid.Atlas
import Data.Grid.Atlas.Rect
import Data.Grid.Sized
import Data.Maybe (fromMaybe)
import Data.Vector qualified as V
import GHC.TypeLits

-- | The strip's two axes, named for what happens at their ends: 'Wrapped'
-- is glued to itself, 'Straight' has a genuine 'Clamped' edge with nothing
-- on the other side. Synonyms for "Data.Grid.Atlas.Rect"\'s 'U' and 'V'
-- rather than a type of this module's own, so that 'rectStep' does this
-- chart's coordinate arithmetic too --- the names are the only thing a
-- Mobius strip adds, and they are worth keeping, because they are what make
-- @crossMobiusEdge@ readable as a statement about the surface.
pattern Wrapped :: Axis
pattern Wrapped = U

-- | The axis with a genuine edge --- see 'Wrapped'.
pattern Straight :: Axis
pattern Straight = V

{-# COMPLETE Wrapped, Straight #-}

mobiusAtlas ::
  forall w h a.
  Grid '[Clamped w, Clamped h] a ->
  Atlas '[Clamped w, Clamped h] 1 a
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
-- That is the whole of what this function says beyond 'rectStep' --- the
-- gluing it hands over answers in 'Maybe', and a step is exactly as partial
-- as its gluing, where "Data.Grid.Atlas.CubeMap" hands over a total one and
-- gets a total step.
--
-- The 'Crossing' is the third answer and it is not decoration. This surface is
-- not orientable, so a walker carries a frame that a crossing can hand back
-- mirrored, and nothing in the position or the heading says so: a step through
-- the seam arrives at the mirrored row facing the way it already faced. Every
-- 'Wrapped' crossing here is a 'MirroredSeam'. See 'reversedFrame'.
mobiusStep ::
  forall w h.
  (KnownNat w, KnownNat h, 1 <= w, 1 <= h) =>
  AtlasCoord '[Clamped w, Clamped h] 1 ->
  Heading ->
  Maybe (AtlasCoord '[Clamped w, Clamped h] 1, Heading, Crossing)
mobiusStep (chart, u :| v :| EmptyCoord) heading = do
  Landing () (ui, vi) heading' crossing <-
    rectStep
      axisSize
      glued
      ()
      (ordinalToInt (unClamped u), ordinalToInt (unClamped v))
      heading
  pure
    ( ( chart,
        Clamped (unsafeOrdinal ui)
          :| Clamped (unsafeOrdinal vi)
          :| EmptyCoord
      ),
      heading',
      crossing
    )
  where
    axisSize Wrapped = ordinalSize @w
    axisSize Straight = ordinalSize @h
    -- 'mobiusSeam' has entries for the 'Straight' edges only because a
    -- 'SeamTable' is total; they are the identity, and this is where the
    -- honest answer -- that edge is glued to nothing -- is given instead.
    glued () (Straight, _) = Nothing
    glued () edge = Just (crossSeam mobiusSeam () edge)
