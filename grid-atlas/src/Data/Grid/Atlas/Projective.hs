{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}

-- | A projective plane: a single-chart 'Atlas' glued to itself along both
-- axes with the other axis reflected on crossing.
module Data.Grid.Atlas.Projective
  ( Axis (..),
    pattern Horizontal,
    pattern Vertical,
    Heading (..),
    projectiveAtlas,
    projectiveSeam,
    projectiveStep,
  )
where

import Data.Atlas.Topology.Seam (SeamTable (..), crossSeam)
import Data.Functor.Identity (Identity (..))
import Data.Grid.Atlas
import Data.Grid.Atlas.Rect
import Data.Grid.Sized
import Data.Maybe (fromMaybe)
import Data.Vector qualified as V
import GHC.TypeLits

-- | The first chart axis.
pattern Horizontal :: Axis
pattern Horizontal = U

-- | The second chart axis.
pattern Vertical :: Axis
pattern Vertical = V

{-# COMPLETE Horizontal, Vertical #-}

projectiveAtlas ::
  forall w h a.
  Grid '[Clamped w, Clamped h] a ->
  Atlas '[Clamped w, Clamped h] 1 a
projectiveAtlas g =
  fromMaybe (error "projectiveAtlas: impossible, one chart always matches k = 1") $
    atlasFromVector (V.singleton g)

projectiveSeam :: SeamTable () (Axis, Extremum)
projectiveSeam = SeamTable crossProjectiveEdge

crossProjectiveEdge :: () -> (Axis, Extremum) -> ((), (Axis, Extremum), Bool)
crossProjectiveEdge () (Horizontal, AtMin) = ((), (Horizontal, AtMax), True)
crossProjectiveEdge () (Horizontal, AtMax) = ((), (Horizontal, AtMin), True)
crossProjectiveEdge () (Vertical, AtMin) = ((), (Vertical, AtMax), True)
crossProjectiveEdge () (Vertical, AtMax) = ((), (Vertical, AtMin), True)

-- | Total: the projective plane has no boundary, so every step crosses into
-- the same chart when it leaves the rectangle.
-- Every crossing is a 'MirroredSeam': both of this surface's seams glue with a
-- reflection, which is what separates it from the Klein bottle, where one does
-- and one does not.
projectiveStep ::
  forall w h.
  (KnownNat w, KnownNat h, 1 <= w, 1 <= h) =>
  AtlasCoord '[Clamped w, Clamped h] 1 ->
  Heading ->
  (AtlasCoord '[Clamped w, Clamped h] 1, Heading, Crossing)
projectiveStep (chart, u :| v :| EmptyCoord) heading =
  let Landing () (ui, vi) heading' crossing =
        runIdentity $
          rectStep
            axisSize
            (\() edge -> Identity (crossSeam projectiveSeam () edge))
            ()
            (ordinalToInt (unClamped u), ordinalToInt (unClamped v))
            heading
   in ( ( chart,
          Clamped (unsafeOrdinal ui)
            :| Clamped (unsafeOrdinal vi)
            :| EmptyCoord
        ),
        heading',
        crossing
      )
  where
    axisSize Horizontal = ordinalSize @w
    axisSize Vertical = ordinalSize @h
