{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PatternSynonyms #-}

-- | A Klein bottle: a single-chart 'Atlas' glued to itself along both axes,
-- 'Twisted' reflecting 'Rolled' on crossing and 'Rolled' gluing straight
-- through. Gluing both pairs with a reflection would be the projective
-- plane, not this.
module Data.Grid.Atlas.Klein
  ( Axis(..)
  , pattern Twisted
  , pattern Rolled
  , Heading(..)
  , kleinAtlas
  , kleinSeam
  , kleinStep
  ) where

import           Data.Atlas.Topology.Seam (SeamTable (..), crossSeam)
import           Data.Grid.Atlas
import           Data.Grid.Atlas.Rect
import           Data.Grid.Sized

import           Data.Functor.Identity (Identity (..))
import           Data.Maybe            (fromMaybe)
import qualified Data.Vector           as V
import           GHC.TypeLits

-- | The bottle's two axes, named for how each self-seam glues: 'Twisted'
-- reflects its sibling on crossing, 'Rolled' glues straight through.
-- Synonyms for "Data.Grid.Atlas.Rect"\'s 'U' and 'V' rather than a type of
-- this module's own, so that 'rectStep' does this chart's coordinate
-- arithmetic too --- the names are the only thing this surface adds to a
-- rectangle, and they are what make @crossKleinEdge@ readable.
pattern Twisted :: Axis
pattern Twisted = U

-- | The axis that glues straight through --- see 'Twisted'.
pattern Rolled :: Axis
pattern Rolled = V

{-# COMPLETE Twisted, Rolled #-}

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
-- is glued to another, so no step can leave the surface --- which is why the
-- gluing handed to 'rectStep' answers in 'Identity' where a Mobius strip's
-- answers in 'Maybe'.
kleinStep ::
       forall w h. (KnownNat w, KnownNat h, 1 <= w, 1 <= h)
    => AtlasCoord '[ Clamped w, Clamped h] 1
    -> Heading
    -> (AtlasCoord '[ Clamped w, Clamped h] 1, Heading)
kleinStep (chart, u :| v :| EmptyCoord) heading =
    let ((), (ui, vi), heading') =
            runIdentity $
            rectStep
                axisSize
                (\() edge -> Identity (crossSeam kleinSeam () edge))
                ()
                (ordinalToInt (unClamped u), ordinalToInt (unClamped v))
                heading
    in ( ( chart
         , Clamped (unsafeOrdinal ui) :| Clamped (unsafeOrdinal vi) :|
           EmptyCoord)
       , heading')
  where
    axisSize Twisted = ordinalSize @w
    axisSize Rolled  = ordinalSize @h
