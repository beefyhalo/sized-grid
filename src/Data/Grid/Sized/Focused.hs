{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Grid.Sized.Focused
  ( FocusedGrid (..),
    focusedAtZero,
    traceOffset,
    tracePath,
    offsetFocus,
    walkFocus,
    focusRay,
    walkEverywhere,
    Walker (..),
    walkerFrameFlips,
    stepWalker,
    stepWalkerWithin,
    walkerTrail,
    partitionFocus,
  )
where

-- `Grid` is imported from its own hidden module, not "Data.Grid.Sized", to
-- avoid an import cycle (that module re-exports this one).

import Control.Comonad
import Control.Comonad.Store
import Control.DeepSeq (NFData (..))
import Data.Functor.Rep
import Data.Grid.Sized.Coord
import Data.Grid.Sized.Internal.Grid (Grid)
import Generics.SOP

-- | Like `Grid` but with a focus on a certain cell; trades `Applicative` for
-- `Comonad` and `ComonadStore`.
data FocusedGrid cs a = FocusedGrid
  { focusedGrid :: Grid cs a,
    focusedGridPosition :: Coord cs
  }
  deriving stock (Functor, Foldable, Traversable)

deriving stock instance (Eq a) => Eq (FocusedGrid cs a)

deriving stock instance
  (IsCoordList cs, All Show cs, Show a) =>
  Show (FocusedGrid cs a)

-- @NFData (Coord cs)@ is unconditional now that a coordinate is one 'Int'.
instance (NFData (Grid cs a)) => NFData (FocusedGrid cs a) where
  rnf (FocusedGrid g p) = rnf g `seq` rnf p

-- | A grid focused at 'zeroCoord'.
focusedAtZero :: (IsCoordList cs) => Grid cs a -> FocusedGrid cs a
focusedAtZero = (`FocusedGrid` zeroCoord)

instance
  ( AllSizedKnown cs,
    IsCoordList cs,
    SListI cs
  ) =>
  Comonad (FocusedGrid cs)
  where
  extract (FocusedGrid g p) = index g p
  duplicate (FocusedGrid g p) = FocusedGrid (tabulate (FocusedGrid g)) p

instance
  ( AllSizedKnown cs,
    IsCoordList cs,
    SListI cs
  ) =>
  ComonadStore (Coord cs) (FocusedGrid cs)
  where
  pos = focusedGridPosition
  peek p (FocusedGrid g _) = index g p
  peeks func (FocusedGrid g p) = index g (func p)
  seek p (FocusedGrid g _) = FocusedGrid g p
  seeks func (FocusedGrid g p) = FocusedGrid g $ func p

-- | The cell a single displacement away from the focus, or 'Nothing' if the
-- step would leave the grid.
--
-- @'fmap' 'extract' . 'offsetFocus' d@: the position 'offsetFocus' moves to,
-- read off. Kept because every existing call site wants the value rather than
-- the moved grid; that it falls out of 'offsetFocus' in one line is the check
-- that the position-preserving lifting is the primitive one.
traceOffset ::
  ( AllSizedKnown cs,
    IsCoordList cs
  ) =>
  Delta (MapStep cs) ->
  FocusedGrid cs a ->
  Maybe a
traceOffset d = fmap extract . offsetFocus d

-- | The cell a 'Path' away from the focus, walked one step at a time through
-- 'walkPath' rather than summed first.
--
-- @'fmap' 'extract' . 'walkFocus' p@, for the reason 'traceOffset' is
-- @'fmap' 'extract' . 'offsetFocus'@.
tracePath ::
  ( AllSizedKnown cs,
    IsCoordList cs
  ) =>
  Path cs ->
  FocusedGrid cs a ->
  Maybe a
tracePath p = fmap extract . walkFocus p

-- | Move the focus one checked step in the given direction, or 'Nothing' if
-- the step would leave the grid --- 'offsetCoord' with the payload carried
-- along.
--
-- The position-preserving counterpart of 'traceOffset', and the one nothing
-- in the pointing family offered before (sized-grid-qbal): 'traceOffset' and
-- 'tracePath' both computed a new position and then threw it away. Indexed by
-- @'MapStep' cs@, so it works inside a window --- an
-- 'Data.Grid.Sized.Ordinal.Ordinal' axis included.
offsetFocus ::
  (IsCoordList cs) =>
  Delta (MapStep cs) ->
  FocusedGrid cs a ->
  Maybe (FocusedGrid cs a)
offsetFocus d (FocusedGrid g p) = FocusedGrid g <$> offsetCoord p d

-- | Walk the focus along a 'Path', one checked step at a time through
-- 'walkPath', stopping with 'Nothing' as soon as a step would leave the grid
-- --- so a route can fail even where its steps cancel out net.
--
-- The position-preserving counterpart of 'tracePath'.
walkFocus ::
  (IsCoordList cs) =>
  Path cs ->
  FocusedGrid cs a ->
  Maybe (FocusedGrid cs a)
walkFocus p (FocusedGrid g focusPos) = FocusedGrid g <$> walkPath focusPos p

-- | The focus stepped repeatedly in one direction: the grid focused at each
-- cell of 'coordRay' from the current focus, not including the current focus
-- itself. Infinite on a torus or with a zero displacement.
focusRay ::
  (IsCoordList cs) =>
  Delta (MapStep cs) ->
  FocusedGrid cs a ->
  [FocusedGrid cs a]
focusRay d (FocusedGrid g p) = FocusedGrid g <$> coordRay p d

-- | Start a walker at every cell and follow the same 'Path' from each.
walkEverywhere ::
  ( AllSizedKnown cs,
    IsCoordList cs
  ) =>
  Path cs ->
  FocusedGrid cs a ->
  FocusedGrid cs (Maybe a)
walkEverywhere p = extend (tracePath p)

-- | A 'FocusedGrid' paired with a heading and the 'Frame' the walk has
-- accumulated --- for each axis, whether the walker's own sense of it now runs
-- backwards against the chart's, @xor@-composed step by step.
--
-- 'walkerFrameFlips' is the parity of that frame, which is all an /orientation/
-- question needs; a consumer that reads direction keys in the walker's own
-- frame needs the whole element and reads it through 'throughFrame'.
--
-- The heading is a @'Delta' ('MapStep' cs)@ --- one signed step count per axis
-- --- and not the affine @'Data.AffineSpace.Diff' ('Coord' cs)@ it was
-- (sized-grid-qbal). That is what lets a walker be written down inside a
-- window: every restriction narrows its axis to
-- 'Data.Grid.Sized.Ordinal.Ordinal', @'Data.AffineSpace.Diff'
-- ('Data.Grid.Sized.Ordinal.Ordinal' n)@ is stuck, and so the old heading
-- field had no values there. At every axis list where both reduce they are the
-- same list, so no call site that compiled before changed. 'stepWalker' is
-- still total and still needs the affine action, so it bridges the two with
-- @'MapStep' cs ~ 'MapDiff' cs@ the way 'walkPathTotal' does.
data Walker cs a = Walker
  { walkerGrid :: FocusedGrid cs a,
    walkerHeading :: Delta (MapStep cs),
    walkerFrame :: Frame cs
  }
  deriving stock (Functor)

-- | Whether an odd number of axes have been reversed along the walk --- the
-- parity of 'walkerFrame'. A field before the walker carried the whole 'Frame'
-- (sized-grid-t8rw); kept as the projection 'stepWalker'\'s coarser consumers
-- (the ant, which only asks whether its handedness has flipped) actually want.
walkerFrameFlips :: Walker cs a -> Bool
walkerFrameFlips = frameParity . walkerFrame

deriving stock instance (Eq a, Eq (Delta (MapStep cs))) => Eq (Walker cs a)

deriving stock instance
  ( IsCoordList cs,
    All Show cs,
    Show a,
    Show (Delta (MapStep cs))
  ) =>
  Show (Walker cs a)

-- | Take one step in the walker's own heading, transporting the heading
-- through 'transportCoord' so the boundary policy decides what the heading
-- becomes when the walker crosses a seam.
--
-- Total, unlike 'traceOffset'\/ 'tracePath': a walker with a heading always
-- lands somewhere. Being total is what the axis type licenses, so this keeps
-- the affine @'MapDiff' cs@ that 'transportCoord' takes and bridges it to the
-- heading's @'MapStep' cs@ with an equality, the way 'walkPathTotal' does ---
-- free at a concrete axis list, and still refused on
-- 'Data.Grid.Sized.Ordinal.Ordinal'. The checked, position-preserving
-- counterpart that /does/ work in a window is 'stepWalkerWithin'.
stepWalker ::
  ( TransportCoordList cs,
    AllDiffSame Int cs,
    FrameAfterStep cs,
    MapStep cs ~ MapDiff cs
  ) =>
  Walker cs a ->
  Walker cs a
stepWalker (Walker (FocusedGrid g p) h fr) =
  case transportCoord p h of
    (p', h') -> Walker (FocusedGrid g p') h' (frameAfterStep p h fr)

-- | Take one /checked/ step in the walker's own heading: 'offsetCoord' on the
-- position, with the heading and the accumulated 'Frame' passed through
-- unchanged. 'Nothing' if the step would leave the grid.
--
-- The heading does not turn, because a checked step that succeeds has not hit
-- a wall --- the law 'axisFrameFlipsIsCoord' now states and sized-grid-c0s9
-- put in force. So there is no fold and no class here beyond 'IsCoordList',
-- which is what lets this run where 'stepWalker' cannot: inside a window, on
-- an 'Data.Grid.Sized.Ordinal.Ordinal' axis, anywhere a restriction reaches.
-- On a 'Data.Grid.Sized.Coord.Clamped.Clamped' axis it reports the wall with
-- 'Nothing' rather than clamping the walker onto it.
stepWalkerWithin ::
  (IsCoordList cs) =>
  Walker cs a ->
  Maybe (Walker cs a)
stepWalkerWithin (Walker (FocusedGrid g p) h fr) =
  (\p' -> Walker (FocusedGrid g p') h fr) <$> offsetCoord p h

-- | The walker's trail: itself, then every walker 'stepWalkerWithin' reaches
-- from it, stopping when a checked step would leave the grid. Finite unless
-- the heading wraps forever on a torus.
walkerTrail :: (IsCoordList cs) => Walker cs a -> [Walker cs a]
walkerTrail w = w : maybe [] walkerTrail (stepWalkerWithin w)

-- | Split a self-contained window into its centre value and a function
-- naming every other cell's value.
partitionFocus ::
  forall cs a.
  (AllSizedKnown cs, IsCoordList cs, All CentredAxis cs) =>
  Grid cs a ->
  (a, PuncturedCoord cs -> a)
partitionFocus g = (index g (centreCoord @cs), index g . puncturedToCoord)
