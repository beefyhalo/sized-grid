module Data.Grid.Sized.Focused
  ( FocusedGrid (..),
    focusedAtZero,
    traceOffset,
    tracePath,
    walkEverywhere,
    Walker (..),
    stepWalker,
    partitionFocus,
  )
where

-- `Grid` is imported from its own hidden module, not "Data.Grid.Sized", to
-- avoid an import cycle (that module re-exports this one).

import Control.Comonad
import Control.Comonad.Store
import Control.DeepSeq (NFData (..))
import Data.AffineSpace (Diff)
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
traceOffset ::
  ( AllSizedKnown cs,
    IsCoordList cs
  ) =>
  Delta (MapStep cs) ->
  FocusedGrid cs a ->
  Maybe a
traceOffset d (FocusedGrid g p) = index g <$> offsetCoord p d

-- | The cell a 'Path' away from the focus, walked one step at a time through
-- 'walkPath' rather than summed first.
tracePath ::
  ( AllSizedKnown cs,
    IsCoordList cs
  ) =>
  Path cs ->
  FocusedGrid cs a ->
  Maybe a
tracePath p (FocusedGrid g focusPos) = index g <$> walkPath focusPos p

-- | Start a walker at every cell and follow the same 'Path' from each.
walkEverywhere ::
  ( AllSizedKnown cs,
    IsCoordList cs
  ) =>
  Path cs ->
  FocusedGrid cs a ->
  FocusedGrid cs (Maybe a)
walkEverywhere p = extend (tracePath p)

-- | A 'FocusedGrid' paired with a heading.
data Walker cs a = Walker
  { walkerGrid :: FocusedGrid cs a,
    walkerHeading :: Diff (Coord cs)
  }
  deriving stock (Functor)

deriving stock instance (Eq a, Eq (Diff (Coord cs))) => Eq (Walker cs a)

deriving stock instance
  ( IsCoordList cs,
    All Show cs,
    Show a,
    Show (Diff (Coord cs))
  ) =>
  Show (Walker cs a)

-- | Take one step in the walker's own heading, transporting the heading
-- through 'transportCoord' so the boundary policy decides what the heading
-- becomes when the walker crosses a seam.
--
-- Total, unlike 'traceOffset'\/'tracePath': a walker with a heading always
-- lands somewhere.
stepWalker ::
  ( TransportCoordList cs,
    AllDiffSame Int cs
  ) =>
  Walker cs a ->
  Walker cs a
stepWalker (Walker (FocusedGrid g p) h) =
  case transportCoord p h of
    (p', h') -> Walker (FocusedGrid g p') h'

-- | Split a self-contained window into its centre value and a function
-- naming every other cell's value.
partitionFocus ::
  forall cs a.
  (AllSizedKnown cs, IsCoordList cs, All CentredAxis cs) =>
  Grid cs a ->
  (a, PuncturedCoord cs -> a)
partitionFocus g = (index g (centreCoord @cs), index g . puncturedToCoord)
