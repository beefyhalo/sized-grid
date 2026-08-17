module Data.Grid.Sized.Focused
  ( FocusedGrid(..)
  , traceOffset
  , tracePath
  , walkEverywhere
  , Walker(..)
  , stepWalker
  , partitionFocus
  ) where

import           Data.Grid.Sized.Coord
-- The `Grid` type comes from the hidden module it is defined in, not from
-- "Data.Grid.Sized", because that one re-exports this module: taking the type
-- from its own module is what keeps the two from forming a cycle. The import
-- list is what it is so that the constructor stays behind
-- "Data.Grid.Sized.Unsafe".
import           Data.Grid.Sized.Internal.Grid (Grid)

import           Control.Comonad
import           Control.Comonad.Store
import           Data.AffineSpace (Diff)
import           Data.Functor.Rep
import           Generics.SOP

-- | Similar to `Grid`, but this has a focus on a certain square. Becuase of this we loose some instances, such as `Applicative`, but we gain a `Comonad` and `ComonadStore` instance. We can convert between a focused and unfocused list using facilites in `IsGrid`
data FocusedGrid cs a = FocusedGrid
    { focusedGrid         :: Grid cs a
    , focusedGridPosition :: Coord cs
    } deriving (Functor,Foldable,Traversable)

-- | `Grid` has had both of these all along; `FocusedGrid` had neither, which
-- made the `Comonad` instance below the one part of the library whose laws
-- could not be written down as a test -- @extract . duplicate == id@ needs to
-- compare two `FocusedGrid`s, and the coassociativity law needs to compare two
-- triply-nested ones and print them when they differ.
--
-- Equality is on both fields, so two grids holding the same cells but focused
-- on different positions are distinct. That is the notion the comonad laws want:
-- `duplicate` is required to preserve the focus, and an `Eq` that ignored the
-- focus would pass the laws without checking it.
deriving instance (All Eq cs, Eq a) => Eq (FocusedGrid cs a)

deriving instance (All Show cs, Show a) => Show (FocusedGrid cs a)

-- | @All Monoid cs@ and @All Semigroup cs@ used to be demanded by both this
-- instance and `ComonadStore` below. Neither body does anything but `index` and
-- `tabulate`, which need only the `Representable` instance for `Grid`, so the
-- two constraints were pure noise -- and noise every caller had to repeat,
-- since `Comonad` is the whole reason to reach for a `FocusedGrid`.
instance ( AllSizedKnown cs
         , IsCoordList cs
         , SListI cs
         ) =>
         Comonad (FocusedGrid cs) where
    extract (FocusedGrid g p) = index g p
    duplicate (FocusedGrid g p) = FocusedGrid (tabulate (FocusedGrid g)) p

instance ( AllSizedKnown cs
         , IsCoordList cs
         , SListI cs
         ) =>
         ComonadStore (Coord cs) (FocusedGrid cs) where
    pos = focusedGridPosition
    peek p (FocusedGrid g _) = index g p
    peeks func (FocusedGrid g p) = index g (func p)
    seek p (FocusedGrid g _) = FocusedGrid g p
    seeks func (FocusedGrid g p) = FocusedGrid g $ func p

-- | The cell a single displacement away from the focus, or 'Nothing' if the
-- step would leave the grid.
--
-- This is the relative counterpart to 'peek' that 'ComonadTraced' would have
-- given us, without the class: sized-grid-yws found that 'ComonadTraced'
-- cannot be added lawfully here, because its @Monoid m@ obligation does not
-- fit a displacement (an 'AdditiveGroup', not a 'Monoid' -- 'Int' has no
-- 'Semigroup' instance) and its fundep would commit 'FocusedGrid' to being
-- traced by a displacement *or* a 'Path' but never both, foreclosing the
-- order-dependent case sized-grid-ghj needs. A plain function has neither
-- problem and fuses the same way under 'extend'.
--
-- 'Maybe', not @a@, for the same reason 'offsetCoord' returns 'Maybe': a
-- displacement can name a point that is not on the grid, and this library
-- does not clamp or crash silently.
traceOffset ::
       ( AllSizedKnown cs
       , IsCoordList cs
       , AllDiffSame Int cs
       )
    => Coord (MapDiff cs)
    -> FocusedGrid cs a
    -> Maybe a
traceOffset d (FocusedGrid g p) = index g <$> offsetCoord p d

-- | The cell a 'Path' away from the focus, walked one step at a time through
-- 'walkPath' rather than summed first -- see 'Path' for the wall it walks
-- into that a summed displacement would not.
tracePath ::
       ( AllSizedKnown cs
       , IsCoordList cs
       , AllDiffSame Int cs
       )
    => Path cs
    -> FocusedGrid cs a
    -> Maybe a
tracePath p (FocusedGrid g focusPos) = index g <$> walkPath focusPos p

-- | Start a walker at every cell and follow the same 'Path' from each,
-- landing on the whole grid's worth of answers in one 'tabulate' instead of
-- calling 'tracePath' from each position by hand.
walkEverywhere ::
       ( AllSizedKnown cs
       , IsCoordList cs
       , AllDiffSame Int cs
       )
    => Path cs
    -> FocusedGrid cs a
    -> FocusedGrid cs (Maybe a)
walkEverywhere p = extend (tracePath p)

-- | A 'FocusedGrid' paired with a heading: the fibre bundle sized-grid-448
-- was filed for, in the sense
-- @Data.Manifold.FibreBundle.TangentBundle m = FibreBundle m (Needle m)@
-- pairs a point with a direction at it. 'FocusedGrid' was already the point
-- half --- a grid with a position, see the note on that type. The heading is
-- a @'Diff' ('Coord' cs)@, the same needle 'offsetCoord' and 'transportCoord'
-- already displace by (sized-grid-iet); the library does not restrict it to
-- a unit step in the type, any more than it restricts a 'Path' step to one,
-- so a heading several cells long is well-formed and just means a longer
-- stride.
data Walker cs a = Walker
    { walkerGrid    :: FocusedGrid cs a
    , walkerHeading :: Diff (Coord cs)
    }
    deriving (Functor)

deriving instance (All Eq cs, Eq a, Eq (Diff (Coord cs))) => Eq (Walker cs a)

deriving instance (All Show cs, Show a, Show (Diff (Coord cs))) =>
                   Show (Walker cs a)

-- | Take one step in the walker's own heading, transporting the heading
-- through 'transportCoord' --- the 'ParallelTransporting' half of the fibre
-- bundle sized-grid-448 asked for: as the walker crosses a seam, the
-- boundary policy decides what its heading becomes on the far side, not the
-- caller re-deriving it inline (the accident sized-grid-448 opens with,
-- ../aoc/src/2016/02.hs relying on 'Data.Grid.Sized.Coord.Clamped.Clamped'\'s
-- clamp to hold a walker's position at a wall).
--
-- Total, like 'transportCoord' and unlike 'traceOffset'\/'tracePath': a
-- walker with a heading always lands somewhere, because every boundary
-- policy's @('.+^')@ does.
stepWalker ::
       ( TransportCoordList cs
       , AllDiffSame Int cs
       )
    => Walker cs a
    -> Walker cs a
stepWalker (Walker (FocusedGrid g p) h) =
    case transportCoord p h of
        (p', h') -> Walker (FocusedGrid g p') h'

-- | Split a self-contained window into its centre value and a function
-- naming every other cell's value, sized-grid-meg's answer to the shape
-- Conway's rule actually wants --- a cell and its neighbours together,
-- rather than a caller of 'neighbours' re-deriving the centre by hand.
--
-- Prior art and the reasoning behind returning a function rather than
-- Chris Penner's @Grid window (Maybe a)@ are on 'PuncturedCoord'; this is
-- that type's one consumer. @g@ is any self-contained window, not a
-- 'FocusedGrid' sitting inside a larger one --- it is never boundary-clipped,
-- so unlike 'neighbours' this needs no boundary policy and always hands back
-- exactly @'MaxCoordSize' cs - 1@ values, whatever the axis types are.
partitionFocus ::
       forall cs a. (AllSizedKnown cs, IsCoordList cs, All CentredAxis cs)
    => Grid cs a
    -> (a, PuncturedCoord cs -> a)
partitionFocus g = (index g (centreCoord @cs), index g . puncturedToCoord)
