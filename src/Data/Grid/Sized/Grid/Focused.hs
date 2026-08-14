module Data.Grid.Sized.Grid.Focused
  ( FocusedGrid(..)
  ) where

import           Data.Grid.Sized.Coord
import           Data.Grid.Sized.Grid

import           Control.Comonad
import           Control.Comonad.Store
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
