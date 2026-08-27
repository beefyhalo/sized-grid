module Data.Grid.Sized.Class
  ( IsGrid(..)
  ) where

import           Data.Grid.Sized.Coord
import           Data.Grid.Sized.Focused
import           Data.Grid.Sized.Internal.Grid (Grid, cellLens)

import           Control.Lens

-- | Access grids at a `Coord`, and the `Grid` a `Grid` or `FocusedGrid`
-- contains. There is no @asFocusedGrid@: a `Grid` has no focus to hand back,
-- so no `Lens'` from it to `FocusedGrid` can be lawful (see sized-grid-3au).
-- Building a `FocusedGrid` from a `Grid` needs a `Coord` supplied -- write
-- @FocusedGrid g p@, or go through `_FocusedGrid`.
class IsGrid cs grid | grid -> cs where
  gridIndex :: Coord cs -> Lens' (grid a) a
  asGrid :: Lens' (grid a) (Grid cs a)

-- | No context. The whole instance is now position arithmetic: @'cellLens'@
-- needs only that the vector element type is storable in @v@, which for the
-- boxed `Grid` is unconditional, and @asGrid@ is the identity. The
-- @AllSizedKnown cs@ this used to carry came from `Data.Functor.Rep.index`,
-- which the old getter went through, and the @IsCoordList cs@ from the fold
-- that read a coordinate before sized-grid-adr.16 made a coordinate /be/ its
-- position. Both are gone the way sized-grid-o9s took `AllSizedKnown` off
-- `Apply` and `Bind`: a constraint a consumer must satisfy is a cost, and
-- this one bought nothing.
instance IsGrid cs (Grid cs) where
    -- @'cellLens'@ rather than the @ix (coordPosition coord)@ traversal this
    -- used to be, for the reason `Data.Grid.Sized.indexGrid` drops its bounds
    -- check: the position is in range by the 'Data.Grid.Sized.Ordinal.Ordinal'
    -- invariant and the vector has exactly that many elements by the grid's own
    -- size invariant, so the traversal could only ever have matched. Writing it
    -- as a traversal also said the lens might miss its target, which is the one
    -- thing the type of 'gridIndex' promises it does not. @cellLens@ is the
    -- same lens `Data.Grid.Sized.Optics.cell` and @'ix'@ are.
    gridIndex = cellLens
    asGrid = id

-- | Likewise contextless, by delegation to the instance above.
instance IsGrid cs (FocusedGrid cs) where
    gridIndex c = (\f (FocusedGrid g p) -> (`FocusedGrid` p) <$> f g) . gridIndex c
    asGrid = lens focusedGrid (\(FocusedGrid _ p) g -> FocusedGrid g p)
