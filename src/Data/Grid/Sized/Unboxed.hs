-- |
-- Module      :  Data.Grid.Sized.Unboxed
-- License     :  MIT -style (see the file LICENSE)
--
-- The unboxed grid: a `Data.Grid.Sized.GridOf` over "Data.Vector.Unboxed",
-- machine words instead of heap-allocated boxes.
--
-- `Functor`, `Foldable`, `Traversable`, `Applicative`, `Monad` and
-- `Data.Functor.Rep.Representable` are all gone -- an unboxed vector only
-- holds types with a `U.Unbox` instance -- replaced by bulk operations
-- carrying that constraint explicitly: @mapGrid@, @imapGrid@,
-- @tabulateGrid@, @indexGrid@, @foldlGrid'@, @zipWithGrid@.
-- `Data.Grid.Sized.Focused.FocusedGrid` \/ `Control.Comonad.Comonad` stay
-- boxed-only, since @duplicate@ builds a grid of grids and a grid is never
-- an unboxed element.
--
-- On a 300x300 @Int@ grid (@bench\/Main.hs@'s @boxed vs unboxed@ group),
-- operations touching the vector wholesale (@tabulateGrid@, @mapGrid@,
-- @foldlGrid'@, @transposeGrid@) run 2-3.5x faster unboxed; @indexGrid@
-- does not, since its cost is coordinate arithmetic, not vector access.
-- Reach for this module for bulk operations, not single-element indexing.
--
-- Everything else -- @takeGrid@, @dropGrid@, @splitGrid@, @gridWindows@,
-- @shrinkGrid@, etc. -- is shared: those functions are polymorphic in the
-- vector, so importing "Data.Grid.Sized" alongside this module gets them at
-- @UGrid@ for free, with the same size proofs.
module Data.Grid.Sized.Unboxed
  ( UGrid
  , ugridFromVector
  , ugridFromList
  ) where

import           Data.Grid.Sized.Coord         (AllSizedKnown)
import           Data.Grid.Sized.Internal.Grid (CollapseGrid, GridOf,
                                                gridFromList, gridFromVector)

import qualified Data.Vector.Unboxed           as U

-- | An unboxed grid. @UGrid cs a@ is @'GridOf' 'U.Vector' cs a@, and holds
-- exactly @MaxCoordSize cs@ elements just as a boxed one does.
type UGrid = GridOf U.Vector

-- | 'Data.Grid.Sized.gridFromVector' at @'U.Vector'@.
ugridFromVector ::
     forall cs a. (U.Unbox a, AllSizedKnown cs)
  => U.Vector a
  -> Maybe (UGrid cs a)
ugridFromVector = gridFromVector

-- | 'Data.Grid.Sized.gridFromList' at @'U.Vector'@: the general version's
-- result vector @v@ is otherwise ambiguous, since nothing in a nested-list
-- argument says which vector to build.
ugridFromList ::
     forall cs a. (U.Unbox a, AllSizedKnown cs)
  => CollapseGrid cs a
  -> Maybe (UGrid cs a)
ugridFromList = gridFromList
