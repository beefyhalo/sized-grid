-- |
-- Module      :  Data.Grid.Sized.Unboxed
-- License     :  MIT -style (see the file LICENSE)
--
-- The unboxed grid: a `Data.Grid.Sized.GridOf` over "Data.Vector.Unboxed".
--
-- A @'Grid' cs Int@ stores 90,000 pointers to 90,000 heap-allocated boxes. A
-- @'UGrid' cs Int@ stores 90,000 machine words. For the numeric workloads this
-- library is aimed at -- summed-area tables, lattice simulations, cellular
-- automata over small enumerations -- that is the representation you want.
--
-- == What it costs you
--
-- `Functor`, `Foldable`, `Traversable`, `Applicative`, `Monad` and
-- `Data.Functor.Rep.Representable` are all gone, and no amount of cleverness
-- brings them back: each one promises to work at /every/ element type, and an
-- unboxed vector holds only the types with an `U.Unbox` instance. The
-- replacements are the bulk operations, which say the same things as plain
-- functions carrying an @'U.Unbox'@ constraint:
--
-- > fmap      ~>  mapGrid
-- > imap      ~>  imapGrid
-- > tabulate  ~>  tabulateGrid
-- > index     ~>  indexGrid
-- > foldl'    ~>  foldlGrid'
-- > liftA2 f  ~>  zipWithGrid f
--
-- `Data.Grid.Sized.Focused.FocusedGrid` and the `Control.Comonad.Comonad`
-- interface are boxed-only for the same reason, and more sharply: @duplicate@
-- builds a grid /of grids/, and a grid is never an unboxed element. A
-- comonadic cellular automaton stays on `Data.Grid.Sized.Grid`.
--
-- == What it buys you
--
-- Measured on a 300x300 @Int@ grid by the @boxed vs unboxed@ group in
-- @bench\/Main.hs@ (sized-grid-up6, GHC 9.12.3 aarch64-darwin). Both columns run
-- the same code at a different vector, so the ratio is the representation and
-- nothing else:
--
-- > operation             boxed      unboxed   speedup
-- > tabulateGrid          6.36 ms    1.93 ms     3.3x
-- > mapGrid then sum       318 us    92.1 us     3.5x
-- > foldlGrid'             312 us    88.7 us     3.5x
-- > transposeGrid         5.06 ms    2.15 ms     2.4x
-- > summed-area build     28.1 ms    13.1 ms     2.1x
-- > indexGrid x90000       751 us     793 us     none
--
-- Read the last row carefully, because it is the one that decides whether this
-- module is worth importing. The win is /wholly/ in operations that touch the
-- vector wholesale. A workload that walks coordinates and reads one element at
-- a time gains nothing measurable, because its cost is the coordinate
-- arithmetic and not the vector access. Reach for this module when the hot path
-- is 'mapGrid', 'zipWithGrid', 'foldlGrid'', 'Data.Grid.Sized.scanl1Grid' or
-- 'Data.Grid.Sized.transposeGrid' over the whole grid; do not reach for it
-- hoping to speed up 'indexGrid'.
--
-- An earlier throwaway spike reported much larger figures -- 7.5x, 16x, 6.5x,
-- 4.3x on those same rows -- and they did not survive being reproduced against
-- the real implementation. The spike compared a hand-written, monomorphic
-- unboxed loop against the library's boxed path, so it credited specialisation
-- to the representation. Both sides here are the shared generic code, and the
-- honest ratio is 2-3.5x.
--
-- == Everything else is shared
--
-- The shape algebra -- 'Data.Grid.Sized.takeGrid', 'Data.Grid.Sized.dropGrid',
-- 'Data.Grid.Sized.splitGrid', 'Data.Grid.Sized.mapLowerDim',
-- 'Data.Grid.Sized.gridTiles', 'Data.Grid.Sized.gridWindows',
-- 'Data.Grid.Sized.shrinkGrid' and the rest -- is not duplicated here and needs
-- no unboxed variant. Those functions are polymorphic in the vector, so
-- importing "Data.Grid.Sized" alongside this module gets you all of them at
-- @UGrid@, with the same size proofs. This module adds the type synonym and
-- nothing else of substance.
--
-- 'Data.Grid.Sized.gridWindows' is the worked example of what that is worth. It
-- was added to the boxed grid while this module was being written, and reached
-- @UGrid@ by changing @V.@ to @VG.@ in one signature and one body -- no second
-- implementation, no second set of size proofs, and "Test.Unboxed" checks the
-- two agree.
module Data.Grid.Sized.Unboxed
  ( UGrid
  , ugridFromVector
  , ugridFromList
  ) where

import           Data.Grid.Sized.Coord         (AllSizedKnown)
import           Data.Grid.Sized.Internal.Grid (AllGridSizeKnown, CollapseGrid,
                                                GridOf, gridFromList,
                                                gridFromVector)

import qualified Data.Vector.Unboxed           as U

-- | An unboxed grid. @UGrid cs a@ is @'GridOf' 'U.Vector' cs a@, and holds
-- exactly @MaxCoordSize cs@ elements just as a boxed one does.
type UGrid = GridOf U.Vector

-- | 'Data.Grid.Sized.gridFromVector' at @'U.Vector'@.
--
-- Given only for symmetry with 'ugridFromList'; the general one infers @v@ from
-- its argument perfectly well.
ugridFromVector ::
     forall cs a. (U.Unbox a, AllSizedKnown cs)
  => U.Vector a
  -> Maybe (UGrid cs a)
ugridFromVector = gridFromVector

-- | 'Data.Grid.Sized.gridFromList' at @'U.Vector'@.
--
-- This one earns its place. The general 'Data.Grid.Sized.gridFromList' takes a
-- nested list and returns @Maybe (GridOf v cs a)@, so nothing in its argument
-- says which vector to build; without an annotation on the result @v@ is
-- ambiguous. Naming the representation here is tidier than annotating the call.
ugridFromList ::
     forall cs a. (U.Unbox a, AllGridSizeKnown cs)
  => CollapseGrid cs a
  -> Maybe (UGrid cs a)
ugridFromList = gridFromList
