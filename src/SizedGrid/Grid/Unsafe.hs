-- |
-- Module      :  SizedGrid.Grid.Unsafe
-- License     :  MIT -style (see the file LICENSE)
--
-- The escape hatch: building a `SizedGrid.Grid.Grid.Grid` from a vector without
-- checking its length.
--
-- Importing this module opts out of the one guarantee the library provides. A
-- @Grid cs a@ is supposed to hold exactly @MaxCoordSize cs@ elements; build one
-- through `unsafeGridFromVector` and nothing checks that it does. A grid that is
-- too short makes `Data.Functor.Rep.index` throw on positions its own type says
-- are valid, and makes ('<*>') truncate silently against a well-formed grid.
--
-- It is a separate module, and not re-exported by "SizedGrid", so that reaching
-- for it is a decision that shows up in an import list.
--
-- Reading a grid's vector is not here, because reading is safe: that is
-- `SizedGrid.Grid.Grid.gridVector`, and it needs no unsafe import.
--
-- The legitimate use is applying a vector function that preserves length, where
-- you can see that it does but the type cannot say so:
--
-- > rotateRows :: Grid cs a -> Grid cs a
-- > rotateRows = unsafeGridFromVector . (\v -> V.backpermute v ixs) . gridVector
--
-- Before writing that, check whether the safe API already covers it:
-- `SizedGrid.Grid.Grid.scanl1Grid` for running totals, `fmap` and
-- `Control.Lens.Indexed.imap` for pointwise changes,
-- `Data.Functor.Rep.tabulate` to build from a function of the coordinate, and
-- `SizedGrid.Grid.Grid.gridFromVector` whenever the length can be checked at
-- runtime instead of asserted.
module SizedGrid.Grid.Unsafe
  ( unsafeGridFromVector
  ) where

import           SizedGrid.Internal.Grid (unsafeGridFromVector)
