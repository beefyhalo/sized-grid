-- |
-- Module      :  Data.Grid.Sized.Unsafe
-- License     :  MIT -style (see the file LICENSE)
--
-- The escape hatch: building a `Data.Grid.Sized.Grid` from a vector without
-- checking its length.
--
-- Importing this module opts out of the one guarantee the library provides. A
-- @Grid cs a@ is supposed to hold exactly @MaxCoordSize cs@ elements; build one
-- through `unsafeGridFromVector` and nothing checks that it does. A grid that is
-- too short makes `Data.Functor.Rep.index` read past the end of the vector, and
-- makes ('<*>') truncate silently against a well-formed grid.
--
-- /Read past the end/, not throw: `Data.Grid.Sized.index` indexes without a
-- bounds check, because on a grid built through the safe API the size invariant
-- has already proved the position is in range. Asserting that invariant with
-- this function and getting it wrong therefore gives an unrelated value, or a
-- crash, rather than an exception naming the position. That is the honest
-- statement of the trade and it does not change who is exposed: the check it
-- removes could only ever have fired on a grid built here.
--
-- It is a separate module, and not re-exported by "Data.Grid.Sized", so that reaching
-- for it is a decision that shows up in an import list.
--
-- Reading a grid's vector is not here, because reading is safe: that is
-- `Data.Grid.Sized.gridVector`, and it needs no unsafe import.
--
-- The legitimate use is applying a vector function that preserves length, where
-- you can see that it does but the type cannot say so:
--
-- > rotateRows :: Grid cs a -> Grid cs a
-- > rotateRows = unsafeGridFromVector . (\v -> V.backpermute v ixs) . gridVector
--
-- Before writing that, check whether the safe API already covers it:
-- `Data.Grid.Sized.scanl1Grid` for running totals, `fmap` and
-- `Control.Lens.Indexed.imap` for pointwise changes,
-- `Data.Functor.Rep.tabulate` to build from a function of the coordinate, and
-- `Data.Grid.Sized.gridFromVector` whenever the length can be checked at
-- runtime instead of asserted.
module Data.Grid.Sized.Unsafe
  ( unsafeGridFromVector
  ) where

import           Data.Grid.Sized.Internal.Grid (unsafeGridFromVector)
