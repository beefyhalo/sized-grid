-- | The escape hatch: building a `Data.Grid.Sized.Grid` from a vector without
-- checking its length. A @Grid cs a@ is supposed to hold exactly
-- @MaxCoordSize cs@ elements; build one through `unsafeGridFromVector` at
-- the wrong length and `Data.Functor.Rep.index` can read past the end of
-- the vector instead of throwing, since indexing trusts the size invariant
-- rather than bounds-checking. Kept out of "Data.Grid.Sized" so reaching
-- for it shows up in an import list. Legitimate use: applying a vector
-- function that preserves length where the type can't say so; check
-- `Data.Grid.Sized.gridFromVector` first if the length can be checked at
-- runtime instead.
module Data.Grid.Sized.Unsafe
  ( unsafeGridFromVector,
  )
where

import Data.Grid.Sized.Internal.Grid (unsafeGridFromVector)
