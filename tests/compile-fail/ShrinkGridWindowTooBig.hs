-- | The 'ShrinkableGrid' instance needs @x + z <= y + 1@: a window of size
-- @z@ taken at any of @x@ offsets must still fit inside a source of size
-- @y@. Three offsets times a window of 3 does not fit inside a source of 3:
-- @3 + 3 <= 3 + 1@ is false. Compiled by Test.CompileFail, never by the test
-- suite proper.
module ShrinkGridWindowTooBig where

import Data.Grid.Sized

bad :: Coord '[ Ordinal 3] -> Grid '[ Ordinal 3] Int -> Grid '[ Ordinal 3] Int
bad = shrinkGrid
