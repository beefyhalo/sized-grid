-- | 'takeGrid' needs @n <= m@; taking 9 from a 3-grid does not typecheck.
-- Compiled by Test.CompileFail, never by the test suite proper.
module TakeGridTooBig where

import Data.Grid.Sized

bad :: Grid '[ Ordinal 3] Int -> Grid '[ Ordinal 9] Int
bad = takeGrid 9
