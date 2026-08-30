-- | 'dropGrid' needs @n <= m@; dropping 9 from a 3-grid does not typecheck.
-- Compiled by Test.CompileFail, never by the test suite proper.
module DropGridTooBig where

import Data.Grid.Sized
import GHC.TypeLits (type (-))

bad :: Grid '[Ordinal 3] Int -> Grid '[Ordinal (3 - 9)] Int
bad = dropGrid 9
