-- | 'splitHigherDim's second component is fixed at @x - y@, not a free type
-- variable; annotating it as anything else does not typecheck. Compiled by
-- Test.CompileFail, never by the test suite proper.
module SplitHigherDimWrongRemainder where

import Data.Grid.Sized

bad :: Grid '[Ordinal 3, Ordinal 3] Int -> Grid '[Ordinal 7, Ordinal 3] Int
bad g =
  let (_ :: Grid '[Ordinal 1, Ordinal 3] Int, b) = splitHigherDim g
   in b
