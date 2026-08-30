-- | Total movement is licensed by the axis type, and 'Ordinal' does not
-- license it: it cannot leave its interval, so it has no 'AffineSpace'
-- instance and @('.+^')@ has no displacement to take.
--
-- The negative half of sized-grid-i0ob.2. 'Data.Grid.Sized.Coord.offsetCoord'
-- now works on 'Ordinal' because a /bounds check/ is something every axis can
-- do; that must not be read as 'Ordinal' having acquired an affine action.
-- Clamping or throwing here is the silent answer the coordinate types exist to
-- prevent. Compiled by Test.CompileFail, never by the test suite proper.
module AffineStepOnOrdinal where

import Data.AffineSpace ((.+^))
import Data.Grid.Sized

bad :: Coord '[Ordinal 5, Ordinal 5] -> Delta '[Int, Int] -> Coord '[Ordinal 5, Ordinal 5]
bad = (.+^)
