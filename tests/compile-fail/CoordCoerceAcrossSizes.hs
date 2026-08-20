-- | sized-grid-adr.16 made a 'Coord' a bare 'Int' holding its row-major
-- position, which means @cs@ no longer appears in the representation at all
-- and GHC would infer a /phantom/ role for it. A phantom role would make this
-- @coerce@ legal, and it would forge a @Coord '[Clamped 3]@ holding position
-- 8 -- out of range for the type, and read straight through
-- 'Data.Grid.Sized.indexGrid'\'s @unsafeIndex@.
--
-- The @type role Coord nominal@ annotation is what stops it, the same
-- annotation and the same reason as 'Data.Grid.Sized.Ordinal.Ordinal'\'s.
-- Compiled by Test.CompileFail, never by the test suite proper.
module CoordCoerceAcrossSizes where

import Data.Coerce (coerce)
import Data.Grid.Sized

bad :: Coord '[Clamped 9] -> Coord '[Clamped 3]
bad = coerce
