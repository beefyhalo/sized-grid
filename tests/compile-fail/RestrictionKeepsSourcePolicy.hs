-- | A restriction destroys the boundary policy, so the narrowing half of the
-- shape algebra returns @Ordinal@ along the axis it narrowed and can no
-- longer be annotated with the source's own policy.
--
-- This is sized-grid-pnws, the companion to sized-grid-mbh0's
-- WindowKeepsSourcePolicy. @takeGrid 3@ of a @Grid '[Periodic 9]@ used to
-- hand back a @Grid '[Periodic 3]@ that wrapped round its own three cells;
-- @dropGrid@'s remainder clamped at a wall the source does not have.
-- Compiled by Test.CompileFail, never by the test suite proper.
module RestrictionKeepsSourcePolicy where

import Data.Grid.Sized

bad :: Grid '[Periodic 9] Int -> Grid '[Periodic 3] Int
bad = takeGrid 3
