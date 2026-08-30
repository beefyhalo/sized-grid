-- | A restriction destroys the boundary policy: 'gridWindows' returns an
-- @Ordinal@-axed window whatever the source's axis type was, so a window can
-- no longer be annotated with the source's policy.
--
-- This is sized-grid-mbh0. @gridWindows \@(Clamped 3)@ over a
-- @Grid '[Periodic 9]@ used to compile -- @small@ and @big@ were independent
-- types related only through @CoordNat@ -- and produced a window that wrapped
-- round its own three cells, one step left of its first cell reading 4 where
-- the same step in the source reads 1. Compiled by Test.CompileFail, never by
-- the test suite proper.
module WindowKeepsSourcePolicy where

import Data.Grid.Sized

bad :: Grid '[Periodic 9] Int -> [Grid '[Periodic 3] Int]
bad = gridWindows @3
