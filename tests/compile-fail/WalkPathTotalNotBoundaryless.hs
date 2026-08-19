-- | 'walkPathTotal' needs 'Boundaryless' on every axis; 'Clamped' has walls
-- and so cannot supply it. Compiled by Test.CompileFail, never by the test
-- suite proper.
module WalkPathTotalNotBoundaryless where

import Data.Grid.Sized

bad :: Coord '[Periodic 5, Clamped 5] -> Path '[Periodic 5, Clamped 5] -> Coord '[Periodic 5, Clamped 5]
bad = walkPathTotal
