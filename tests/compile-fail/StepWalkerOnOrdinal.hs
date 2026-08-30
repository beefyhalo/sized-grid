-- | 'stepWalker' is total -- a walker with a heading always lands somewhere --
-- so it is affine, and on an 'Ordinal' axis there is no affine action for it
-- to use. Its heading is a @'Data.AffineSpace.Diff' ('Coord' cs)@, which is
-- stuck here, so the 'Walker' cannot even be written down.
--
-- The other negative half of sized-grid-i0ob.2. That a walker in a window is
-- inexpressible is a real gap, and a separate one: it belongs to
-- sized-grid-qbal, which re-indexes the heading by 'MapStep' and adds the
-- checked step this file asserts does not exist yet. Compiled by
-- Test.CompileFail, never by the test suite proper.
module StepWalkerOnOrdinal where

import Data.Grid.Sized

bad :: Walker '[Ordinal 5, Ordinal 5] Int -> Walker '[Ordinal 5, Ordinal 5] Int
bad = stepWalker
