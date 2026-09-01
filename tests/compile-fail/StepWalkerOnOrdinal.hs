-- | 'stepWalker' is total -- a walker with a heading always lands somewhere --
-- so it is affine, and on an 'Ordinal' axis there is no affine action for it
-- to use. It keeps the affine @'MapDiff' cs@ that 'Data.Grid.Sized.transportCoord'
-- takes and bridges it to the heading with @'MapStep' cs ~ 'MapDiff' cs@; on an
-- 'Ordinal' axis that equality is @'Int' ~ 'Data.AffineSpace.Diff' (Ordinal 5)@,
-- and @Diff (Ordinal 5)@ is stuck.
--
-- The line the negative half of sized-grid-i0ob.2 drew, still held after
-- sized-grid-qbal re-indexed the /heading/ by 'MapStep': the 'Walker' can now
-- be written down on an 'Ordinal' axis and 'Data.Grid.Sized.stepWalkerWithin'
-- -- the checked step -- moves it, but 'stepWalker' stays refused, because
-- being total is something the axis type has to license. Compiled by
-- Test.CompileFail, never by the test suite proper.
module StepWalkerOnOrdinal where

import Data.Grid.Sized

bad :: Walker '[Ordinal 5, Ordinal 5] Int -> Walker '[Ordinal 5, Ordinal 5] Int
bad = stepWalker
