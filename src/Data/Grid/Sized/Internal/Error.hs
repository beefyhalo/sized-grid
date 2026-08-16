-- | A type-level 'Bool' turned into a custom compile error, for stating size
-- preconditions ('takeGrid'\/'dropGrid' need @n <= m@, 'sliceGrid' needs
-- @off + len <= m@, window functions want an odd extent) so that a failed
-- precondition reads as a sentence instead of an unsolved 'GHC.Nat' goal.
--
-- The idea, not the code, is Chris Penner's: @grids@ (Hackage 0.5.0.1,
-- @Data.Grid.Internal.Errors@) carries the same four-line type family.
module Data.Grid.Sized.Internal.Error (type (?!), ErrorMessage) where

import Data.Kind (Constraint)
import GHC.TypeLits (ErrorMessage, TypeError)

-- | @b ?! e@ is the empty constraint when @b@ is 'True', and the custom
-- error @e@ when @b@ is 'False'.
type family (b :: Bool) ?! (e :: ErrorMessage) :: Constraint where
  'True ?! _ = ()
  'False ?! e = TypeError e

infixr 1 ?!
