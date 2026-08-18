-- | Turns a type-level 'Bool' into a custom compile error.
module Data.Grid.Sized.Internal.Error (type (?!), ErrorMessage) where

import Data.Kind (Constraint)
import GHC.TypeLits (ErrorMessage, TypeError)

type family (b :: Bool) ?! (e :: ErrorMessage) :: Constraint where
  'True ?! _ = ()
  'False ?! e = TypeError e

infixr 1 ?!
