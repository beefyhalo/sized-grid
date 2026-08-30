{-# LANGUAGE AllowAmbiguousTypes #-}

-- |
-- Module      :  Data.Grid.Sized.Internal.Type
-- Copyright   :  (C) 2018 Edward Wastell, (C) 2025-2026 Kevin Horlick
-- License     :  MIT -style (see the file LICENSE)
-- Maintainer  :  Kevin Horlick <beefyhalo@gmail.com>
-- Stability   :  provisional
--
-- Type-level facts the library needs but GHC's Nat solver will not derive.
module Data.Grid.Sized.Internal.Type
  ( windowFits,
    requiring,
  )
where

import Data.Constraint
import Data.Constraint.Nat (leTrans, plusMonotone1)
import GHC.TypeLits

-- | The fact @shrinkGrid@ needs in order to call @sliceGrid@: a window of @z@
-- taken at offset @n@ stays inside a source of size @y@.
--
-- @n + 1 <= x@ (from @reifyCoord@) and @x + z <= y + 1@ (the
-- @ShrinkableGrid@ instance) give @n + 1 + z <= x + z <= y + 1@, i.e.
-- @n + z <= y@ -- chained here via 'plusMonotone1' and 'leTrans' rather than
-- an @unsafeCoerce@, since natnormalise alone won't take those two steps.
windowFits ::
  forall n x y z.
  (n + 1 <= x, x + z <= y + 1) =>
  Dict (n + z <= y)
windowFits =
  case plusMonotone1 @(n + 1) @x @z of
    Sub Dict ->
      case leTrans @(n + 1 + z) @(x + z) @(y + 1) of
        Sub Dict -> Dict

-- | Consume a constraint the implementation has no other use for, so
-- @-Wredundant-constraints@ doesn't flag a bound (e.g. @n <= m@) whose only
-- job is to stop the caller building a `Data.Grid.Sized.Grid` that lies
-- about its size.
requiring :: forall (c :: Constraint) a. (c) => a -> a
requiring x = case Dict @c of Dict -> x
{-# INLINE requiring #-}
