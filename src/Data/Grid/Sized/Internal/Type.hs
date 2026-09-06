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
  ( cmpNatLE,
    windowFits,
    requiring,
  )
where

import Data.Constraint (Constraint, Dict(..), (:-)(..))
import Data.Constraint.Nat (leTrans, plusMonotone1)
import Data.Proxy
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

-- | 'cmpNat' specialized to a less-than-or-equal check, passing a witness of
-- @a <= b@ to the success branch.
cmpNatLE :: forall a b x. (KnownNat a, KnownNat b) => (Dict (a <= b) -> x) -> x -> x
cmpNatLE yesValue noValue =
  case cmpNat (Proxy @a) (Proxy @b) of
    LTI -> yesValue Dict
    EQI -> yesValue Dict
    GTI -> noValue
{-# INLINE cmpNatLE #-}

-- | Consume a constraint the implementation has no other use for, so
-- @-Wredundant-constraints@ doesn't flag a bound (e.g. @n <= m@) whose only
-- job is to stop the caller building a `Data.Grid.Sized.Grid` that lies
-- about its size.
requiring :: forall (c :: Constraint) a. (c) => a -> a
requiring x = case Dict @c of Dict -> x
{-# INLINE requiring #-}
