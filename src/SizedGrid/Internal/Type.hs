{-# LANGUAGE AllowAmbiguousTypes #-}

-- |
-- Module      :  SizedGrid.Internal.Type
-- Copyright   :  (C) 2018-18 Edward Wastell
-- License     :  MIT -style (see the file LICENSE)
-- Maintainer  :  Edward Wastell <ed@wastell.co.uk>
-- Stability   :  provisional
--
-- Type-level facts the library needs but GHC's Nat solver will not derive.
--
-- The export list is deliberately closed. 'windowFits' is an asserted axiom,
-- and an axiom that escapes into more modules than strictly need it is an axiom
-- waiting to be misused.
module SizedGrid.Internal.Type
  ( windowFits
  , requiring
  ) where

import           Data.Constraint
import           GHC.TypeLits
import           Unsafe.Coerce

-- | The two facts @shrinkGrid@ needs in order to call @dropGrid@ then
-- @takeGrid@, which GHC's Nat solver cannot derive on its own.
--
-- At the use site the window offset @n@ comes from @reifyCoord@ on a coord of
-- size @x@, so @n + 1 <= x@, and the @ShrinkableGrid@ instance requires
-- @x + z <= y + 1@. Together:
--
-- > n + 1 + z <= x + z <= y + 1        so   n + z <= y
--
-- which gives both @n <= y@ (drop stays in range) and @z <= y - n@ (the window
-- fits in what is left). That is ordinary linear arithmetic, but the second
-- wanted mentions a truncating subtraction over an existential @n@, which is
-- out of reach of ghc-typelits-natnormalise. So it is asserted here.
--
-- Soundness rests entirely on the @x + z <= y + 1@ constraint on the instance,
-- which /is/ checked at every call site.
windowFits :: forall n y z. Dict (n <= y, z <= (y - n))
windowFits = unsafeCoerce (Dict :: Dict (0 <= 0, 0 <= 0))

-- | Consume a constraint the implementation has no other use for.
--
-- Several signatures in "SizedGrid.Grid.Grid" carry a bound (@n <= m@,
-- @Mod (CoordNat big) (CoordNat small) ~ 0@) whose entire job is to stop the
-- caller building a `SizedGrid.Grid.Grid.Grid` whose type lies about its size.
-- The body never mentions such a bound, so @-Wredundant-constraints@ reports
-- it, and that module used to answer with a blanket
-- @-Wno-redundant-constraints@ -- which also hid the constraints that really
-- were dead. Naming the contract instead leaves the warning doing its job:
--
-- > takeGrid n (Grid v) = requiring @(n <= m) $ Grid $ V.take (natVal (Proxy @n)) v
--
-- Building the `Dict` is what consumes the evidence; @id@ alone would report
-- the constraint as redundant here instead.
requiring :: forall (c :: Constraint) a. c => a -> a
requiring x = case Dict @c of Dict -> x
{-# INLINE requiring #-}
