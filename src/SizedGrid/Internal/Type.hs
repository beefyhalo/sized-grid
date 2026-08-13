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
-- The export list is deliberately closed: a fact that escapes into more modules
-- than strictly need it is a fact waiting to be misused.
module SizedGrid.Internal.Type
  ( windowFits
  , requiring
  ) where

import           Data.Constraint
import           Data.Constraint.Nat (leTrans, plusMonotone1)
import           GHC.TypeLits

-- | The fact @shrinkGrid@ needs in order to call @sliceGrid@: a window of @z@
-- taken at offset @n@ stays inside a source of size @y@.
--
-- At the use site @n@ comes from @reifyCoord@ on a coord of size @x@, so
-- @n + 1 <= x@, and the @ShrinkableGrid@ instance requires @x + z <= y + 1@:
--
-- > n + 1 + z <= x + z <= y + 1        so   n + z <= y
--
-- This used to be an @unsafeCoerce@ producing a @Dict@ out of nothing -- the
-- library's only one -- because the goal was then stated as @z <= y - n@ over
-- GHC's /truncating/ subtraction, which ghc-typelits-natnormalise will not
-- touch. Two changes retired it (sized-grid-wrc):
--
-- 1. @sliceGrid@ replaced @takeGrid . dropGrid@, so the subtraction never
--    appears and the goal is the linear @n + z <= y@.
-- 2. The two steps natnormalise still cannot take -- adding @z@ to both sides
--    of a given, and chaining two @<=@ givens transitively -- are named here as
--    'plusMonotone1' and 'leTrans' from @constraints@.
--
-- What is gained is not the disappearance of @unsafeCoerce@ from the world:
-- 'plusMonotone1' and 'leTrans' are themselves implemented with it inside
-- @constraints@. It is that the /reasoning/ is now machine-checked. Those two
-- are general, standard lemmas -- monotonicity of @+@ and transitivity of @<=@
-- -- whereas the axiom they replace was a bespoke three-variable claim about
-- this library's own window invariant, asserted whole. GHC now verifies that
-- the composition really does yield @n + z <= y@ from the givens, so an error
-- in that reasoning is a type error rather than a silently wrong grid.
--
-- The closing step from @n + 1 + z <= y + 1@ to @n + z <= y@ is natnormalise
-- cancelling a constant from both sides, which it does do.
windowFits ::
       forall n x y z. (n + 1 <= x, x + z <= y + 1)
    => Dict (n + z <= y)
windowFits =
    case plusMonotone1 @(n + 1) @x @z of
        Sub Dict ->
            case leTrans @(n + 1 + z) @(x + z) @(y + 1) of
                Sub Dict -> Dict

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
