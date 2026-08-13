{-# LANGUAGE AllowAmbiguousTypes #-}

-- |
-- Module      :  SizedGrid.Internal.Type
-- Copyright   :  (C) 2018-18 Edward Wastell
-- License     :  MIT -style (see the file LICENSE)
-- Maintainer  :  Edward Wastell <ed@wastell.co.uk>
-- Stability   :  provisional
--
-- Type-level odds and ends that do not belong to any one part of the API.
--
-- This module used to also hold @windowFits@, an @unsafeCoerce@-backed axiom
-- supplying the two facts @shrinkGrid@ needs. It was the library's only
-- @unsafeCoerce@; ghc-typelits-presburger now derives those facts, so it is
-- gone (sized-grid-wrc). The library asserts nothing.
module SizedGrid.Internal.Type
  ( requiring
  ) where

import           Data.Constraint

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
