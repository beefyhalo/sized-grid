{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- |
-- Module      :  SizedGrid.Internal.Type
-- Copyright   :  (C) 2018-18 Edward Wastell
-- License     :  MIT -style (see the file LICENSE)
-- Maintainer  :  Edward Wastell <ed@wastell.co.uk>
-- Stability   :  provisional

module SizedGrid.Internal.Type where

import           Data.Constraint
import           Data.Proxy
import           GHC.TypeLits
import           Unsafe.Coerce

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
-- > takeGrid p (Grid v) = requiring @(n <= m) $ Grid $ V.take (natVal p) v
--
-- Building the `Dict` is what consumes the evidence; @id@ alone would report
-- the constraint as redundant here instead.
requiring :: forall (c :: Constraint) a. c => a -> a
requiring x = case Dict @c of Dict -> x
{-# INLINE requiring #-}

-- | A singleton type for Bools
data SBool a where
  STrue :: SBool 'True
  SFalse :: SBool 'False

deriving instance Show (SBool a)

-- | A type constraint for getting `SingI`
class SBoolI a where
  sBool :: SBool a

instance SBoolI 'True where
  sBool = STrue

instance SBoolI 'False where
  sBool = SFalse

-- | Give a runtime representation of a type level number being less than or equal than another
sLessThan ::
       forall n m. (KnownNat n, KnownNat m)
    => Proxy n
    -> Proxy m
    -> SBool (n <=? m)
sLessThan _ _ =
    if natVal (Proxy @n) <= natVal (Proxy @m)
        then unsafeCoerce STrue
        else unsafeCoerce SFalse

-- | A Dict prove that m - 1 + 1 is m
takeAddIsId :: forall m . Dict (((m - 1) + 1) ~ m)
takeAddIsId = unsafeCoerce (Dict :: Dict (a ~ a))

-- | Magic is stole from Constraints, and I don't really understand it, but it is needed for 'takeNat'
newtype Magic n = Magic (KnownNat n => Dict (KnownNat n))

-- | Also don't understand
magic ::
       forall n m o.
       (Integer -> Integer -> Integer)
    -> (KnownNat n, KnownNat m) :- KnownNat o
magic f =
    Sub $
    unsafeCoerce
        (Magic Dict)
        (natVal (Proxy :: Proxy n) `f` natVal (Proxy :: Proxy m))

-- | Runtime proof that n - m is an insance of KnownNat if n and m are
takeNat :: (KnownNat n, KnownNat m) :- KnownNat (n - m)
takeNat = magic (-)

-- | The two facts @shrinkGrid@ needs in order to call @dropGrid@ then
-- @takeGrid@, which GHC's Nat solver cannot derive on its own.
--
-- At the use site the window offset @n@ comes from @asSizeProxy@ on a coord of
-- size @x@, so @n + 1 <= x@, and the @ShrinkableGrid@ instance requires
-- @x + z <= y + 1@. Together:
--
-- > n + 1 + z <= x + z <= y + 1        so   n + z <= y
--
-- which gives both @n <= y@ (drop stays in range) and @z <= y - n@ (the window
-- fits in what is left). That is ordinary linear arithmetic, but the second
-- wanted mentions a truncating subtraction over an existential @n@, which is
-- out of reach of ghc-typelits-natnormalise. So it is asserted here, in the
-- same spirit as 'takeAddIsId' above.
--
-- Soundness rests entirely on the @x + z <= y + 1@ constraint on the instance,
-- which /is/ checked at every call site.
windowFits :: forall n y z. Dict (n <= y, z <= (y - n))
windowFits = unsafeCoerce (Dict :: Dict (0 <= 0, 0 <= 0))
