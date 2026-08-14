{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The 'Arbitrary' instances the test suites share.
--
-- These are orphans, and deliberately so: they belong to neither QuickCheck nor
-- the library, and a library dependency on QuickCheck to avoid the orphan would
-- be a much worse trade. They lived in @tests/Main.hs@ until 'Test.Neighbours'
-- needed them too; a second module cannot redefine them without a duplicate
-- instance, so they moved here.
module Test.Arbitrary
  (
  ) where

import           Data.Grid.Sized

import           Data.Proxy
import           Generics.SOP    hiding (S, Z)
import           GHC.TypeLits
import           Test.QuickCheck (Arbitrary (..), Arbitrary1 (..), Gen,
                                  chooseInt)

-- | Pick one position on an axis, in constant time.
--
-- The coordinate instances below used to read
-- @oneof (map pure [minBound .. maxBound])@, which materialises the entire
-- domain of the axis to draw a single sample. That is unnoticeable at the
-- @Periodic 10@ this suite mostly uses and quadratic in the axis size over a
-- run, so the cost of testing a wider grid grew with the square of its width --
-- which is a good reason not to test one, and exactly the wrong incentive.
--
-- @1 <= n@ is load-bearing rather than inherited: it is what gives 'maxBound',
-- and an axis of size zero has no inhabitant to return.
genOrdinal :: forall n. (1 <= n, KnownNat n) => Gen (Ordinal n)
genOrdinal =
  unsafeOrdinal <$> chooseInt (0, ordinalToNum (maxBound :: Ordinal n))

-- | Needed by the windowing properties in "Test.Shrink", which quantify over
-- the offset a window is taken at, and that offset is an 'Ordinal'.
instance (1 <= n, KnownNat n) => Arbitrary (Ordinal n) where
  arbitrary = genOrdinal

instance (1 <= n, KnownNat n) => Arbitrary (Periodic n) where
  arbitrary = Periodic <$> genOrdinal

instance (1 <= n, KnownNat n) => Arbitrary (Clamped n) where
  arbitrary = Clamped <$> genOrdinal

-- | The product a 'Coord' wraps. Split out from the 'Coord' instance below so
-- that the '_WrappedCoord' iso can be quantified over from both ends.
instance (All Arbitrary cs, SListI cs) => Arbitrary (NP I cs) where
  arbitrary = hsequence (hcpure (Proxy @Arbitrary) arbitrary)

instance (All Arbitrary cs, SListI cs) => Arbitrary (Coord cs) where
  arbitrary = Coord <$> arbitrary

instance AllSizedKnown cs => Arbitrary1 (Grid cs) where
  liftArbitrary g = sequenceA (pure g)

instance (AllSizedKnown cs, Arbitrary a) => Arbitrary (Grid cs a) where
  arbitrary = liftArbitrary arbitrary

-- | A grid and a focus drawn independently: the comonad laws have to hold for
-- every focus, not just an in-some-sense canonical one, and every 'Coord' is a
-- valid focus by construction.
instance (AllSizedKnown cs, All Arbitrary cs, SListI cs, Arbitrary a) =>
         Arbitrary (FocusedGrid cs a) where
  arbitrary = FocusedGrid <$> arbitrary <*> arbitrary
