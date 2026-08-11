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

import           SizedGrid

import           Data.Proxy
import           Generics.SOP    hiding (S, Z)
import           GHC.TypeLits
import           Test.QuickCheck (Arbitrary (..), Arbitrary1 (..), oneof)

instance (1 <= n, KnownNat n) => Arbitrary (Periodic n) where
  arbitrary = Periodic <$> oneof (map pure [minBound .. maxBound])

instance (1 <= n, KnownNat n) => Arbitrary (Clamped n) where
  arbitrary = Clamped <$> oneof (map pure [minBound .. maxBound])

instance (All Arbitrary cs, SListI cs) => Arbitrary (Coord cs) where
  arbitrary = Coord <$> hsequence (hcpure (Proxy @Arbitrary) arbitrary)

instance AllSizedKnown cs => Arbitrary1 (Grid cs) where
  liftArbitrary g = sequenceA (pure g)

instance (AllSizedKnown cs, Arbitrary a) => Arbitrary (Grid cs a) where
  arbitrary = liftArbitrary arbitrary
