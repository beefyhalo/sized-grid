{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module SizedGrid.Ordinal where

import           Control.Lens            (Prism', prism')
import           Control.Monad           (guard)
import           Data.Aeson
import           Data.Constraint
import           Data.Constraint.Nat
import           Data.Maybe              (fromJust)
import           Data.Proxy
import           GHC.TypeLits
import           System.Random

-- | An Ordinal can only hold m different values, ususally corresponding to 0 .. m - 1. We store it here using a `Proxy` of a type level number and use constraints to keep the required invariants.
--
-- Desprite represeting a number, Ordinal is not an instance of Num and many functions (such as negate) would only be partial
data Ordinal m where
  Ordinal :: (KnownNat n, KnownNat m, n + 1 <= m) => Proxy n -> Ordinal m

instance Show (Ordinal m) where
  show (Ordinal p) = "Ordinal (" ++ show (natVal p) ++ "/" ++ show (natVal (Proxy @m)) ++ ")"

instance Eq (Ordinal m) where
  Ordinal a == Ordinal b = natVal a == natVal b

instance Ord (Ordinal m) where
  compare (Ordinal a) (Ordinal b) = compare (natVal a) (natVal b)

instance (1 <= m, KnownNat m) => Random (Ordinal m) where
  randomR (mi, ma) g =
    let (n, g') = randomR (fromEnum mi, fromEnum ma) g
    in (toEnum n, g')
  random = randomR (minBound, maxBound)

-- | Convert a normal integral to an ordinal. If it is outside the range (< 0 or >= m), Nothing is returned.
numToOrdinal ::
       forall a m. (KnownNat m, Integral a)
    => a
    -> Maybe (Ordinal m)
numToOrdinal n = 
  case someNatVal (fromIntegral n) of
    Nothing -> Nothing
    Just (SomeNat (p :: Proxy n)) ->
      (case cmpNat (Proxy @(n + 1)) (Proxy @m) of
        GTI -> Nothing
        LTI -> Just $ Ordinal p  
        EQI -> Just $ Ordinal p) \\ plusNat @n @1
              
-- | Transform an ordinal to a given number
ordinalToNum :: Num a => Ordinal m -> a
ordinalToNum (Ordinal p) = fromIntegral $ natVal p

strengthenOrdinal :: forall n m . (KnownNat m, n <= m) => Ordinal n -> Ordinal m
strengthenOrdinal (Ordinal (p :: Proxy x)) = Ordinal p \\ leTrans @(x + 1) @n @m

weakenOrdinal :: KnownNat m => Ordinal n -> Maybe (Ordinal m)
weakenOrdinal = numToOrdinal . ordinalToNum @Integer

-- | Convert between an ordinal and a usual number. This is a `Prism` as it may fail as `Ordinals` can only exist in a certain range.
_Ordinal :: (KnownNat n, Integral a) => Prism' a (Ordinal n)
_Ordinal = prism' ordinalToNum numToOrdinal

instance (1 <= m, KnownNat m) => Bounded (Ordinal m) where
  minBound = Ordinal @0 Proxy
  maxBound = Ordinal @(m - 1) Proxy \\ plusMinusInverse3 @1 @m \\ minusNat @m @1

instance (1 <= m, KnownNat m) => Enum (Ordinal m) where
  toEnum = fromJust . numToOrdinal
  fromEnum (Ordinal p) = fromIntegral $ natVal p

instance KnownNat m => ToJSON (Ordinal m) where
  toJSON (Ordinal p) = object ["size" .= natVal (Proxy @m), "value" .= natVal p]

instance KnownNat m => FromJSON (Ordinal m) where
  parseJSON = withObject "Ordinal" $ \v -> do
    size <- v .: "size"
    guard (size == natVal (Proxy @m))
    Just o <- numToOrdinal @Integer <$> v .: "value"
    return o

instance KnownNat m => ToJSONKey (Ordinal m)
instance KnownNat m => FromJSONKey (Ordinal m)
