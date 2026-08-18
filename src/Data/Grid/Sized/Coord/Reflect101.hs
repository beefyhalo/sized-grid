{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Grid.Sized.Coord.Reflect101
  ( Reflect101(..)
  ) where

import           Data.Grid.Sized.Coord.Class
import           Data.Grid.Sized.Ordinal

import           Control.Lens          (iso)
import           Data.Aeson
import           Data.AffineSpace
import           GHC.TypeLits
import           System.Random         (Random (..))

-- | A coordinate on a bounded axis that mirrors around its edge cells rather
-- than across the wall beyond them: index @-1@ becomes @1@ (not @0@), and
-- @-2@ becomes @2@.
newtype Reflect101 (n :: Nat) = Reflect101
    { unReflect101 :: Ordinal n
    } deriving (Eq, Ord)

deriving instance KnownNat n => Show (Reflect101 n)

deriving instance (KnownNat n, 1 <= n) => Random (Reflect101 n)
deriving instance (KnownNat n, 1 <= n) => Enum (Reflect101 n)
deriving instance (KnownNat n, 1 <= n) => Bounded (Reflect101 n)
deriving instance KnownNat n => ToJSON (Reflect101 n)
deriving instance KnownNat n => FromJSON (Reflect101 n)
deriving instance KnownNat n => ToJSONKey (Reflect101 n)
deriving instance KnownNat n => FromJSONKey (Reflect101 n)

instance IsCoord Reflect101 where
  asOrdinal = iso unReflect101 Reflect101
  zeroPosition = Reflect101 minBound

  -- | A mirror bounce reverses direction on an odd number of wall hits,
  -- which 'mirrorAt' already computes for ('.+^').
  axisFrameFlipsIsCoord :: forall n. KnownNat n => Reflect101 n -> Int -> Bool
  axisFrameFlipsIsCoord (Reflect101 a) d =
      snd (mirrorAt @n (ordinalToInt a) d)

-- | ('.-.') is not mirrored: doing so would break @b .+^ (a .-. b) == a@.
instance (1 <= n, KnownNat n) => AffineSpace (Reflect101 n) where
  type Diff (Reflect101 n) = Int
  Reflect101 a .-. Reflect101 b = ordinalToInt a - ordinalToInt b
  Reflect101 a .+^ d = Reflect101 $ unsafeOrdinal $ fst (mirrorAt @n (ordinalToInt a) d)

-- | Billiard bounce with period @2 * m@, @m = size - 1@. @m == 0@ has no
-- distinct neighbour to mirror around, so it is special-cased rather than
-- divided by. @r == m@ is the far mirror's fixed point, genuinely ambiguous
-- in parity; @>=@ (not @>@) resolves it as reflected.
mirrorAt :: forall n. KnownNat n => Int -> Int -> (Int, Bool)
mirrorAt i d
    | m == 0 = (0, False)
    | otherwise =
        if r >= m
            then (period - r, True)
            else (r, False)
  where
    m = ordinalSize @n - 1
    period = 2 * m
    dx = d `mod` period
    r = (i + dx) `mod` period
