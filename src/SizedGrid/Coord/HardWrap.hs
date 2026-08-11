{-# LANGUAGE AllowAmbiguousTypes #-}

module SizedGrid.Coord.HardWrap
  ( HardWrap(..)
  ) where

import           SizedGrid.Coord.Class
import           SizedGrid.Ordinal

import           Control.Lens          (iso)
import           Data.Aeson
import           Data.AffineSpace
import           GHC.TypeLits
import           System.Random         (Random (..))

-- | A coordinate that clamps its numbers
newtype HardWrap (n :: Nat) = HardWrap
    { unHardWrap :: Ordinal n
    } deriving (Eq, Ord)

deriving instance KnownNat n => Show (HardWrap n)

deriving instance (KnownNat n, 1 <= n) => Random (HardWrap n)
deriving instance (KnownNat n, 1 <= n) => Enum (HardWrap n)
deriving instance (KnownNat n, 1 <= n) => Bounded (HardWrap n)
deriving instance KnownNat n => ToJSON (HardWrap n)
deriving instance KnownNat n => FromJSON (HardWrap n)
deriving instance KnownNat n => ToJSONKey (HardWrap n)
deriving instance KnownNat n => FromJSONKey (HardWrap n)

instance IsCoord HardWrap where
  asOrdinal = iso unHardWrap HardWrap

instance (1 <= n, KnownNat n) => Semigroup (HardWrap n) where
  HardWrap a <> HardWrap b =
    HardWrap $
    unsafeOrdinal $
    min (ordinalSize @n - 1) (ordinalToInt a + ordinalToInt b)

instance (KnownNat n, 1 <= n) => Monoid (HardWrap n) where
  mempty = HardWrap minBound
  mappend = (<>)

-- | The difference of two coords is a signed displacement, not a coord, so it
-- is not clamped: clamping it broke @b .+^ (a .-. b) == a@ for every pair with
-- @a < b@ (the difference came back 0, so the round trip landed on @b@).
-- Clamping belongs in ('.+^') alone, which is what keeps the result in range.
instance (1 <= n, KnownNat n) => AffineSpace (HardWrap n) where
  type Diff (HardWrap n) = Integer
  HardWrap a .-. HardWrap b = toInteger $ ordinalToInt a - ordinalToInt b
  -- The displacement is an unbounded 'Integer', so the clamp happens there:
  -- reducing to 'Int' first could wrap a large offset into a small one and
  -- clamp to the wrong end.
  HardWrap a .+^ b =
    HardWrap $
    unsafeOrdinal $
    fromInteger $
    max 0 $
    min (toInteger $ ordinalSize @n - 1) (toInteger (ordinalToInt a) + b)
