{-# LANGUAGE AllowAmbiguousTypes #-}

module SizedGrid.Coord.Clamped
  ( Clamped(..)
  ) where

import           SizedGrid.Coord.Class
import           SizedGrid.Ordinal

import           Control.Lens          (iso)
import           Data.Aeson
import           Data.AffineSpace
import           GHC.TypeLits
import           System.Random         (Random (..))

-- | A coordinate on a bounded axis: values outside @0 .. n-1@ are clamped to
-- the nearest end rather than rejected or wrapped. Contrast 'Ordinal', which
-- has no way to leave the range at all, and
-- 'SizedGrid.Coord.Periodic.Periodic', which wraps around modularly.
--
-- Clamping is confined to ('.+^'), where 'AffineSpace' forces a total result.
-- It is not the general policy of the type: ('.-.') returns a true signed
-- displacement, and 'SizedGrid.Coord.offsetCoord' reports leaving the axis
-- with 'Nothing' instead of folding back onto the edge.
newtype Clamped (n :: Nat) = Clamped
    { unClamped :: Ordinal n
    } deriving (Eq, Ord)

deriving instance KnownNat n => Show (Clamped n)

deriving instance (KnownNat n, 1 <= n) => Random (Clamped n)
deriving instance (KnownNat n, 1 <= n) => Enum (Clamped n)
deriving instance (KnownNat n, 1 <= n) => Bounded (Clamped n)
deriving instance KnownNat n => ToJSON (Clamped n)
deriving instance KnownNat n => FromJSON (Clamped n)
deriving instance KnownNat n => ToJSONKey (Clamped n)
deriving instance KnownNat n => FromJSONKey (Clamped n)

instance IsCoord Clamped where
  asOrdinal = iso unClamped Clamped

instance (1 <= n, KnownNat n) => Semigroup (Clamped n) where
  Clamped a <> Clamped b =
    Clamped $
    unsafeOrdinal $
    min (ordinalSize @n - 1) (ordinalToInt a + ordinalToInt b)

instance (KnownNat n, 1 <= n) => Monoid (Clamped n) where
  mempty = Clamped minBound
  mappend = (<>)

-- | The difference of two coords is a signed displacement, not a coord, so it
-- is not clamped: clamping it broke @b .+^ (a .-. b) == a@ for every pair with
-- @a < b@ (the difference came back 0, so the round trip landed on @b@).
-- Clamping belongs in ('.+^') alone, which is what keeps the result in range.
instance (1 <= n, KnownNat n) => AffineSpace (Clamped n) where
  type Diff (Clamped n) = Integer
  Clamped a .-. Clamped b = toInteger $ ordinalToInt a - ordinalToInt b
  -- The displacement is an unbounded 'Integer', so the clamp happens there:
  -- reducing to 'Int' first could wrap a large offset into a small one and
  -- clamp to the wrong end.
  Clamped a .+^ b =
    Clamped $
    unsafeOrdinal $
    fromInteger $
    max 0 $
    min (toInteger $ ordinalSize @n - 1) (toInteger (ordinalToInt a) + b)
