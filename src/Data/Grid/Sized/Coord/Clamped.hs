module Data.Grid.Sized.Coord.Clamped
  ( Clamped(..)
  ) where

import           Data.Grid.Sized.Coord.Class
import           Data.Grid.Sized.Ordinal

import           Control.DeepSeq       (NFData)
import           Control.Lens          (iso)
import           Data.Aeson
import           Data.AffineSpace
import           Data.Hashable        (Hashable)
import           Data.Ix              (Ix)
import           Data.Primitive.Types  (Prim)
import           Data.Universe.Class  (universe, universeF)
import qualified Data.Universe.Class  as U
import           GHC.TypeLits
import           System.Random         (Random (..))

-- | A coordinate on a bounded axis: values outside @0 .. n-1@ are clamped to
-- the nearest end rather than rejected or wrapped.
newtype Clamped (n :: Nat) = Clamped
    { unClamped :: Ordinal n
    } deriving (Eq, Ord)

deriving instance KnownNat n => Show (Clamped n)

deriving instance NFData (Clamped n)

deriving newtype instance Ix (Clamped n)
deriving newtype instance Hashable (Clamped n)
deriving newtype instance Prim (Clamped n)

deriving instance (KnownNat n, 1 <= n) => Random (Clamped n)
deriving instance (KnownNat n, 1 <= n) => Enum (Clamped n)
deriving instance (KnownNat n, 1 <= n) => Bounded (Clamped n)
deriving instance KnownNat n => ToJSON (Clamped n)
deriving instance KnownNat n => FromJSON (Clamped n)
deriving instance KnownNat n => ToJSONKey (Clamped n)
deriving instance KnownNat n => FromJSONKey (Clamped n)

instance (1 <= n, KnownNat n) => U.Universe (Clamped n) where
  universe = allCoordLike

instance (1 <= n, KnownNat n) => U.Finite (Clamped n) where
  universeF = allCoordLike

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

instance (1 <= n, KnownNat n) => AffineSpace (Clamped n) where
  type Diff (Clamped n) = Int
  Clamped a .-. Clamped b = ordinalToInt a - ordinalToInt b
  -- This is a retraction of the partial interior action; associativity fails
  -- when a displacement reaches a wall.
  -- Clamped by comparison, not by addition-then-clamp: adding first can
  -- overflow for a @b@ near 'maxBound' and clamp to the wrong end. Both
  -- bounds compared against here are built from @i@ and the size alone, so
  -- neither they nor the final @i + b@ can overflow.
  Clamped a .+^ b
    | b > hi - i = Clamped maxBound
    | b < negate i = Clamped minBound
    | otherwise = Clamped $ unsafeOrdinal $ i + b
    where
      i = ordinalToInt a
      hi = ordinalSize @n - 1
