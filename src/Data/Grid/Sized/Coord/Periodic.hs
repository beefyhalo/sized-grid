module Data.Grid.Sized.Coord.Periodic
  ( Periodic(..)
  ) where

import           Data.Grid.Sized.Coord.Class
import           Data.Grid.Sized.Ordinal

import           Control.DeepSeq     (NFData)
import           Control.Lens
import           Data.AdditiveGroup
import           Data.Aeson
import           Data.AffineSpace
import           Data.Group          (Group (..))
import           GHC.TypeLits
import           System.Random

-- | A coordinate with periodic boundaries, as if on a taurus
newtype Periodic (n :: Nat) = Periodic
    { unPeriodic :: Ordinal n
    } deriving (Eq, Ord)

deriving instance KnownNat n => Show (Periodic n)

deriving instance NFData (Periodic n)

deriving instance (1 <= n, KnownNat n) => Random (Periodic n)

deriving instance KnownNat n => ToJSON (Periodic n)
deriving instance KnownNat n => ToJSONKey (Periodic n)
deriving instance KnownNat n => FromJSON (Periodic n)
deriving instance KnownNat n => FromJSONKey (Periodic n)

instance (1 <= n, KnownNat n) => Enum (Periodic n) where
    toEnum x = Periodic $ unsafeOrdinal $ x `mod` ordinalSize @n
    fromEnum (Periodic o) = ordinalToInt o

instance IsCoord Periodic where
  asOrdinal = iso unPeriodic Periodic
  -- A torus has no edges, so the offset never fails.
  offsetIsCoord c d = Just (c .+^ d)
  -- The shorter way round, not the straight difference.
  axisDistanceIsCoord :: forall n. KnownNat n => Periodic n -> Periodic n -> Int
  axisDistanceIsCoord (Periodic a) (Periodic b) =
      let size = ordinalSize @n
          d = abs (ordinalToInt a - ordinalToInt b)
      in min d (size - d)
  -- A torus has no edges, so no value is at one.
  axisBoundaryIsCoord _ = Nothing

instance (1 <= n, KnownNat n) => Semigroup (Periodic n) where
    Periodic a <> Periodic b =
        Periodic $
        unsafeOrdinal $ (ordinalToInt a + ordinalToInt b) `mod` ordinalSize @n

instance (1 <= n, KnownNat n) => Monoid (Periodic n) where
    mappend = (<>)
    mempty = Periodic minBound

instance (1 <= n, KnownNat n) => AdditiveGroup (Periodic n) where
    zeroV = mempty
    (^+^) = (<>)
    negateV (Periodic o) =
        Periodic $ unsafeOrdinal $ negate (ordinalToInt o) `mod` ordinalSize @n

instance (1 <= n, KnownNat n) => AffineSpace (Periodic n) where
    type Diff (Periodic n) = Int
    Periodic a .-. Periodic b =
        (ordinalToInt a - ordinalToInt b) `mod` ordinalSize @n
    -- The displacement is reduced into [0, size) before adding, so the sum
    -- stays below 2 * size and cannot overflow; reducing after adding could
    -- overflow first for a b near maxBound.
    Periodic a .+^ b =
        let size = ordinalSize @n
            offset = b `mod` size
        in Periodic $ unsafeOrdinal $ (ordinalToInt a + offset) `mod` size

-- | Periodic n IS Z/nZ: the monoid operation already inherited from
-- 'AdditiveGroup' is its own inverse under negation.
instance (1 <= n, KnownNat n) => Group (Periodic n) where
    invert = negateV

-- | A torus has no edges: 'offsetIsCoord' never refuses, and the monoid
-- operation ('<>' via 'AdditiveGroup') is exactly '.+^'. See
-- 'Data.Grid.Sized.Coord.Class.Boundaryless' for the laws this instance
-- promises.
instance (1 <= n, KnownNat n) => Boundaryless (Periodic n)
