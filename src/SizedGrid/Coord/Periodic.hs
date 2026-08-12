{-# LANGUAGE AllowAmbiguousTypes #-}

module SizedGrid.Coord.Periodic
  ( Periodic(..)
  ) where

import           SizedGrid.Coord.Class
import           SizedGrid.Ordinal

import           Control.Lens
import           Data.AdditiveGroup
import           Data.Aeson
import           Data.AffineSpace
import           GHC.TypeLits
import           System.Random

-- | A coordinate with periodic boundaries, as if on a taurus
newtype Periodic (n :: Nat) = Periodic
    { unPeriodic :: Ordinal n
    } deriving (Eq, Ord)

deriving instance KnownNat n => Show (Periodic n)

deriving instance (1 <= n, KnownNat n) => Random (Periodic n)

deriving instance KnownNat n => ToJSON (Periodic n)
deriving instance KnownNat n => ToJSONKey (Periodic n)
deriving instance KnownNat n => FromJSON (Periodic n)
deriving instance KnownNat n => FromJSONKey (Periodic n)

-- | Every operation below reduces into @[0, n)@ before it builds an 'Ordinal',
-- which is what makes 'unsafeOrdinal' safe here: @`mod`@ with a positive
-- divisor is non-negative and smaller than the divisor.
instance (1 <= n, KnownNat n) => Enum (Periodic n) where
    -- This used to wrap modulo @maxCoordSize@, which is @n - 1@, so
    -- @toEnum 2 :: Periodic 3@ came back as 0 and @fromEnum . toEnum@ was not
    -- the identity on the type's own range. The period of a @Periodic n@ is
    -- @n@.
    toEnum x = Periodic $ unsafeOrdinal $ x `mod` ordinalSize @n
    fromEnum (Periodic o) = ordinalToInt o

instance IsCoord Periodic where
  asOrdinal = iso unPeriodic Periodic
  -- A torus has no edges, so the checked offset never fails. Delegating to
  -- ('.+^') keeps the one definition of what wrapping means in the
  -- 'AffineSpace' instance below, where the law tests already cover it.
  offsetIsCoord c d = Just (c .+^ d)
  -- Two ways round, and the shorter one is the distance. The default measures
  -- straight, which would report @n - 1@ for two cells that are actually
  -- adjacent across the seam.
  --
  -- The instance signature is what brings @n@ into scope for @ordinalSize@:
  -- the instance head is @IsCoord Periodic@, so the size lives only in the
  -- method's own @forall@. It drops the class's @1 <= n@, which this body does
  -- not use --- an instance signature may be more general, and carrying the
  -- constraint here would only earn a redundancy warning.
  axisDistanceIsCoord :: forall n. KnownNat n => Periodic n -> Periodic n -> Int
  axisDistanceIsCoord (Periodic a) (Periodic b) =
      let size = ordinalSize @n
          d = abs (ordinalToInt a - ordinalToInt b)
      in min d (size - d)

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
    type Diff (Periodic n) = Integer
    Periodic a .-. Periodic b =
        toInteger $ (ordinalToInt a - ordinalToInt b) `mod` ordinalSize @n
    Periodic a .+^ b =
        let size = ordinalSize @n
            -- The displacement is an unbounded 'Integer', so it is reduced into
            -- @[0, size)@ there; everything after that is 'Int' arithmetic on
            -- two values below @size@, which cannot overflow.
            offset = fromInteger $ b `mod` toInteger size
        in Periodic $ unsafeOrdinal $ (ordinalToInt a + offset) `mod` size
