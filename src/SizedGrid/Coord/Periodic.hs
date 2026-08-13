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
  -- A torus has no edges, so no value is at one. The default compares against
  -- 0 and @n - 1@, which on this type are simply two cells that happen to be
  -- adjacent across the seam --- nothing distinguishes them from any other
  -- pair, and calling them ends is the mistake this override exists to stop.
  -- It is the same fact as 'offsetIsCoord' being total above, said where a
  -- caller asks it directly: 'SizedGrid.Coord.isCorner' returns 'False' on an
  -- all-'Periodic' coord because of this line.
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
    -- The displacement is reduced into @[0, size)@ before it is added, which is
    -- what keeps the addition in range: @mod@ by a positive divisor is
    -- non-negative and smaller than it whatever the sign of the numerator, so
    -- the sum of two such values is below @2 * size@ and cannot overflow. The
    -- reduction has to come first --- @(i + b) `mod` size@ would overflow for a
    -- @b@ near 'maxBound' and wrap to the wrong residue.
    --
    -- This took an unbounded 'Integer' until sized-grid-0tj, for the overflow
    -- safety that the early @mod@ gives instead. 'Clamped' makes the same trade
    -- by comparison rather than by @mod@; see the note there.
    Periodic a .+^ b =
        let size = ordinalSize @n
            offset = b `mod` size
        in Periodic $ unsafeOrdinal $ (ordinalToInt a + offset) `mod` size
