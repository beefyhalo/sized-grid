module Data.Grid.Sized.Coord.Periodic
  ( Periodic (..),
  )
where

import Control.DeepSeq (NFData)
import Control.Lens
import Data.AdditiveGroup
import Data.Aeson
import Data.AffineSpace
import Data.Grid.Sized.Coord.Class
import Data.Grid.Sized.Ordinal
import Data.Group (Abelian, Cyclic (..), Group (..))
import Data.Hashable (Hashable)
import Data.Ix (Ix)
import Data.Primitive.Types (Prim)
import Data.Universe.Class (universe, universeF)
import Data.Universe.Class qualified as U
import GHC.TypeLits
import System.Random

-- | A coordinate with periodic boundaries, as if on a taurus
newtype Periodic (n :: Nat) = Periodic
  { unPeriodic :: Ordinal n
  }
  deriving stock (Eq, Ord)
  deriving newtype
    ( Show,
      NFData,
      Ix,
      Hashable,
      Prim,
      ToJSON,
      ToJSONKey,
      FromJSON,
      FromJSONKey
    )

deriving newtype instance (1 <= n, KnownNat n) => Random (Periodic n)

instance (1 <= n, KnownNat n) => U.Universe (Periodic n) where
  universe = allCoordLike

instance (1 <= n, KnownNat n) => U.Finite (Periodic n) where
  universeF = allCoordLike

instance (1 <= n, KnownNat n) => Enum (Periodic n) where
  toEnum x = Periodic $ unsafeOrdinal $ x `mod` ordinalSize @n
  fromEnum (Periodic o) = ordinalToInt o

  -- Overridden because the default 'enumFrom'/'enumFromThen' count up in
  -- Int forever: 'toEnum' wraps instead of erroring, so the walk-off-the-
  -- end that stops 'Ordinal's default never happens here, and the list
  -- would repeat silently. A torus has no natural end to stop at, so each
  -- lands after exactly one lap around the axis.
  enumFrom a = take (ordinalSize @n) (iterate succ a)
  enumFromThen a b =
    take (ordinalSize @n) (iterate (\x -> toEnum (fromEnum x + step)) a)
    where
      step = fromEnum b - fromEnum a
  enumFromTo a b = map toEnum [fromEnum a .. fromEnum b]
  enumFromThenTo a b c = map toEnum [fromEnum a, fromEnum b .. fromEnum c]

instance (1 <= n, KnownNat n) => Bounded (Periodic n) where
  minBound = Periodic minBound
  maxBound = Periodic maxBound

instance IsCoord Periodic where
  asOrdinal = iso unPeriodic Periodic

  -- A torus has no edges, so the offset never fails.
  offsetIsCoord c d = Just (c .+^ d)

  -- The shorter way round, not the straight difference.
  axisDistanceIsCoord :: forall n. (KnownNat n) => Periodic n -> Periodic n -> Int
  axisDistanceIsCoord (Periodic a) (Periodic b) =
    let size = ordinalSize @n
        d = abs (ordinalToInt a - ordinalToInt b)
     in min d (size - d)

  -- A torus has no edges, so no value is at one.
  axisBoundaryIsCoord _ = Nothing

instance (1 <= n, KnownNat n) => Semigroup (Periodic n) where
  Periodic a <> Periodic b =
    Periodic $
      unsafeOrdinal $
        (ordinalToInt a + ordinalToInt b) `mod` ordinalSize @n

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

  -- (.-.) returns the shortest signed representative in Z/nZ. At the
  -- half-turn of an even-sized axis, the raw difference determines the sign.
  Periodic a .-. Periodic b =
    let size = ordinalSize @n
        difference = ordinalToInt a - ordinalToInt b
        half = size `div` 2
     in if difference > half
          then difference - size
          else
            if difference < negate half
              then difference + size
              else difference

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

-- | Z/nZ is commutative: translation on a torus does not care about order.
instance (1 <= n, KnownNat n) => Abelian (Periodic n)

-- | 1 generates Z/nZ: repeated translation by one step reaches every
-- position, since 'pow' on 'Periodic'\'s additive monoid is repeated '.+^'.
instance (1 <= n, KnownNat n) => Cyclic (Periodic n) where
  generator = toEnum 1

-- | A torus has no edges: 'offsetIsCoord' never refuses, and the monoid
-- operation ('<>' via 'AdditiveGroup') is exactly '.+^'. See
-- 'Data.Grid.Sized.Coord.Class.Boundaryless' for the laws this instance
-- promises.
instance (1 <= n, KnownNat n) => Boundaryless (Periodic n)
