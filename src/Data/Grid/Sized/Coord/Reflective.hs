{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Grid.Sized.Coord.Reflective
  ( Reflective(..)
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

-- | A coordinate on a bounded axis that bounces off its walls like a billiard
-- ball: index @-1@ becomes @0@, @-2@ becomes @1@; past the top, index @n@
-- becomes @n - 1@, @n + 1@ becomes @n - 2@. The reflection is at the wall,
-- not around the edge cell, so the edge cell is visited twice in a row.
newtype Reflective (n :: Nat) = Reflective
    { unReflective :: Ordinal n
    } deriving (Eq, Ord)

deriving instance KnownNat n => Show (Reflective n)

deriving instance NFData (Reflective n)

deriving newtype instance Ix (Reflective n)
deriving newtype instance Hashable (Reflective n)
deriving newtype instance Prim (Reflective n)

deriving instance (KnownNat n, 1 <= n) => Random (Reflective n)
deriving instance (KnownNat n, 1 <= n) => Enum (Reflective n)
deriving instance (KnownNat n, 1 <= n) => Bounded (Reflective n)
deriving instance KnownNat n => ToJSON (Reflective n)
deriving instance KnownNat n => FromJSON (Reflective n)
deriving instance KnownNat n => ToJSONKey (Reflective n)
deriving instance KnownNat n => FromJSONKey (Reflective n)

instance (1 <= n, KnownNat n) => U.Universe (Reflective n) where
  universe = allCoordLike

instance (1 <= n, KnownNat n) => U.Finite (Reflective n) where
  universeF = allCoordLike

instance IsCoord Reflective where
  asOrdinal = iso unReflective Reflective
  zeroPosition = Reflective minBound

  -- | A billiard bounce reverses direction on an odd number of wall hits,
  -- which 'bounceAt' already computes for ('.+^').
  axisFrameFlipsIsCoord :: forall n. KnownNat n => Reflective n -> Int -> Bool
  axisFrameFlipsIsCoord (Reflective a) d =
      snd (bounceAt @n (ordinalToInt a) d)

-- | ('.-.') is not bounced: doing so would break @b .+^ (a .-. b) == a@.
instance (1 <= n, KnownNat n) => AffineSpace (Reflective n) where
  type Diff (Reflective n) = Int
  Reflective a .-. Reflective b = ordinalToInt a - ordinalToInt b
  -- This is a retraction of the partial interior action; associativity fails
  -- when a displacement reaches a wall.
  Reflective a .+^ d = Reflective $ unsafeOrdinal $ fst (bounceAt @n (ordinalToInt a) d)

-- | Closed form of a recursive billiard bounce, computed as a triangle wave
-- over one period (@2 * size@) instead of recursing. The displacement is
-- reduced modulo the period first so the addition cannot overflow. @r >=
-- size@ means the remainder landed in the mirrored half, which is exactly
-- when the bounce count's parity flips.
bounceAt :: forall n. KnownNat n => Int -> Int -> (Int, Bool)
bounceAt i d =
    if r < size
        then (r, False)
        else (period - 1 - r, True)
  where
    size = ordinalSize @n
    period = 2 * size
    dx = d `mod` period
    r = (i + dx) `mod` period
