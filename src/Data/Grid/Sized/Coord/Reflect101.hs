{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Grid.Sized.Coord.Reflect101
  ( Reflect101(..)
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

-- | A coordinate on a bounded axis that mirrors around its edge cells rather
-- than across the wall beyond them: index @-1@ becomes @1@ (not @0@), and
-- @-2@ becomes @2@.
newtype Reflect101 (n :: Nat) = Reflect101
    { unReflect101 :: Ordinal n
  } deriving stock (Eq, Ord)
      deriving newtype (Show, NFData, Ix, Hashable, Prim, ToJSON, FromJSON,
                        ToJSONKey, FromJSONKey)

deriving newtype instance (KnownNat n, 1 <= n) => Random (Reflect101 n)
deriving newtype instance (KnownNat n, 1 <= n) => Enum (Reflect101 n)
deriving newtype instance (KnownNat n, 1 <= n) => Bounded (Reflect101 n)

instance (1 <= n, KnownNat n) => U.Universe (Reflect101 n) where
  universe = allCoordLike

instance (1 <= n, KnownNat n) => U.Finite (Reflect101 n) where
  universeF = allCoordLike

instance IsCoord Reflect101 where
  asOrdinal = iso unReflect101 Reflect101
  zeroPosition = Reflect101 minBound

  -- | A mirror bounce reverses direction on an odd number of wall /crossings/,
  -- which 'mirrorAt' already computes for ('.+^'). Landing on a mirror is not
  -- crossing it, so it does not count -- see 'mirrorAt' for why that is the
  -- only reading the 'IsCoord' law leaves open.
  axisFrameFlipsIsCoord :: forall n. KnownNat n => Reflect101 n -> Int -> Bool
  axisFrameFlipsIsCoord (Reflect101 a) d =
      snd (mirrorAt @n (ordinalToInt a) d)

-- | ('.-.') is not mirrored: doing so would break @b .+^ (a .-. b) == a@.
instance (1 <= n, KnownNat n) => AffineSpace (Reflect101 n) where
  type Diff (Reflect101 n) = Int
  Reflect101 a .-. Reflect101 b = ordinalToInt a - ordinalToInt b
  -- This is a retraction of the partial interior action; associativity fails
  -- when a displacement reaches a wall.
  Reflect101 a .+^ d = Reflect101 $ unsafeOrdinal $ fst (mirrorAt @n (ordinalToInt a) d)

-- | Billiard bounce with period @2 * m@, @m = size - 1@. @m == 0@ has no
-- distinct neighbour to mirror around, so it is special-cased rather than
-- divided by.
--
-- @r == 0@ and @r == m@ are the two mirrors' fixed points, genuinely
-- ambiguous in parity, and @>@ (not @>=@) resolves both the same way: /not/
-- reflected. Landing on a mirror is not crossing it.
--
-- The position does not depend on that choice -- at @r == m@ the two branches
-- agree, since @period - m == m@ -- so it is a choice about the flag alone,
-- and the flag is what 'axisFrameFlipsIsCoord' has to get right.
-- @>=@ resolved it the other way, which made this the only axis in the
-- library where a step the bounds check /accepts/ also reports a flip: on a
-- 5-cell axis, all five steps landing exactly on cell 4, plus the identity
-- displacement standing on it. That breaks the 'IsCoord' law that a
-- successful checked step has not hit a wall, and it made a checked walker
-- turn around one cell early where
-- "Data.Grid.Sized.Coord.Reflective" walks to the wall (sized-grid-c0s9).
mirrorAt :: forall n. KnownNat n => Int -> Int -> (Int, Bool)
mirrorAt i d
    | m == 0 = (0, False)
    | otherwise =
        if r > m
            then (period - r, True)
            else (r, False)
  where
    m = ordinalSize @n - 1
    period = 2 * m
    dx = d `mod` period
    r = (i + dx) `mod` period
