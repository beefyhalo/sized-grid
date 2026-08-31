{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Grid.Sized.Coord.Reflect101
  ( Reflect101 (..),
  )
where

import Control.DeepSeq (NFData)
import Control.Lens (iso)
import Data.Aeson
import Data.AffineSpace
import Data.Grid.Sized.Coord.Class
import Data.Grid.Sized.Coord.Internal.Reflect
import Data.Grid.Sized.Ordinal
import Data.Hashable (Hashable)
import Data.Ix (Ix)
import Data.Primitive.Types (Prim)
import Data.Universe.Class (universe, universeF)
import Data.Universe.Class qualified as U
import GHC.TypeLits
import System.Random (Random (..))

-- | A coordinate on a bounded axis that mirrors around its edge cells rather
-- than across the wall beyond them: index @-1@ becomes @1@ (not @0@), and
-- @-2@ becomes @2@.
--
-- The mirror choice ('AtEdge') is the only thing separating this from
-- "Data.Grid.Sized.Coord.Reflective"; the shared body is in
-- "Data.Grid.Sized.Coord.Internal.Reflect", including why the parity of a
-- step that lands exactly on a mirror resolves to /not/ reflected.
newtype Reflect101 (n :: Nat) = Reflect101
  { unReflect101 :: Ordinal n
  }
  deriving stock (Eq, Ord)
  deriving newtype
    ( Show,
      NFData,
      Ix,
      Hashable,
      Prim,
      ToJSON,
      FromJSON,
      ToJSONKey,
      FromJSONKey
    )

deriving newtype instance (KnownNat n, 1 <= n) => Random (Reflect101 n)

instance (KnownNat n, 1 <= n) => Enum (Reflect101 n) where
  toEnum = reflectToEnum AtEdge
  fromEnum (Reflect101 o) = ordinalToInt o

deriving newtype instance (KnownNat n, 1 <= n) => Bounded (Reflect101 n)

instance (1 <= n, KnownNat n) => U.Universe (Reflect101 n) where
  universe = allCoordLike

instance (1 <= n, KnownNat n) => U.Finite (Reflect101 n) where
  universeF = allCoordLike

instance IsCoord Reflect101 where
  asOrdinal = iso unReflect101 Reflect101
  zeroPosition = Reflect101 minBound

  -- \| A mirror bounce reverses direction on an odd number of wall
  -- /crossings/, which 'reflectFlips' computes for ('.+^'). Landing on a
  -- mirror is not crossing it, so it does not count -- see
  -- "Data.Grid.Sized.Coord.Internal.Reflect" for why that is the only
  -- reading the 'IsCoord' law leaves open.
  axisFrameFlipsIsCoord :: forall n. (KnownNat n) => Reflect101 n -> Int -> Bool
  axisFrameFlipsIsCoord = reflectFlips AtEdge

instance (1 <= n, KnownNat n) => AffineSpace (Reflect101 n) where
  type Diff (Reflect101 n) = Int

  -- \| ('.-.') is not mirrored: doing so would break @b .+^ (a .-. b) == a@.
  (.-.) = ordinalDelta

  -- ('.+^') is a retraction of the partial interior action; associativity
  -- fails when a displacement reaches a wall.
  a .+^ d = reflectPlus AtEdge a d
