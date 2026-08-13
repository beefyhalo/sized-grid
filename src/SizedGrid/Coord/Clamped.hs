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
  type Diff (Clamped n) = Int
  Clamped a .-. Clamped b = ordinalToInt a - ordinalToInt b
  -- Clamped by comparison, not by arithmetic.
  --
  -- The obvious body --- add, then @max 0 . min (n - 1)@ back into range --- has
  -- to do the addition before it can clamp, and @i + b@ overflows for a @b@ near
  -- 'maxBound', wrapping a large positive offset into a negative sum and
  -- clamping to the wrong end. That is a real hazard, and it is what the
  -- 'Integer' this used to take was buying off: every call paid a 'toInteger' per
  -- operand, an 'Integer' add, min and max, and a 'fromInteger' back.
  --
  -- Comparing instead of adding buys it off for nothing. Both bounds are built
  -- from the coord and the size alone --- @hi - i@ lies in @[0, hi]@ and
  -- @negate i@ in @[-hi, 0]@ --- so neither can overflow, and @b@ is only ever
  -- compared against them, never added to anything until it is known to be
  -- small. The final branch has @negate i <= b <= hi - i@ and so
  -- @0 <= i + b <= hi@: in range by construction, which is what makes
  -- 'unsafeOrdinal' safe here.
  --
  -- What this is worth, measured rather than assumed (sized-grid-0tj). On a bare
  -- axis, 360,000 offsets went from 8.41 ms and 22 MB to 2.20 ms and 94 KB: the
  -- per-axis arithmetic no longer allocates at all.
  --
  -- The same 360,000 offsets through a two-axis 'SizedGrid.Coord.Coord' went
  -- from 32.4 ms and 143 MB to 28.6 ms and 126 MB --- 11%, not the order of
  -- magnitude the issue predicted. So 126 of those 143 MB were never the
  -- 'Integer'; they are the fold over the axis list in
  -- @'Data.AffineSpace.AffineSpace' ('SizedGrid.Coord.Coord' cs)@, which is
  -- self-recursive and polymorphic and so cannot unroll. That is still there.
  -- Both benchmarks are in @bench\/Main.hs@ and the pair is what separates the
  -- two costs; the issue's own decomposition removed them together and so
  -- attributed all of it here.
  Clamped a .+^ b
    | b > hi - i = Clamped maxBound
    | b < negate i = Clamped minBound
    | otherwise = Clamped $ unsafeOrdinal $ i + b
    where
      i = ordinalToInt a
      hi = ordinalSize @n - 1
