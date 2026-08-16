{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Grid.Sized.Coord.Reflective
  ( Reflective(..)
  ) where

import           Data.Grid.Sized.Coord.Class
import           Data.Grid.Sized.Ordinal

import           Control.Lens          (iso)
import           Data.Aeson
import           Data.AffineSpace
import           GHC.TypeLits
import           System.Random         (Random (..))

-- | A coordinate on a bounded axis that bounces off its walls like a billiard
-- ball, rather than stopping at them ('Data.Grid.Sized.Coord.Clamped.Clamped')
-- or wrapping past them ('Data.Grid.Sized.Coord.Periodic.Periodic'): stepping
-- past an edge reflects the excess back inside. Index @-1@ becomes @0@, @-2@
-- becomes @1@; past the top, index @n@ (one past the last valid index) becomes
-- @n - 1@, @n + 1@ becomes @n - 2@.
--
-- The reflection is at the /wall/ --- the seam between @-1@ and @0@, or
-- between @n - 1@ and @n@ --- not around the edge cell itself. That is what
-- distinguishes this from
-- 'Data.Grid.Sized.Coord.Reflect101.Reflect101', which reflects around the
-- edge cell and so never repeats it on the way back in; here the edge cell is
-- visited twice in a row, once outbound and once on the bounce, which is the
-- standard boundary that double-weights the edge cell in a convolution.
--
-- Like 'Data.Grid.Sized.Coord.Clamped.Clamped', the bounce is confined to
-- ('.+^'), where 'AffineSpace' forces a total result. It is not the general
-- policy of the type: ('.-.') returns a true signed displacement, and
-- 'Data.Grid.Sized.Coord.offsetIsCoord' reports leaving the axis with
-- 'Nothing' rather than folding the offset back inside. That is exactly the
-- split 'Data.Grid.Sized.Coord.Clamped.Clamped' documents at
-- @Coord\/Clamped.hs:19-22@, copied here: the type is total the same way, and
-- honest only because the caller named it 'Reflective'.
newtype Reflective (n :: Nat) = Reflective
    { unReflective :: Ordinal n
    } deriving (Eq, Ord)

deriving instance KnownNat n => Show (Reflective n)

deriving instance (KnownNat n, 1 <= n) => Random (Reflective n)
deriving instance (KnownNat n, 1 <= n) => Enum (Reflective n)
deriving instance (KnownNat n, 1 <= n) => Bounded (Reflective n)
deriving instance KnownNat n => ToJSON (Reflective n)
deriving instance KnownNat n => FromJSON (Reflective n)
deriving instance KnownNat n => ToJSONKey (Reflective n)
deriving instance KnownNat n => FromJSONKey (Reflective n)

instance IsCoord Reflective where
  asOrdinal = iso unReflective Reflective
  zeroPosition = Reflective minBound
  -- 'axisDistanceIsCoord' and 'axisBoundaryIsCoord' are both left at their
  -- defaults, which measure straight and report real edges. That is a
  -- deliberate decision and not an oversight: unlike
  -- 'Data.Grid.Sized.Coord.Periodic.Periodic', which overrides both because a
  -- torus genuinely has no far side and no edge, a reflective axis is still
  -- bounded. The bounce is a property of ('.+^') alone --- two cells here are
  -- exactly as far apart as their indices say, whatever route a walker takes
  -- between them, and the wall a walker bounces off is still the wall. Pinned
  -- by property tests in @Test.Boundary@ rather than left to be rediscovered.

-- | The difference of two coords is a signed displacement, not a coord, so it
-- is not bounced: bouncing it would break @b .+^ (a .-. b) == a@ the same way
-- clamping it does on 'Data.Grid.Sized.Coord.Clamped.Clamped', for the reason
-- given there.
instance (1 <= n, KnownNat n) => AffineSpace (Reflective n) where
  type Diff (Reflective n) = Int
  Reflective a .-. Reflective b = ordinalToInt a - ordinalToInt b
  -- The closed form of
  --
  -- > bounceAxis size x dx = go (x + dx) where
  -- >   go i | i < 0     = go (negate i - 1)
  -- >        | i >= size = go (2 * size - 1 - i)
  -- >        | otherwise = i
  --
  -- A billiard bounce off two walls @size@ apart is periodic with period
  -- @2 * size@: unfolding the reflections turns @go@ into a triangle wave over
  -- one period, identity on the first half and mirrored on the second, which
  -- is exactly what @r@ and the branch below compute without recursing.
  --
  -- The displacement is reduced modulo the period before it is added, which is
  -- what keeps the addition from overflowing: @i@ is already in @[0, size)@
  -- and @dx@ in @[0, period)@ after the first @mod@, so their sum is below
  -- @3 * size@ and cannot wrap a bounded 'Int' whatever @d@ was. This is the
  -- same trade 'Data.Grid.Sized.Coord.Periodic.Periodic' makes in its own
  -- ('.+^'); see the note there.
  Reflective a .+^ d =
      Reflective $
      unsafeOrdinal $
      if r < size
          then r
          else period - 1 - r
    where
      size = ordinalSize @n
      period = 2 * size
      dx = d `mod` period
      r = (ordinalToInt a + dx) `mod` period
