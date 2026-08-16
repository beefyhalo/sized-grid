{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Grid.Sized.Coord.Reflect101
  ( Reflect101(..)
  ) where

import           Data.Grid.Sized.Coord.Class
import           Data.Grid.Sized.Ordinal

import           Control.Lens          (iso)
import           Data.Aeson
import           Data.AffineSpace
import           GHC.TypeLits
import           System.Random         (Random (..))

-- | A coordinate on a bounded axis that mirrors around its edge cells rather
-- than across the wall beyond them: stepping past an edge reflects around the
-- boundary value itself, so index @-1@ becomes @1@ (not @0@, which is what
-- 'Data.Grid.Sized.Coord.Reflective.Reflective' gives), and @-2@ becomes @2@.
-- Past the top, index @n@ (one past the last valid index) becomes @n - 2@.
--
-- This is the standard boundary for image-processing kernels such as Sobel or
-- Laplacian, where the alternative --- reflecting across the wall and so
-- repeating the edge cell --- double-weights that cell. Mirroring around it
-- instead visits every cell near the edge exactly as often as its interior
-- neighbours, at the cost that an axis of size 1 has no cell to mirror
-- around: see the note on ('.+^').
--
-- Like 'Data.Grid.Sized.Coord.Reflective.Reflective', the mirroring is
-- confined to ('.+^'), where 'AffineSpace' forces a total result: ('.-.')
-- returns a true signed displacement, and
-- 'Data.Grid.Sized.Coord.offsetIsCoord' reports leaving the axis with
-- 'Nothing' instead of folding the offset back inside. Total for the same
-- reason and the same way as 'Data.Grid.Sized.Coord.Clamped.Clamped' at
-- @Coord\/Clamped.hs:19-22@: honest only because the caller named it
-- 'Reflect101'.
newtype Reflect101 (n :: Nat) = Reflect101
    { unReflect101 :: Ordinal n
    } deriving (Eq, Ord)

deriving instance KnownNat n => Show (Reflect101 n)

deriving instance (KnownNat n, 1 <= n) => Random (Reflect101 n)
deriving instance (KnownNat n, 1 <= n) => Enum (Reflect101 n)
deriving instance (KnownNat n, 1 <= n) => Bounded (Reflect101 n)
deriving instance KnownNat n => ToJSON (Reflect101 n)
deriving instance KnownNat n => FromJSON (Reflect101 n)
deriving instance KnownNat n => ToJSONKey (Reflect101 n)
deriving instance KnownNat n => FromJSONKey (Reflect101 n)

instance IsCoord Reflect101 where
  asOrdinal = iso unReflect101 Reflect101
  zeroPosition = Reflect101 minBound
  -- Left at their defaults for the same reason
  -- 'Data.Grid.Sized.Coord.Reflective.Reflective' leaves them: the mirroring
  -- is a property of ('.+^') alone, and this axis is still bounded, so
  -- distance is measured straight and the two ends still report as edges.
  -- Pinned by property tests in @Test.Boundary@.

  -- | The seam rule's frame half (sized-grid-o1n), for the same reason and by
  -- the same sharing as
  -- 'Data.Grid.Sized.Coord.Reflective.Reflective''s: a mirror bounce reverses
  -- direction on an odd number of wall hits, which 'mirrorAt' already
  -- computes for ('.+^').
  axisFrameFlipsIsCoord :: forall n. KnownNat n => Reflect101 n -> Int -> Bool
  axisFrameFlipsIsCoord (Reflect101 a) d =
      snd (mirrorAt @n (ordinalToInt a) d)

-- | Not mirrored, for the reason 'Data.Grid.Sized.Coord.Reflective.Reflective'
-- gives at its own ('.-.'): mirroring the difference would break
-- @b .+^ (a .-. b) == a@.
instance (1 <= n, KnownNat n) => AffineSpace (Reflect101 n) where
  type Diff (Reflect101 n) = Int
  Reflect101 a .-. Reflect101 b = ordinalToInt a - ordinalToInt b
  Reflect101 a .+^ d = Reflect101 $ unsafeOrdinal $ fst (mirrorAt @n (ordinalToInt a) d)

-- | Mirroring around the edge /cell/ rather than the wall beyond it is a
-- billiard bounce on an axis stretched by one at each end: the two edge
-- cells, at @0@ and @m = size - 1@, are each their own mirror image, so the
-- period is @2 * m@ rather than the @2 * size@ of
-- 'Data.Grid.Sized.Coord.Reflective.Reflective'. The branch is the same
-- triangle wave with @m@ in place of @size@ and a strict @>@ in place of
-- @>=@ for the /position/, since @r == m@ is the fixed point of the mirror
-- rather than a value that still has a partner past it --- both branches
-- agree at @r == m@ (@period - m == m@), so it makes no difference to the
-- position which side of the boundary owns it.
--
-- @m == 0@ is the axis of size 1, which has one value and no distinct
-- neighbour to mirror around: every displacement lands back on it, so the
-- general formula's @2 * m@ period is degenerate and is special-cased
-- directly rather than divided by. It never bounces, so it never flips.
--
-- The displacement is reduced modulo the period before it is added, for the
-- overflow reason given on
-- 'Data.Grid.Sized.Coord.Reflective.Reflective'\'s ('.+^').
--
-- == Why the flip half uses @>=@ where the position half uses @>@
--
-- Unlike 'Data.Grid.Sized.Coord.Reflective.Reflective', whose two walls sit
-- at half-integers and so are never landed on exactly, @r == m@ here /is/ a
-- real, reachable position --- the fixed point of the far mirror --- and it
-- is genuinely ambiguous which orientation it carries: unfolding a raw
-- displacement that lands there can equally be described as one reflection
-- or as a translation composed with a different reflection, and those two
-- descriptions disagree on parity even though they agree on where you land.
-- 'Data.Grid.Sized.Coord.Reflective.Reflective' has no such point, which is
-- why its own recursive bounce count is unambiguous everywhere and this
-- one is not (see the note on @reflect101FlipRef@ in @Test.Reflective@,
-- where the property test excludes exactly this case).
--
-- So @r == m@'s flip is a choice, not a derivation, and @>=@ records the one
-- made here: the far wall's fixed point sides with the reflected copy.
-- Pinned by an explicit example in @Test.Reflective@ rather than a property,
-- since there is no independent law that example could report a
-- disagreement with.
mirrorAt :: forall n. KnownNat n => Int -> Int -> (Int, Bool)
mirrorAt i d
    | m == 0 = (0, False)
    | otherwise =
        if r >= m
            then (period - r, True)
            else (r, False)
  where
    m = ordinalSize @n - 1
    period = 2 * m
    dx = d `mod` period
    r = (i + dx) `mod` period
