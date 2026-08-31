{-# LANGUAGE AllowAmbiguousTypes #-}

-- | The shared body of the two reflecting boundary policies,
-- "Data.Grid.Sized.Coord.Reflective" and "Data.Grid.Sized.Coord.Reflect101".
--
-- Both are the same axis: a bounded interval quotiented by reflection, so a
-- displacement that runs off an end folds back and every other fold reverses
-- the axis's sense of direction. They differ in one thing only --- where the
-- mirror sits --- and 'Mirror' is that choice:
--
--   * 'AtWall' reflects across the wall /beyond/ the edge cell, so index
--     @-1@ maps to @0@ and the edge cell is visited twice in a row. This is
--     'Data.Grid.Sized.Coord.Reflective.Reflective'.
--   * 'AtEdge' reflects around the edge cell itself, so index @-1@ maps to
--     @1@ and the edge cell is visited once. This is
--     'Data.Grid.Sized.Coord.Reflect101.Reflect101'.
--
-- Hidden: it exports nothing a consumer needs and its whole reason to exist
-- is that the two public newtypes wrap it. Everything a caller wants is on
-- @Reflective@ / @Reflect101@ and re-exported from
-- "Data.Grid.Sized.Coord".
module Data.Grid.Sized.Coord.Internal.Reflect
  ( Mirror (..),
    reflectAt,
    reflectToEnum,
    reflectPlus,
    reflectFlips,
    ordinalDelta,
  )
where

import Control.Lens (review, view)
import Data.Grid.Sized.Coord.Class (IsCoord, asOrdinal)
import Data.Grid.Sized.Ordinal
import GHC.TypeLits

-- | Where a reflecting axis's mirror sits. See the module header.
data Mirror
  = -- | Reflect across the wall beyond the edge cell; the edge cell repeats.
    AtWall
  | -- | Reflect around the edge cell; the edge cell does not repeat.
    AtEdge
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Closed form of a recursive reflection, computed as a triangle wave over
-- one period instead of recursing, and returning the reflected position
-- together with whether the fold count was odd.
--
-- The period is @2 * k@, where @k@ is the distance between the two mirrors:
-- @size@ for 'AtWall' (the mirrors sit half a cell outside each end),
-- @size - 1@ for 'AtEdge' (they sit on the end cells). The displacement is
-- reduced modulo the period first so the addition cannot overflow. A
-- remainder in the mirrored half is exactly when the fold count's parity
-- flips; the two mirror positions differ by one in where that half starts
-- (@e@) and in the fold-back expression, and 'AtEdge' additionally has no
-- distinct cell to mirror around when @size == 1@, which is special-cased
-- rather than divided by.
--
-- For 'AtEdge', @r == 0@ and @r == k@ are the two mirrors' fixed points,
-- genuinely ambiguous in parity; @<@ (strict) resolves both the same way ---
-- /not/ reflected --- because landing on a mirror is not crossing it. The
-- position does not depend on that choice (at @r == k@ the branches agree,
-- since @period - k == k@), so it is a choice about the flag alone, and the
-- flag is what 'reflectFlips' has to get right. Resolving it the other way
-- made this the only axis where a step the bounds check /accepts/ also
-- reports a flip, which breaks the 'Data.Grid.Sized.Coord.Class.IsCoord' law
-- that a successful checked step has not hit a wall, and made a checked
-- walker turn around one cell early where 'AtWall' walks to the wall
-- (sized-grid-c0s9).
reflectAt :: forall n. (KnownNat n) => Mirror -> Int -> Int -> (Int, Bool)
reflectAt mir i d
  | k == 0 = (0, False)
  | r < k + e = (r, False)
  | otherwise = (2 * k - 1 + e - r, True)
  where
    size = ordinalSize @n
    (k, e) = case mir of
      AtWall -> (size, 0)
      AtEdge -> (size - 1, 1)
    period = 2 * k
    dx = d `mod` period
    r = (i + dx) `mod` period

-- | 'Enum.toEnum' for a reflecting axis: reflect a raw 'Int' into range from
-- the origin.
reflectToEnum :: forall c n. (IsCoord c, KnownNat n) => Mirror -> Int -> c n
reflectToEnum mir x =
  review asOrdinal $ unsafeOrdinal $ fst (reflectAt @n mir 0 x)

-- | @('Data.AffineSpace..+^')@ for a reflecting axis: a retraction of the
-- partial interior action, so associativity fails when a displacement
-- reaches a wall.
reflectPlus :: forall c n. (IsCoord c, KnownNat n) => Mirror -> c n -> Int -> c n
reflectPlus mir a d =
  review asOrdinal $
    unsafeOrdinal $
      fst (reflectAt @n mir (ordinalToInt (view asOrdinal a)) d)

-- | 'Data.Grid.Sized.Coord.Class.axisFrameFlipsIsCoord' for a reflecting
-- axis: whether @('Data.AffineSpace..+^')@ by this displacement reverses the
-- axis's sense of direction, which is an odd number of folds.
reflectFlips :: forall c n. (IsCoord c, KnownNat n) => Mirror -> c n -> Int -> Bool
reflectFlips mir a d =
  snd (reflectAt @n mir (ordinalToInt (view asOrdinal a)) d)

-- | The signed 'Int' difference of two axis positions: the body of
-- @('Data.AffineSpace..-.')@ for both reflecting policies. It is deliberately
-- not reflected --- @b '.+^' (a '.-.' b) == a@ would break otherwise --- and
-- it is the unsigned form
-- 'Data.Grid.Sized.Coord.Class.axisDistanceIsCoord' takes @abs@ of.
-- ("Data.Grid.Sized.Coord.Clamped" has the same one-line body but does not
-- reach through this module for it.)
ordinalDelta :: forall c n. (IsCoord c) => c n -> c n -> Int
ordinalDelta a b =
  ordinalToInt (view asOrdinal a) - ordinalToInt (view asOrdinal b)
