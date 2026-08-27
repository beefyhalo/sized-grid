{-# LANGUAGE AllowAmbiguousTypes #-}

-- | What one axis's boundary policy means.
--
-- 'IsCoord' is the class a boundary policy implements -- how a value of one
-- axis steps, where its ends are, how far apart two of its values are -- and
-- 'IsCoordLifted' is that class at kind 'Type' rather than @Nat -> Type@, so
-- an axis list can hold it. Everything else here is per-axis too: the
-- 'Extremum' an edge report names, the 'Even' \/ 'Odd' size predicates a
-- centred window needs, and the index conversions that let a policy speak in
-- its own axis type while the fold speaks in positions.
--
-- Nothing here knows there is an axis /list/. That is
-- "Data.Grid.Sized.Coord.Class.List", which is built on this and which this
-- does not mention; the dependency runs one way and there is no cycle.
-- Both are re-exported from "Data.Grid.Sized.Coord.Class".
module Data.Grid.Sized.Coord.Class.Axis
  ( IsCoord(..)
  , IsCoordLifted(..)
  , Boundaryless
  , Extremum(..)
  , Even
  , Odd
  , OddC
  , maxCoordSize
  , allCoordLike
  , axisSteps
  , axisStepsIx
  , toAxisIndex
  , unsafeFromAxisIndex
  ) where

import           Data.Grid.Sized.Internal.Error (type (?!))
import           Data.Grid.Sized.Ordinal

import           Control.Lens
import           Data.AffineSpace   (AffineSpace, Diff)
import           Data.Group         (Group)
import           Data.Kind          (Type)
import           Data.Type.Bool     (Not)
import           Data.Type.Equality (type (==))
import           GHC.TypeLits

maxCoordSize :: forall n -> KnownNat n => Integer
maxCoordSize n = fromIntegral (ordinalSize @n) - 1

-- | Deliberately not a 'Bool': a caller that has to act on which edge was hit
-- needs to know which end, not just that one was.
data Extremum
    = AtMin
    | AtMax
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | An axis coordinate and its boundary policy. The policies have a useful
-- geometric correspondence: 'Periodic' is the discrete analogue of the
-- boundaryless circle S1; 'Clamped' is the closed interval D1; and
-- 'Reflective' and 'Reflect101' are D1 quotiented by reflection. 'Ordinal'
-- has no affine action because it cannot leave its interval.
--
-- The only required method is 'asOrdinal'; the rest can be derived
-- automatically.
class IsCoord (c :: Nat -> Type) where
  asOrdinal :: Iso' (c n) (Ordinal n)

  -- | The origin; if @c@ is a 'Monoid' this should be 'mempty'.
  zeroPosition :: (1 <= n, KnownNat n) => c n
  default zeroPosition :: Monoid (c n) => c n
  zeroPosition = mempty

  -- | @1 <= n@ is required because a @c 0@ has no inhabitants.
  maxCoord :: (KnownNat n, 1 <= n) => c n
  maxCoord = review asOrdinal maxCoord

  -- | Recover the coord's value as a type-level 'Nat', with the evidence that
  -- it is in range, and hand it to the continuation as a required type
  -- argument.
  reifyCoord ::
         KnownNat n
      => c n
      -> (forall m -> (KnownNat m, m + 1 <= n) => x)
      -> x
  reifyCoord c = reifyCoord (view asOrdinal c)

  -- | Offset by a signed displacement, or 'Nothing' if that leaves the space.
  -- The checked counterpart of @('Data.AffineSpace..+^')@, which is total and
  -- so must clamp on a bounded coord instead of reporting the overflow.
  offsetIsCoord :: (KnownNat n, 1 <= n) => c n -> Int -> Maybe (c n)
  offsetIsCoord = offsetByPosition

  -- | The number of steps between two values on this axis, by the shorter
  -- route if the axis offers more than one.
  axisDistanceIsCoord :: (KnownNat n, 1 <= n) => c n -> c n -> Int
  {-# INLINE axisDistanceIsCoord #-}
  axisDistanceIsCoord a b =
      abs (ordinalToInt (a ^. asOrdinal) - ordinalToInt (b ^. asOrdinal))

  -- | Which end of the axis this value sits at, or 'Nothing' if it is in the
  -- interior.
  axisBoundaryIsCoord :: KnownNat n => c n -> Maybe Extremum
  axisBoundaryIsCoord = axisBoundaryByPosition

  weakenIsCoord :: KnownNat m => c n -> Maybe (c m)
  weakenIsCoord = fmap (review asOrdinal) . weakenOrdinal . view asOrdinal

  strengthenIsCoord :: (KnownNat m, (n <= m)) => c n -> c m
  strengthenIsCoord = review asOrdinal . strengthenOrdinal . view asOrdinal

  -- | Whether the total step @('Data.AffineSpace..+^')@ takes by this
  -- displacement reverses this axis's own sense of direction; used by
  -- reflecting boundaries. Default is 'False' (no reversal) for every other
  -- policy.
  axisFrameFlipsIsCoord :: (KnownNat n, 1 <= n) => c n -> Int -> Bool
  axisFrameFlipsIsCoord _ _ = False

-- | The bounds check that 'offsetIsCoord' takes as its default.
offsetByPosition ::
       forall c n. (IsCoord c, KnownNat n)
    => c n
    -> Int
    -> Maybe (c n)
{-# INLINE offsetByPosition #-}
offsetByPosition c d
    | d > hi - i = Nothing
    | d < negate i = Nothing
    | otherwise = Just $ review asOrdinal $ unsafeOrdinal $ i + d
  where
    i = ordinalToInt (c ^. asOrdinal)
    hi = ordinalSize @n - 1

-- | The bounds check that 'axisBoundaryIsCoord' takes as its default.
axisBoundaryByPosition ::
       forall c n. (IsCoord c, KnownNat n)
    => c n
    -> Maybe Extremum
{-# INLINE axisBoundaryByPosition #-}
axisBoundaryByPosition c
    -- Order matters: a one-cell axis is both ends, so this reports 'AtMin'.
    | i == 0 = Just AtMin
    | i == ordinalSize @n - 1 = Just AtMax
    | otherwise = Nothing
  where
    i = ordinalToInt (c ^. asOrdinal)

-- | 'IsCoord' lifted to a concrete coord type (kind @Type@) rather than @Nat -> Type@.
class ( x ~ (CoordContainer x) (CoordNat x)
      , 1 <= CoordNat x
      , IsCoord (CoordContainer x)
      , KnownNat (CoordNat x)
      ) =>
      IsCoordLifted x
    where
    type CoordContainer x :: Nat -> Type
    type CoordNat x :: Nat

instance (KnownNat n, 1 <= n, IsCoord c) => IsCoordLifted (c n) where
  type CoordContainer (c n) = c
  type CoordNat (c n) = n

-- | An axis with no walls: the translation action is total and associative.
-- At kind @Type@, not @Nat -> Type@, mirroring the 'IsCoord' \/ 'IsCoordLifted'
-- split -- a marker class has no method with @n@ quantified, so it needs no
-- container-level twin.
--
-- Laws, none checkable by GHC, stated here and tested for every instance:
--
--   1. @'axisBoundaryIsCoord' x == 'Nothing'@ -- no value is at an end.
--   2. @'offsetIsCoord' x d == 'Just' (x '.+^' d)@ -- the action is total.
--   3. @(p '.+^' u) '.+^' v == p '.+^' (u + v)@ -- and associative.
--   4. @p '<>' q == p '.+^' (q '.-.' 'zeroPosition')@ -- the group op IS
--      translation, which is what earns 'Group' its place as a superclass
--      rather than leaving this and 'Group' as two overlapping markers: a
--      boundaryless axis is a Z-torsor, and 'zeroPosition' is the origin
--      that turns the torsor into the group.
class (IsCoordLifted x, AffineSpace x, Diff x ~ Int, Group x) => Boundaryless x

type Even (n :: Nat) = Mod n 2 == 0

-- | Not 'Even'; a window with an even side has no middle cell, so there is
-- no coordinate for a focus to sit at.
type Odd (n :: Nat) = Not (Even n)

-- | 'Odd', turned into a readable compile error naming the offending axis size.
type OddC (x :: Type) =
    Odd (CoordNat x) ?!
    ('Text "Dimension '" ':<>: 'ShowType (CoordNat x) ':<>:
     'Text "' must be odd to have a centre coordinate")

-- | The values one axis can reach within @r@ steps of @c@, paired with the
-- number of steps it actually takes to get there.
--
-- Every value appears once: where two offsets reach the same value (as a
-- torus axis does once @2 * r >= n@), the one that took fewer steps wins, so
-- the recorded distance is the true distance and a bounded axis never
-- reports the same edge cell twice. Ordering is by the surviving offset,
-- ascending, so the centre sits in the middle.
--
-- @INLINE@: at a concrete axis this unrolls into unboxed comparisons with
-- no dictionary passing, which was the whole remaining cost of neighbour
-- enumeration -- measured, not assumed.
axisSteps :: forall x. IsCoordLifted x => Int -> x -> [(Int, x)]
{-# INLINE axisSteps #-}
axisSteps r c =
    [(d, unsafeFromAxisIndex v) | (d, v) <- axisStepsIx @x r (toAxisIndex c)]

-- | 'axisSteps' on the axis's index rather than on a value of the axis type:
-- the form the row-major fold needs, since after sized-grid-adr.16 all it has
-- to hand is a position it has divided its way into.
--
-- This is where the real definition lives and 'axisSteps' is the wrapper,
-- rather than the other way round, because the fold is the hot caller: a
-- neighbourhood enumeration reaches this once per axis per cell, and going
-- via 'axisSteps' would build an axis value per reachable offset only for the
-- caller to take its index again.
axisStepsIx :: forall x. IsCoordLifted x => Int -> Int -> [(Int, Int)]
{-# INLINE axisStepsIx #-}
axisStepsIx r i =
    [(abs d, v) | (d, v) <- reachable, not (any (beats (d, v)) reachable)]
  where
    -- Hoisted out of the comprehension: one axis value per call, not one per
    -- offset tried.
    c :: x
    c = unsafeFromAxisIndex i
    -- Already an 'Int', so -- as before -- no 'Eq' is needed on the axis type.
    reachable :: [(Int, Int)]
    reachable =
        [(d, toAxisIndex v) | d <- [-r .. r], Just v <- [offsetIsCoord c d]]
    -- Fewer steps wins; an exact tie in distance goes to the lower offset, so
    -- the choice is total and the result does not depend on list order.
    beats :: (Int, Int) -> (Int, Int) -> Bool
    beats (d, v) (d', v') = v' == v && (abs d', d') < (abs d, d)

-- | An axis value as its index, and an index back as an axis value.
--
-- Both are the identity at run time bar one branch: every axis type in this
-- library is a newtype over 'Ordinal', which is a newtype over 'Int', so the
-- wrapping is coercion the simplifier removes. What survives is
-- 'unsafeOrdinal'\'s range check on the way in, which sized-grid-adr.14
-- settled is not negotiable and sized-grid-adr.8 measured the price of.
--
-- These two are what let sized-grid-adr.16 change 'Coord'\'s representation
-- without touching 'IsCoord' at all: a boundary policy still says what it
-- means in terms of its own axis type, and the fold converts at the edges.
toAxisIndex :: forall x. IsCoordLifted x => x -> Int
toAxisIndex x = ordinalToInt (x ^. asOrdinal)
{-# INLINE toAxisIndex #-}

-- | __Precondition:__ @0 <= i < 'CoordNat' x@, /unchecked/ -- see
-- 'unsafeOrdinalUnchecked' for why this one is not guarded when every
-- construction of an axis value still is. Every caller in this library
-- obtains @i@ by dividing an in-range position, which establishes the bound
-- by arithmetic.
unsafeFromAxisIndex :: forall x. IsCoordLifted x => Int -> x
unsafeFromAxisIndex i = review asOrdinal (unsafeOrdinalUnchecked i)
{-# INLINE unsafeFromAxisIndex #-}

instance IsCoord Ordinal where
    asOrdinal = id
    zeroPosition = minBound
    reifyCoord = reifyOrdinal
    maxCoord = maxBound

-- | Enumerate all possible values of a coord, in order
allCoordLike :: (1 <= n, IsCoord c, KnownNat n) => [c n]
allCoordLike = toListOf (traverse . re asOrdinal) [minBound .. maxBound]
