{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Grid.Sized.Coord.Class
  ( IsCoord(..)
  , IsCoordLifted(..)
  , IsCoordList(..)
  , IsCoordListF
  , MapDiff
  , AllDiffSame
  , Extremum(..)
  , Even
  , Odd
  , OddC
  , maxCoordSize
  , allCoordLike
  , axisSteps
  ) where

import           Data.Grid.Sized.Internal.Error (type (?!))
import           Data.Grid.Sized.Ordinal

import           Control.Lens
import           Data.AffineSpace   (Diff)
import           Data.Kind          (Constraint, Type)
import           Data.Maybe         (isJust)
import           Data.Type.Bool     (Not)
import           Data.Type.Equality (type (==))
import           Generics.SOP       (All, I (..), NP (..))
import           GHC.TypeLits

maxCoordSize :: forall n -> KnownNat n => Integer
maxCoordSize n = fromIntegral (ordinalSize @n) - 1

-- | Deliberately not a 'Bool': a caller that has to act on which edge was hit
-- needs to know which end, not just that one was.
data Extremum
    = AtMin
    | AtMax
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | The only required methods are 'asOrdinal' and the 'CoordSized' type
-- instance; the rest can be derived automatically.
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
    [(abs d, v) | (d, v) <- reachable, not (any (beats (d, v)) reachable)]
  where
    reachable :: [(Int, x)]
    reachable =
        [(d, v) | d <- [-r .. r], Just v <- [offsetIsCoord c d]]
    -- Compared as an 'Int' through 'asOrdinal', so no 'Eq' is needed on the
    -- axis type itself.
    key :: x -> Int
    key v = ordinalToInt (v ^. asOrdinal)
    -- Fewer steps wins; an exact tie in distance goes to the lower offset, so
    -- the choice is total and the result does not depend on list order.
    beats :: (Int, x) -> (Int, x) -> Bool
    beats (d, v) (d', v') = key v' == key v && (abs d', d') < (abs d, d)

-- | Apply 'Diff' to each element of a type level list: the displacement
-- between two coords is itself coord-shaped, a
-- @'Data.Grid.Sized.Coord.Coord' cs@ displaced by a
-- @'Data.Grid.Sized.Coord.Coord' ('MapDiff' cs)@, one 'Diff' per axis.
-- Tuple literals no longer typecheck as displacements; use @(':|')@, or
-- 'Data.Grid.Sized.Coord.coordFromTuple' where a tuple reads better.
type family MapDiff xs where
  MapDiff '[] = '[]
  MapDiff (x ': xs) = Diff x ': MapDiff xs

-- | All Diffs of the members of the list must be equal. At a concrete list
-- this reduces to one @~@ per axis and costs nothing at run time, unlike a
-- class constraint, which would hand back a dictionary.
type family AllDiffSame a xs :: Constraint where
  AllDiffSame _ '[] = ()
  AllDiffSame a (x ': xs) = (Diff x ~ a, AllDiffSame a xs)

-- | The per-axis obligations of 'IsCoordList', as a type family so that they
-- can be a superclass of it: without this, @'IsCoordList' (x ': xs)@ would
-- not hand back @'IsCoordList' xs@, and induction over the axis list would
-- stop typechecking one step in.
type family IsCoordListF (cs :: [Type]) :: Constraint where
  IsCoordListF '[]        = ()
  IsCoordListF (x ': xs)  = (IsCoordLifted x, IsCoordList xs)

-- | A list of axes that a 'Data.Grid.Sized.Coord.Coord' can be built from,
-- with the row-major fold over that list available as a method.
--
-- The fold has to be a method, not a self-recursive function: a
-- self-recursive fold over the axis list is polymorphic recursion GHC
-- cannot unroll, so every axis after the first pays a dictionary lookup at
-- run time (measured: 27 MB vs 34 bytes on @index x90000@). As an instance
-- method the dictionary resolves at compile time per axis, so the fold
-- unrolls into literal arithmetic. @generics-sop@'s @cpara_SList@ does
-- unroll, but only where the axis list is concrete, which means @INLINE@
-- has to reach all the way down through this library's polymorphic call
-- sites -- also measured, and worse (584 bytes vs 320).
--
-- Every fold here is per-axis: 'npOffset' offsets one axis at a time,
-- 'npStepsWithin' takes a cartesian product of per-axis steps. Axis @i@
-- never sees axis @j@ -- 'offsetIsCoord' is @c n -> Int -> Maybe (c n)@ and
-- has nothing to reach a sibling axis with. That is exactly the class of
-- boundary policy this can express: every 'IsCoord' instance in the
-- library is /separable/ (axis @i@'s edge behaviour depends only on axis
-- @i@), so any product of them -- a cylinder, a torus -- costs no new
-- instance or class. A Möbius strip (wrapping axis 0 flips axis 1) is not
-- separable and cannot be expressed by any @IsCoord@ instance, however
-- written; that needs a coordinate layer above 'Data.Grid.Sized.Coord.Coord'
-- entirely.
class (IsCoordListF cs, All IsCoordLifted cs) => IsCoordList cs where
    -- | The product of the axis sizes, and the row-major position of the given
    -- coordinate within them.
    sizeAndPosition :: NP I cs -> (Int, Int)

    -- | Offset each axis by its own displacement, or 'Nothing' if any axis
    -- refuses. The fold behind 'Data.Grid.Sized.Coord.offsetCoord'.
    --
    -- The displacement constraint is the type family 'AllDiffSame' rather
    -- than a class constraint, since a class constraint would be a
    -- dictionary the method takes at run time -- exactly the per-axis cost
    -- this fold exists to remove.
    npOffset ::
           AllDiffSame Int cs
        => NP I cs
        -> NP I (MapDiff cs)
        -> Maybe (NP I cs)

    -- | Every combination of per-axis values reachable within @r@ steps on
    -- each axis, paired with the total number of steps across all axes. The
    -- fold behind 'Data.Grid.Sized.Coord.stepsWithin', and so behind every
    -- neighbourhood in the library. @r@ is an argument rather than
    -- something an instance can fix, since it is the same radius on every
    -- axis.
    npStepsWithin :: Int -> NP I cs -> [(Int, NP I cs)]

    -- | The product of the axis sizes alone, with no coordinate to fold
    -- over. Duplicates the size half of 'sizeAndPosition' rather than being
    -- built from it, since that needs a value to fold over and this has
    -- none.
    coordListSize :: Int

    -- | Where each axis sits relative to its own ends, first axis first. The
    -- fold behind 'Data.Grid.Sized.Coord.axisBoundaries', and so behind
    -- 'Data.Grid.Sized.Coord.onBoundary' and 'Data.Grid.Sized.Coord.isCorner'.
    -- A method rather than a @generics-sop@ @hcmap@ fold for the same
    -- unrolling reason as 'sizeAndPosition' above.
    npBoundaries :: NP I cs -> [Maybe Extremum]

    -- | The per-axis distances between two coords, first axis first. The
    -- fold behind 'Data.Grid.Sized.Coord.axisDistances', and so behind
    -- 'Data.Grid.Sized.Coord.coordDistance' and
    -- 'Data.Grid.Sized.Coord.coordManhattan'.
    npDistances :: NP I cs -> NP I cs -> [Int]

    -- | Whether any axis is at one of its ends, and whether every axis is.
    -- Fused counterparts of @'any' 'isJust' . 'npBoundaries'@ and @'all'
    -- 'isJust' . 'npBoundaries'@, which built a @['Maybe' 'Extremum']@ per
    -- call to answer a 'Bool': the 360,000-call
    -- 'Data.Grid.Sized.Coord.onBoundary' sweep allocated 123 MB, and 60 MB
    -- once fused. Methods rather than folds over 'npBoundaries' for the
    -- unrolling reason above.
    --
    -- 'npAllBoundary' is the fold's identity on the empty axis list, so it
    -- is vacuously 'True' there; 'Data.Grid.Sized.Coord.isCorner' rejects
    -- that case itself.
    npAnyBoundary :: NP I cs -> Bool
    npAllBoundary :: NP I cs -> Bool

    -- | The largest and the summed per-axis distance: the Chebyshev and
    -- Manhattan metrics, without the @['Int']@ 'npDistances' would build to
    -- feed them.
    npMaxDistance :: NP I cs -> NP I cs -> Int
    npSumDistance :: NP I cs -> NP I cs -> Int

instance IsCoordList '[] where
    sizeAndPosition Nil = (1, 0)
    npOffset Nil Nil = Just Nil
    -- One way to take no steps at all, at a distance of zero. This is what
    -- makes the centre the only entry whose total is zero, which is how both
    -- neighbourhood functions exclude it without comparing coordinates.
    npStepsWithin _ Nil = [(0, Nil)]
    coordListSize = 1
    npBoundaries Nil = []
    npDistances Nil Nil = []
    npAnyBoundary Nil = False
    npAllBoundary Nil = True
    npMaxDistance Nil Nil = 0
    npSumDistance Nil Nil = 0
    {-# INLINE sizeAndPosition #-}
    {-# INLINE npOffset #-}
    {-# INLINE npStepsWithin #-}
    {-# INLINE coordListSize #-}
    {-# INLINE npBoundaries #-}
    {-# INLINE npDistances #-}
    {-# INLINE npAnyBoundary #-}
    {-# INLINE npAllBoundary #-}
    {-# INLINE npMaxDistance #-}
    {-# INLINE npSumDistance #-}

instance (IsCoordLifted x, IsCoordList xs) => IsCoordList (x ': xs) where
    sizeAndPosition (I c :* cs) =
        case sizeAndPosition cs of
            (stride, rest) ->
                ( ordinalSize @(CoordNat x) * stride
                , ordinalToInt (c ^. asOrdinal) * stride + rest)
    -- The coord drives the match, as it did in the @where@ helper this
    -- replaced: matching ':*' on the first argument is what refines @xs@ far
    -- enough for 'MapDiff' to reduce and the second to match.
    npOffset (I x :* xs) (I dx :* dxs) =
        (\y ys -> I y :* ys) <$> offsetIsCoord x dx <*> npOffset xs dxs
    -- The first axis outermost, so results come out in the row-major order
    -- 'sizeAndPosition' lays a grid out in.
    npStepsWithin r (I x :* xs) =
        [ (d + s, I v :* vs)
        | (d, v) <- axisSteps r x
        , (s, vs) <- npStepsWithin r xs
        ]
    coordListSize = ordinalSize @(CoordNat x) * coordListSize @xs
    -- 'x' unifies with @CoordContainer x (CoordNat x)@ via 'IsCoordLifted's
    -- superclass equality, so 'axisBoundaryIsCoord' and 'axisDistanceIsCoord'
    -- apply to it directly, at the per-axis 'IsCoord' instance 'IsCoordLifted
    -- x' resolves to --- no 'Data.Grid.Sized.Coord.axisBoundary'\/'Data.Grid.Sized.Coord.axisDistance'
    -- indirection needed here, the same way 'npOffset' calls 'offsetIsCoord'
    -- directly rather than through a lifted wrapper.
    npBoundaries (I x :* xs) = axisBoundaryIsCoord x : npBoundaries xs
    npDistances (I x :* xs) (I y :* ys) = axisDistanceIsCoord x y : npDistances xs ys
    npAnyBoundary (I x :* xs) = isJust (axisBoundaryIsCoord x) || npAnyBoundary xs
    npAllBoundary (I x :* xs) = isJust (axisBoundaryIsCoord x) && npAllBoundary xs
    npMaxDistance (I x :* xs) (I y :* ys) =
        max (axisDistanceIsCoord x y) (npMaxDistance xs ys)
    npSumDistance (I x :* xs) (I y :* ys) =
        axisDistanceIsCoord x y + npSumDistance xs ys
    {-# INLINE sizeAndPosition #-}
    {-# INLINE npOffset #-}
    {-# INLINE npStepsWithin #-}
    {-# INLINE coordListSize #-}
    {-# INLINE npBoundaries #-}
    {-# INLINE npDistances #-}
    {-# INLINE npAnyBoundary #-}
    {-# INLINE npAllBoundary #-}
    {-# INLINE npMaxDistance #-}
    {-# INLINE npSumDistance #-}

instance IsCoord Ordinal where
    asOrdinal = id
    zeroPosition = minBound
    reifyCoord = reifyOrdinal
    maxCoord = maxBound

-- | Enumerate all possible values of a coord, in order
allCoordLike :: (1 <= n, IsCoord c, KnownNat n) => [c n]
allCoordLike = toListOf (traverse . re asOrdinal) [minBound .. maxBound]
