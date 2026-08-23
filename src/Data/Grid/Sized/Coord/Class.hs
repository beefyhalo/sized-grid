{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Grid.Sized.Coord.Class
  ( IsCoord(..)
  , IsCoordLifted(..)
  , IsCoordList(..)
  , IsCoordListF
  , Boundaryless
  , MapDiff
  , AllDiffSame
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
-- == Positions, not spines
--
-- sized-grid-adr.16 turned a @Coord@ into its row-major position, so every
-- method below that used to take an @NP I cs@ now takes that @Int@ and
-- divides its way in: at each axis, @'quotRem' ('coordListSize' \@xs)@
-- splits the position into this axis's index and the rest, and the results
-- are multiplied back out by the same stride. At a concrete axis list the
-- strides are literals, so GHC turns the divisions into multiply-shift and
-- the whole fold into flat arithmetic on one unboxed 'Int'.
--
-- What did /not/ change is 'IsCoord'. A boundary policy still says what it
-- means in terms of its own axis type, and 'toAxisIndex' \/ 'unsafeFromAxisIndex'
-- convert at the edges of each step -- both coercions bar 'unsafeOrdinal'\'s
-- range check. That is what kept the port from touching
-- 'Data.Grid.Sized.Coord.Clamped.Clamped',
-- 'Data.Grid.Sized.Coord.Periodic.Periodic',
-- 'Data.Grid.Sized.Coord.Reflective.Reflective' or
-- 'Data.Grid.Sized.Coord.Reflect101.Reflect101' at all.
--
-- == Separability
--
-- Every fold here is per-axis: 'posOffset' offsets one axis at a time,
-- 'posStepsWithin' takes a cartesian product of per-axis steps. Axis @i@
-- never sees axis @j@ -- 'offsetIsCoord' is @c n -> Int -> Maybe (c n)@ and
-- has nothing to reach a sibling axis with. That is exactly the class of
-- boundary policy this can express: every 'IsCoord' instance in the
-- library is /separable/ (axis @i@'s edge behaviour depends only on axis
-- @i@), so any product of them -- a cylinder, a torus -- costs no new
-- instance or class. A Möbius strip (wrapping axis 0 flips axis 1) is not
-- separable and cannot be expressed by any @IsCoord@ instance, however
-- written; that needs a coordinate layer above 'Data.Grid.Sized.Coord.Coord'
-- entirely.
--
-- The position representation does not weaken that: a stride split is still
-- one axis at a time, and nothing in a fold step can see a sibling's index
-- except through the remainder it passes on.
class (IsCoordListF cs, All IsCoordLifted cs) => IsCoordList cs where
    -- | The product of the axis sizes: the size of the coordinate space, and
    -- so the stride of the axis immediately to the left of this list.
    --
    -- This carries the whole shape after adr.16 -- it is what every other
    -- method divides and multiplies by -- where it used to be the lesser
    -- half of a @sizeAndPosition@ that also folded a coordinate.
    coordListSize :: Int

    -- | The row-major position of a coordinate given axis by axis. The
    -- surviving half of the old @sizeAndPosition@, and now a /constructor/
    -- rather than an accessor: 'Data.Grid.Sized.Coord.coordPosition' is
    -- 'Data.Coerce.coerce'.
    npToPosition :: NP I cs -> Int

    -- | The inverse: a position back into one value per axis. The head is
    -- @p \`div\` stride@ and the tail decodes the remainder, so the last
    -- axis is the least significant.
    --
    -- This is what 'Data.Grid.Sized.Coord.unCoord' and the
    -- @('Data.Grid.Sized.Coord.:|')@ pattern are built from, and what the
    -- 'Show', JSON and 'System.Random.Random' instances rebuild an @NP@
    -- with. adr.8 measured the cost of paying it per cell in the worst case
    -- it could construct -- a @tabulate@ whose rule destructures its
    -- coordinate -- and it came out at the same 2.24x as the
    -- non-destructuring one: producing a coordinate costs more than decoding
    -- one.
    npFromPosition :: Int -> NP I cs

    -- | Offset each axis by its own displacement, or 'Nothing' if any axis
    -- refuses. The fold behind 'Data.Grid.Sized.Coord.offsetCoord'.
    --
    -- The displacement constraint is the type family 'AllDiffSame' rather
    -- than a class constraint, since a class constraint would be a
    -- dictionary the method takes at run time -- exactly the per-axis cost
    -- this fold exists to remove.
    posOffset ::
           AllDiffSame Int cs
        => Int
        -> NP I (MapDiff cs)
        -> Maybe Int

    -- | Every combination of per-axis values reachable within @r@ steps on
    -- each axis, paired with the total number of steps across all axes. The
    -- fold behind 'Data.Grid.Sized.Coord.stepsWithin', and so behind every
    -- neighbourhood in the library. @r@ is an argument rather than
    -- something an instance can fix, since it is the same radius on every
    -- axis.
    posStepsWithin :: Int -> Int -> [(Int, Int)]

    -- | Where each axis sits relative to its own ends, first axis first. The
    -- fold behind 'Data.Grid.Sized.Coord.axisBoundaries', and so behind
    -- 'Data.Grid.Sized.Coord.onBoundary' and 'Data.Grid.Sized.Coord.isCorner'.
    -- A method rather than a @generics-sop@ @hcmap@ fold for the same
    -- unrolling reason as 'npToPosition' above.
    posBoundaries :: Int -> [Maybe Extremum]

    -- | The per-axis distances between two coords, first axis first. The
    -- fold behind 'Data.Grid.Sized.Coord.axisDistances', and so behind
    -- 'Data.Grid.Sized.Coord.coordDistance' and
    -- 'Data.Grid.Sized.Coord.coordManhattan'.
    posDistances :: Int -> Int -> [Int]

    -- | Whether any axis is at one of its ends, and whether every axis is.
    -- Fused counterparts of @'any' 'isJust' . 'posBoundaries'@ and @'all'
    -- 'isJust' . 'posBoundaries'@, which built a @['Maybe' 'Extremum']@ per
    -- call to answer a 'Bool': the 360,000-call
    -- 'Data.Grid.Sized.Coord.onBoundary' sweep allocated 123 MB, and 60 MB
    -- once fused. Methods rather than folds over 'posBoundaries' for the
    -- unrolling reason above.
    --
    -- 'posAllBoundary' is the fold's identity on the empty axis list, so it
    -- is vacuously 'True' there; 'Data.Grid.Sized.Coord.isCorner' rejects
    -- that case itself.
    posAnyBoundary :: Int -> Bool
    posAllBoundary :: Int -> Bool

    -- | The largest and the summed per-axis distance: the Chebyshev and
    -- Manhattan metrics, without the @['Int']@ 'posDistances' would build to
    -- feed them.
    posMaxDistance :: Int -> Int -> Int
    posSumDistance :: Int -> Int -> Int

instance IsCoordList '[] where
    coordListSize = 1
    npToPosition Nil = 0
    -- The remainder at the end of a well-formed decode is always zero, so
    -- there is nothing left to represent and nothing to check here.
    -- 'Data.Grid.Sized.Coord.coordFromPosition' does the checking on the way
    -- in, where a bad position can still be rejected.
    npFromPosition _ = Nil
    posOffset p Nil = Just p
    -- One way to take no steps at all, at a distance of zero. This is what
    -- makes the centre the only entry whose total is zero, which is how both
    -- neighbourhood functions exclude it without comparing coordinates.
    posStepsWithin _ _ = [(0, 0)]
    posBoundaries _ = []
    posDistances _ _ = []
    posAnyBoundary _ = False
    posAllBoundary _ = True
    posMaxDistance _ _ = 0
    posSumDistance _ _ = 0
    {-# INLINE coordListSize #-}
    {-# INLINE npToPosition #-}
    {-# INLINE npFromPosition #-}
    {-# INLINE posOffset #-}
    {-# INLINE posStepsWithin #-}
    {-# INLINE posBoundaries #-}
    {-# INLINE posDistances #-}
    {-# INLINE posAnyBoundary #-}
    {-# INLINE posAllBoundary #-}
    {-# INLINE posMaxDistance #-}
    {-# INLINE posSumDistance #-}

-- | Each method splits the position by the stride of the axes to its right,
-- handles this axis through its own 'IsCoord' instance, and multiplies back
-- out. @stride@ is a literal wherever the axis list is concrete, which is
-- what lets GHC turn the 'quotRem' into multiply-shift.
instance (IsCoordLifted x, IsCoordList xs) => IsCoordList (x ': xs) where
    coordListSize = ordinalSize @(CoordNat x) * coordListSize @xs

    npToPosition (I c :* cs) =
        toAxisIndex c * coordListSize @xs + npToPosition cs

    npFromPosition p =
        case p `quotRem` coordListSize @xs of
            (i, r) -> I (unsafeFromAxisIndex @x i) :* npFromPosition r

    -- The displacement drives the match: ':*' on it is what refines @xs@ far
    -- enough for 'MapDiff' to reduce, the job the coord's own ':*' used to do
    -- when this took one.
    posOffset p (I dx :* dxs) =
        case p `quotRem` stride of
            (i, r) ->
                (\y r' -> toAxisIndex y * stride + r') <$>
                offsetIsCoord (unsafeFromAxisIndex @x i) dx <*>
                posOffset @xs r dxs
      where
        stride = coordListSize @xs

    -- The first axis outermost, so results come out in the row-major order
    -- 'npToPosition' lays a grid out in.
    posStepsWithin r p =
        case p `quotRem` stride of
            (i, rest) ->
                [ (d + s, v * stride + vs)
                | (d, v) <- axisStepsIx @x r i
                , (s, vs) <- posStepsWithin @xs r rest
                ]
      where
        stride = coordListSize @xs

    -- 'x' unifies with @CoordContainer x (CoordNat x)@ via 'IsCoordLifted's
    -- superclass equality, so 'axisBoundaryIsCoord' and 'axisDistanceIsCoord'
    -- apply to an 'unsafeFromAxisIndex' of this axis's index directly, at the
    -- per-axis 'IsCoord' instance @IsCoordLifted x@ resolves to --- no
    -- 'Data.Grid.Sized.Coord.axisBoundary'\/'Data.Grid.Sized.Coord.axisDistance'
    -- indirection needed here, the same way 'posOffset' calls 'offsetIsCoord'
    -- directly rather than through a lifted wrapper.
    posBoundaries p =
        case p `quotRem` coordListSize @xs of
            (i, r) ->
                axisBoundaryIsCoord (unsafeFromAxisIndex @x i) : posBoundaries @xs r

    posDistances p q =
        case (p `quotRem` stride, q `quotRem` stride) of
            ((i, r), (j, s)) ->
                axisDistanceIsCoord (unsafeFromAxisIndex @x i) (unsafeFromAxisIndex @x j) :
                posDistances @xs r s
      where
        stride = coordListSize @xs

    posAnyBoundary p =
        case p `quotRem` coordListSize @xs of
            (i, r) ->
                isJust (axisBoundaryIsCoord (unsafeFromAxisIndex @x i)) ||
                posAnyBoundary @xs r

    posAllBoundary p =
        case p `quotRem` coordListSize @xs of
            (i, r) ->
                isJust (axisBoundaryIsCoord (unsafeFromAxisIndex @x i)) &&
                posAllBoundary @xs r

    posMaxDistance p q =
        case (p `quotRem` stride, q `quotRem` stride) of
            ((i, r), (j, s)) ->
                max
                    (axisDistanceIsCoord (unsafeFromAxisIndex @x i) (unsafeFromAxisIndex @x j))
                    (posMaxDistance @xs r s)
      where
        stride = coordListSize @xs

    posSumDistance p q =
        case (p `quotRem` stride, q `quotRem` stride) of
            ((i, r), (j, s)) ->
                axisDistanceIsCoord (unsafeFromAxisIndex @x i) (unsafeFromAxisIndex @x j) +
                posSumDistance @xs r s
      where
        stride = coordListSize @xs

    {-# INLINE coordListSize #-}
    {-# INLINE npToPosition #-}
    {-# INLINE npFromPosition #-}
    {-# INLINE posOffset #-}
    {-# INLINE posStepsWithin #-}
    {-# INLINE posBoundaries #-}
    {-# INLINE posDistances #-}
    {-# INLINE posAnyBoundary #-}
    {-# INLINE posAllBoundary #-}
    {-# INLINE posMaxDistance #-}
    {-# INLINE posSumDistance #-}

instance IsCoord Ordinal where
    asOrdinal = id
    zeroPosition = minBound
    reifyCoord = reifyOrdinal
    maxCoord = maxBound

-- | Enumerate all possible values of a coord, in order
allCoordLike :: (1 <= n, IsCoord c, KnownNat n) => [c n]
allCoordLike = toListOf (traverse . re asOrdinal) [minBound .. maxBound]
