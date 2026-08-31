{-# LANGUAGE AllowAmbiguousTypes #-}

-- | The row-major fold over a list of axes.
--
-- 'IsCoordList' is one class whose fourteen methods are the whole of
-- 'Data.Grid.Sized.Coord.Coord'\'s arithmetic: build a position from a list of
-- axis values, take one apart again, read off its per-axis indices, offset it,
-- enumerate what is within a radius, report which edges it touches, measure
-- two of them. They are class methods rather than a self-recursive function
-- for a measured reason, stated on the class.
--
-- Everything per-axis that these methods call -- 'IsCoord', 'IsCoordLifted',
-- 'toAxisIndex', 'unsafeFromAxisIndex', 'axisStepsIx', 'Extremum' -- lives in
-- "Data.Grid.Sized.Coord.Class.Axis", which knows nothing of this module.
-- Both are re-exported from "Data.Grid.Sized.Coord.Class".
module Data.Grid.Sized.Coord.Class.List
  ( IsCoordList (..),
    IsCoordListF,
    MapDiff,
    MapStep,
    AllDiffSame,
  )
where

import Data.AffineSpace (Diff)
import Data.Grid.Sized.Coord.Class.Axis
import Data.Grid.Sized.Ordinal
import Data.Kind (Constraint, Type)
import Data.Maybe (isJust)
import Generics.SOP (All, I (..), NP (..))

-- | Apply 'Diff' to each element of a type level list: the displacement
-- between two coords is itself coord-shaped, a
-- @'Data.Grid.Sized.Coord.Coord' cs@ displaced by a
-- @'Data.Grid.Sized.Coord.Coord' ('MapDiff' cs)@, one 'Diff' per axis.
-- Tuple literals no longer typecheck as displacements; use @(':|')@, or
-- 'Data.Grid.Sized.Coord.coordFromTuple' where a tuple reads better.
type family MapDiff xs where
  MapDiff '[] = '[]
  MapDiff (x ': xs) = Diff x ': MapDiff xs

-- | One signed step count per axis: the displacement a /checked/ move takes,
-- as against the 'MapDiff' an affine one does.
--
-- The two are the same list wherever both exist -- every 'Diff' in this
-- library is 'Int' -- but they are stuck on different things.
-- @'MapDiff' cs@ reduces only where every axis has an 'Data.AffineSpace.AffineSpace'
-- instance, and 'Data.Grid.Sized.Ordinal.Ordinal' deliberately has none: it
-- cannot leave its interval, so it has no affine action to offer. A bounds
-- check is not an affine action, though. 'Data.Grid.Sized.Coord.Class.offsetIsCoord'
-- already says so -- its displacement is a plain 'Int' -- and this family is
-- that same statement one level up, so that
-- 'Data.Grid.Sized.Coord.offsetCoord' and everything built on it works on the
-- axis whose whole purpose is to have no boundary policy.
--
-- See sized-grid-i0ob.2. Affine movement keeps 'MapDiff': being total is what
-- the axis type licenses, and excluding @Ordinal@ from it is the design
-- working.
type family MapStep xs where
  MapStep '[] = '[]
  MapStep (x ': xs) = Int ': MapStep xs

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
  IsCoordListF '[] = ()
  IsCoordListF (x ': xs) = (IsCoordLifted x, IsCoordList xs)

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

  -- | How many axes are in the list, /d/ in @(2r+1)^d@: the dimension a
  -- caller who knows a radius needs to turn it into an upper bound on a
  -- Moore or von Neumann neighbourhood's width, which is what
  -- 'Data.Grid.Sized.Stencil.mooreStencil' and
  -- 'Data.Grid.Sized.Stencil.vonNeumannStencil' use it for. A method for
  -- the same unrolling reason as 'coordListSize': at a concrete axis list
  -- this counts down to a literal at compile time rather than folding a
  -- list at run time.
  coordListLength :: Int

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

  -- | Each axis's own index, first axis first: the same decode as
  -- 'npFromPosition' stopping one step short, at the 'Int' each axis's
  -- 'quotRem' hands over rather than at the axis value
  -- 'unsafeFromAxisIndex' would rebuild from it. The fold behind
  -- 'Data.Grid.Sized.Coord.coordIndices'.
  --
  -- A method of its own rather than @'map' 'toAxisIndex' . 'npFromPosition'@
  -- for the reason the rest of them are: that route needs an
  -- @'All' 'IsCoordLifted' cs@ fold to reach 'toAxisIndex' per axis, and
  -- builds a value of every axis type on the way to taking its index again.
  posIndices :: Int -> [Int]

  -- | The inverse of 'posIndices': one plain 'Int' per axis, first axis
  -- first, folded back into a row-major position. 'Nothing' if the list is
  -- not exactly one entry per axis, or if any entry is outside @[0, size)@
  -- for its own axis. The fold behind
  -- 'Data.Grid.Sized.Coord.coordFromIndices'.
  --
  -- The bounds check is against the axis /size/, not its boundary policy: a
  -- caller that wants clamping or wrapping composes that itself. On
  -- in-range input @'posIndices' . 'posFromIndices'@ round-trips.
  posFromIndices :: [Int] -> Maybe Int

  -- | Offset each axis by its own step count, or 'Nothing' if any axis
  -- refuses. The fold behind 'Data.Grid.Sized.Coord.offsetCoord'.
  --
  -- Indexed by 'MapStep' and not 'MapDiff', so it carries no obligation at
  -- all beyond 'IsCoordList': the per-axis operation it folds,
  -- 'offsetIsCoord', is a bounds check taking an 'Int', and a bounds check
  -- is something every axis can do -- 'Data.Grid.Sized.Ordinal.Ordinal'
  -- included, which @MapDiff@ shut out.
  posOffset ::
    Int ->
    NP I (MapStep cs) ->
    Maybe Int

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
  coordListLength = 0
  npToPosition Nil = 0

  -- The remainder at the end of a well-formed decode is always zero, so
  -- there is nothing left to represent and nothing to check here.
  -- 'Data.Grid.Sized.Coord.coordFromPosition' does the checking on the way
  -- in, where a bad position can still be rejected.
  npFromPosition _ = Nil
  posIndices _ = []

  -- A well-formed call has run out of axes exactly when it runs out of
  -- indices; anything left over is a length mismatch.
  posFromIndices [] = Just 0
  posFromIndices _ = Nothing
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
  {-# INLINE coordListLength #-}
  {-# INLINE npToPosition #-}
  {-# INLINE npFromPosition #-}
  {-# INLINE posIndices #-}
  {-# INLINE posFromIndices #-}
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

  coordListLength = 1 + coordListLength @xs

  npToPosition (I c :* cs) =
    toAxisIndex c * coordListSize @xs + npToPosition cs

  npFromPosition p =
    case p `quotRem` coordListSize @xs of
      (i, r) -> I (unsafeFromAxisIndex @x i) :* npFromPosition r

  posIndices p =
    case p `quotRem` coordListSize @xs of
      (i, r) -> i : posIndices @xs r

  posFromIndices [] = Nothing
  posFromIndices (i : is)
    | i < 0 || i >= ordinalSize @(CoordNat x) = Nothing
    | otherwise = (\r -> i * coordListSize @xs + r) <$> posFromIndices @xs is

  -- The displacement drives the match: ':*' on it is what refines @xs@ far
  -- enough for 'MapStep' to reduce, the job the coord's own ':*' used to do
  -- when this took one.
  posOffset p (I dx :* dxs) =
    case p `quotRem` stride of
      (i, r) ->
        (\y r' -> toAxisIndex y * stride + r')
          <$> offsetIsCoord (unsafeFromAxisIndex @x i) dx
          <*> posOffset @xs r dxs
    where
      stride = coordListSize @xs

  -- The first axis outermost, so results come out in the row-major order
  -- 'npToPosition' lays a grid out in.
  posStepsWithin r p =
    case p `quotRem` stride of
      (i, rest) ->
        [ (d + s, v * stride + vs)
        | (d, v) <- axisStepsIx @x r i,
          (s, vs) <- posStepsWithin @xs r rest
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
        axisDistanceIsCoord (unsafeFromAxisIndex @x i) (unsafeFromAxisIndex @x j)
          : posDistances @xs r s
    where
      stride = coordListSize @xs

  posAnyBoundary p =
    case p `quotRem` coordListSize @xs of
      (i, r) ->
        isJust (axisBoundaryIsCoord (unsafeFromAxisIndex @x i))
          || posAnyBoundary @xs r

  posAllBoundary p =
    case p `quotRem` coordListSize @xs of
      (i, r) ->
        isJust (axisBoundaryIsCoord (unsafeFromAxisIndex @x i))
          && posAllBoundary @xs r

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
        axisDistanceIsCoord (unsafeFromAxisIndex @x i) (unsafeFromAxisIndex @x j)
          + posSumDistance @xs r s
    where
      stride = coordListSize @xs

  {-# INLINE coordListSize #-}
  {-# INLINE coordListLength #-}
  {-# INLINE npToPosition #-}
  {-# INLINE npFromPosition #-}
  {-# INLINE posIndices #-}
  {-# INLINE posFromIndices #-}
  {-# INLINE posOffset #-}
  {-# INLINE posStepsWithin #-}
  {-# INLINE posBoundaries #-}
  {-# INLINE posDistances #-}
  {-# INLINE posAnyBoundary #-}
  {-# INLINE posAllBoundary #-}
  {-# INLINE posMaxDistance #-}
  {-# INLINE posSumDistance #-}
