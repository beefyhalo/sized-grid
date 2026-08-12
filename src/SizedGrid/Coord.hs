{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Unlike `SizedGrid.Grid.Grid.Grid`, `Coord`\'s constructor is safe to export:
-- an @NP I cs@ is correct by construction for its own index, so there is no
-- length invariant a caller could break.
module SizedGrid.Coord
  ( -- * Coordinates
    Coord(..)
  , pattern (:|)
  , pattern EmptyCoord
  , coordSplit
  , _WrappedCoord
    -- * Building and taking apart
  , singleCoord
  , appendCoord
  , coordFromTuple
  , coordToTuple
  , coordHead
  , coordTail
  , tranposeCoord
  , zeroCoord
  , allCoord
  , coordPosition
  , coordFromPosition
  , coordSpaceSize
    -- * Neighbourhoods
  , offsetCoord
  , neighbours
  , mooreNeighbours
  , vonNeumannNeighbours
  , axisSteps
  , stepsWithin
    -- * Distance
  , axisDistance
  , axisDistances
  , coordDistance
  , coordManhattan
    -- * Boundaries
  , axisBoundary
  , axisBoundaries
  , onBoundary
  , isCorner
  , interiorCoords
    -- * Changing the size of a coord
  , WeakenCoord(..)
  , StrengthenCoord(..)
    -- * Type-level machinery
  , Length
  , MaxCoordSize
  , MapDiff
  , AllDiffSame
  , AllSizedKnown(..)
  ) where

import           SizedGrid.Coord.Class
import           SizedGrid.Ordinal

import           Control.Applicative   (empty)
import           Control.Lens          hiding (from, to)
import           Control.Monad.State
import           Data.AdditiveGroup
import           Data.Aeson
import           Data.AffineSpace
import           Data.Constraint
import           Data.Constraint.Nat
import           Data.Kind (Type)
import           Data.List             (intercalate)
import           Data.Maybe            (isJust)
import qualified Data.Vector           as V
import           Generics.SOP          hiding (Generic, S, Z)
import qualified Generics.SOP          as SOP
import           GHC.Generics          (Generic)
import           GHC.TypeLits
import qualified GHC.TypeLits          as GHC
import           System.Random         (Random (..))

-- | Length of a type level list
type family Length cs where
  Length '[] = 0
  Length (c ': cs) = (GHC.+) 1 (Length cs)

-- | A multideminsion coordinate
newtype Coord cs = Coord {unCoord :: NP I cs}
  deriving (Generic)

coordSplit:: Coord (c ': cs) -> (c, Coord cs)
coordSplit (Coord (I x :* xs)) = (x, Coord xs)

pattern (:|) :: c -> Coord cs -> Coord (c ': cs)
pattern (:|) a as <- (coordSplit -> (a,as))
  where (:|) a (Coord as) = Coord (I a :* as)

pattern EmptyCoord :: Coord '[]
pattern EmptyCoord = Coord Nil

-- | Both patterns are total for their own index: a @Coord (c ': cs)@ is always
-- a ':|' and a @Coord '[]@ is always an 'EmptyCoord'. GHC cannot work that out
-- for a view pattern on its own, so without these the coverage checker demands
-- a fallback equation, and the only thing such an equation can do is 'error' on
-- a value that cannot exist.
{-# COMPLETE (:|) #-}

{-# COMPLETE EmptyCoord #-}

infixr 5 :|

_WrappedCoord :: Iso' (Coord cs) (NP I cs)
_WrappedCoord = dimap unCoord (fmap Coord)

instance All Eq cs => Eq (Coord cs) where
    Coord a == Coord b =
        and $
        hcollapse $ hcliftA2 (Proxy :: Proxy Eq) (\(I x) (I y) -> K (x == y)) a b

-- | @All Eq cs@ looks like it should follow from @All Ord cs@, but it does not
-- and cannot: @All c cs@ expands to the type family @AllF c cs@, and reducing
-- @AllF Eq cs@ from @AllF Ord cs@ needs induction over @cs@, which the
-- constraint solver will not do for a type variable. A @Dict@-based entailment
-- (@Data.SOP.Dict.mapAll@) can produce the evidence at the value level, but
-- superclass evidence for an instance has to be discharged at declaration
-- time, where no such value is in scope. So both constraints stay.
instance (All Eq cs, All Ord cs) => Ord (Coord cs) where
    compare (Coord a) (Coord b) =
        mconcat $
        hcollapse $
        hcliftA2 (Proxy :: Proxy Ord) (\(I x) (I y) -> K (compare x y)) a b

instance All Show cs => Show (Coord cs) where
    show (Coord a) =
        "Coord [" ++
        intercalate
            ", "
            (hcollapse $ hcliftA (Proxy :: Proxy Show) (\(I x) -> K $ show x) a) ++
        "]"

instance (All ToJSON cs) => ToJSON (Coord cs) where
    toJSON (Coord a) =
        Array $
        V.fromList $
        hcollapse $ hcmap (Proxy @ToJSON) (\(I x) -> K $ toJSON x) a

instance All FromJSON cs => FromJSON (Coord cs) where
    parseJSON =
        withArray "Coord" $ \v ->
            case SOP.fromList $ V.toList v of
                Just a ->
                    Coord <$>
                    hsequence
                        (hcmap (Proxy @FromJSON) (\(K x) -> parseJSON x) a)
                Nothing -> empty

instance All Semigroup cs => Semigroup (Coord cs) where
  Coord a <> Coord b = Coord $ hcliftA2 (Proxy :: Proxy Semigroup) (liftA2 (<>)) a b

instance (All Semigroup cs, All Monoid cs) => Monoid (Coord cs) where
  mappend = (<>)
  mempty = Coord $ hcpure (Proxy :: Proxy Monoid) (pure mempty)

instance (All AdditiveGroup cs) => AdditiveGroup (Coord cs) where
    zeroV = Coord $ hcpure (Proxy :: Proxy AdditiveGroup) (pure zeroV)
    Coord a ^+^ Coord b =
        Coord $ hcliftA2 (Proxy :: Proxy AdditiveGroup) (liftA2 (^+^)) a b
    negateV (Coord a) =
        Coord $ hcliftA (Proxy :: Proxy AdditiveGroup) (fmap negateV) a
    Coord a ^-^ Coord b =
        Coord $ hcliftA2 (Proxy :: Proxy AdditiveGroup) (liftA2 (^-^)) a b

instance (All Random cs) => Random (Coord cs) where
    random g =
        let (c, g') =
                runState
                    (hsequence $ hcpure (Proxy :: Proxy Random) (state random))
                    g
        in (Coord c, g')
    randomR (Coord mi, Coord ma) g =
        let (c, g') =
                runState
                    (hsequence $
                     hcliftA2
                         (Proxy :: Proxy Random)
                         (\(I a) (I b) -> state (randomR (a, b)))
                         mi
                         ma)
                    g
        in (Coord c, g')

-- | Get the first element of a coord. Thanks to type level information, we can write this as a total `Lens`
coordHead :: Lens (Coord (a ': as)) (Coord (a' ': as)) a a'
coordHead f (Coord (I a :* as)) = (\a' -> Coord (I a' :* as)) <$> f a

-- | A `Lens` into the the tail of `Coord`
coordTail :: Lens (Coord (a ': as)) (Coord (a ': as')) (Coord as) (Coord as')
coordTail f (Coord (a :* as)) = (\(Coord as') -> Coord (a :* as')) <$> f (Coord as)

-- | Turn a single element into a one dimensional `Coord`
singleCoord :: a -> Coord '[a]
singleCoord a = Coord (I a :* Nil)

-- | Add a new element to a `Coord`. This increases the dimensionality
appendCoord :: a -> Coord as -> Coord (a ': as)
appendCoord a (Coord as) = Coord (I a :* as)

-- | Build a `Coord` from a tuple of the same arity: @coordFromTuple (1, 2)@ is
-- @1 ':|' 2 ':|' 'EmptyCoord'@.
--
-- This exists for displacements, where a tuple used to typecheck directly ---
-- @c '.+^' (-1, -1)@ is now @c '.+^' coordFromTuple (-1, -1)@ --- but it is not
-- restricted to them, and builds an ordinary coordinate just as well.
--
-- @IsProductType@ is confined to these two signatures on purpose. It used to
-- sit in the context of everything that offsets, because 'Diff' was a tuple and
-- so had to be taken apart generically; making the displacement coord-shaped is
-- what removed it. Asking for it back here is opt-in, at a call site where the
-- tuple is a literal and the constraint is discharged on the spot.
--
-- There is deliberately no @coord2@ or two-dimensional pattern synonym. One
-- arity-generic function costs no per-arity code, which is the whole point of
-- the change these replace.
coordFromTuple :: IsProductType t xs => t -> Coord xs
coordFromTuple = Coord . productTypeFrom

-- | Take a `Coord` apart into a tuple of the same arity. The inverse of
-- 'coordFromTuple', and the way to destructure a displacement positionally:
-- @let (dx, dy) = coordToTuple (a '.-.' b)@.
coordToTuple :: IsProductType t xs => Coord xs -> t
coordToTuple = productTypeTo . unCoord

instance Field1 (Coord (a ': cs)) (Coord (a' ': cs)) a a' where
  _1 = coordHead

instance Field2 (Coord (a ': b ': cs)) (Coord (a ': b' ': cs)) b b' where
  _2 = coordTail . _1

instance Field3 (Coord (a ': b ': c ': cs)) (Coord (a ': b ': c' ': cs)) c c' where
  _3 = coordTail . _2

instance Field4 (Coord (a ': b ': c ': d ': cs)) (Coord (a ': b ': c ': d' ': cs)) d d' where
  _4 = coordTail . _3

instance Field5 (Coord (a ': b ': c ': d ': e ': cs)) (Coord (a ': b ': c ': d ': e' ': cs)) e e' where
  _5 = coordTail . _4

-- | Apply `Diff` to each element of a type level list. This is required as type families can't be partially applied.
type family MapDiff xs where
  MapDiff '[] = '[]
  MapDiff (x ': xs) = Diff x ': MapDiff xs

-- | The displacement between two coords is itself coord-shaped: a
-- @'Coord' cs@ is displaced by a @'Coord' ('MapDiff' cs)@, one 'Diff' per axis.
--
-- This follows @manifolds@, where @Needle@ is an associated type and a product
-- gets its own structurally --- @Needle (a,b) = (Needle a, Needle b)@. The
-- displacement here is a 'Coord' rather than a new product type, so it inherits
-- ':|', 'EmptyCoord', 'Show', 'Eq', 'AdditiveGroup' and 'Random' from the
-- instances above at no cost.
--
-- It replaces a @CoordDiff@ family whose instances were written out one per
-- arity, up to six. That was a real ceiling: a seven-axis 'Coord' had no
-- 'Diff', so no 'AffineSpace' instance and no 'offsetCoord'. 'MapDiff' recurses,
-- so there is no ceiling and no per-arity code. It also drops
-- @IsProductType (CoordDiff cs) (MapDiff cs)@ from the context, which is why
-- @generics-sop@ no longer appears in the signature of everything that offsets.
--
-- Tuple literals no longer typecheck as displacements. Use ':|', or
-- 'coordFromTuple' where a tuple reads better.
instance ( All AffineSpace cs
         , All AdditiveGroup (MapDiff cs)
         ) =>
         AffineSpace (Coord cs) where
    type Diff (Coord cs) = Coord (MapDiff cs)
    Coord a .-. Coord b =
        let helper ::
                   All AffineSpace xs => NP I xs -> NP I xs -> NP I (MapDiff xs)
            helper Nil Nil                 = Nil
            helper (I x :* xs) (I y :* ys) = I (x .-. y) :* helper xs ys
        in Coord $ helper a b
    Coord a .+^ Coord b =
        let helper :: All AffineSpace xs => NP I xs -> NP I (MapDiff xs) -> NP I xs
            helper Nil Nil                 = Nil
            helper (I x :* xs) (I y :* ys) = I (x .+^ y) :* helper xs ys
        in Coord $ helper a b

-- | Generate all possible coords in order
allCoord ::
       forall cs. (All IsCoordLifted cs)
    => [Coord cs]
allCoord =
    Coord <$>
    hsequence
        (hcpure (Proxy :: Proxy IsCoordLifted) (allCoordLike))

-- | The number of elements a coord can have. This is equal to the product of the `CoordSized` of each element
type family MaxCoordSize (cs :: [k]) :: GHC.Nat where
  MaxCoordSize '[] = 1
  MaxCoordSize ((c n) ': cs) = n GHC.* (MaxCoordSize cs)

-- | Convert a `Coord` to its position in a vector.
--
-- The layout is row major: the first axis is the most significant, so a step
-- along the last axis moves one place in the vector.
coordPosition :: forall cs. (All IsCoordLifted cs) => Coord cs -> Int
coordPosition (Coord a) = snd $ helper a
  where
    -- One pass, returning the size of the axes alongside the position, because
    -- the stride of an axis is exactly the size of the axes below it. The
    -- previous version recomputed that product with a separate traversal per
    -- axis and did the arithmetic in 'Integer', which cost a boxed 'Integer'
    -- per @natVal@ and a fresh 'NP' and list per axis: about 800 bytes for a
    -- single two-dimensional 'coordPosition', which then showed up multiplied
    -- by 90,000 in every indexed traversal.
    helper :: All IsCoordLifted xs => NP I xs -> (Int, Int)
    helper Nil = (1, 0)
    helper (I (c :: x) :* cs) =
        case helper cs of
            (stride, rest) ->
                let o = c ^. asOrdinal
                 in ( ordinalSize @(CoordNat x) * stride
                    , ordinalToInt o * stride + rest)

-- | The number of positions a @'Coord' cs@ ranges over: the product of the
-- sizes of its axes, and so the length of the vector inside a @'Grid' cs@.
--
-- This is 'MaxCoordSize' as a value. It asks only for @All IsCoordLifted cs@
-- rather than @KnownNat (MaxCoordSize cs)@, so it is available wherever a
-- coordinate can be taken apart at all --- in particular in the indexed
-- traversals, which do not carry the @KnownNat@.
coordSpaceSize :: forall cs. All IsCoordLifted cs => Int
coordSpaceSize =
    -- 'SList' carries no fields, so the head and tail of the list are named
    -- with a type abstraction. 'SCons' quantifies the tail before the head.
    case sList :: SList cs of
        SNil          -> 1
        SCons @xs @x  -> ordinalSize @(CoordNat x) * coordSpaceSize @xs

-- | The inverse of 'coordPosition': the coordinate stored at the given position
-- of a grid's vector, or 'Nothing' if there is no such position.
--
-- @coordFromPosition . coordPosition@ is @Just@ on every 'Coord', and
-- @coordPosition@ undoes it on every position in @[0, 'coordSpaceSize')@.
coordFromPosition ::
       forall cs. All IsCoordLifted cs
    => Int
    -> Maybe (Coord cs)
coordFromPosition p
    | p < 0 = Nothing
    | otherwise =
        case coordDigits p of
            -- A leftover of anything but zero means @p@ needed a place beyond
            -- the most significant axis, which is to say it was at least
            -- 'coordSpaceSize'. No separate bounds check is needed.
            (np, 0) -> Just $ Coord np
            _       -> Nothing

-- | Split a position into one ordinal per axis, least significant first, and
-- return what is left over above the most significant axis.
--
-- The tail is taken apart before the head precisely so that no stride has to be
-- known in advance: each axis takes the low digit of what the axes below it
-- did not consume, exactly as writing a number in a mixed radix does.
coordDigits ::
       forall xs. All IsCoordLifted xs
    => Int
    -> (NP I xs, Int)
coordDigits p =
    case sList :: SList xs of
        SNil -> (Nil, p)
        -- 'unsafeOrdinal' holds on @r@: 'quotRem' by a positive divisor with a
        -- non-negative numerator gives @0 <= r < size@, and @q@ is non-negative
        -- whenever the @p@ it came from was.
        SCons @ys @y ->
            case coordDigits @ys p of
                (rest, q) ->
                    case q `quotRem` ordinalSize @(CoordNat y) of
                        (q', r) ->
                            (I (review asOrdinal (unsafeOrdinal r)) :* rest, q')

-- | All Diffs of the members of the list must be equal
type family AllDiffSame a xs :: Constraint where
  AllDiffSame _ '[] = ()
  AllDiffSame a (x ': xs) = (Diff x ~ a, AllDiffSame a xs)

-- | Move by a signed displacement, or 'Nothing' if that leaves the grid.
--
-- The checked counterpart of @('.+^')@, and a drop-in for it: it takes the same
-- @'Diff' ('Coord' cs)@, so @offsetCoord c d@ replaces @c '.+^' d@ wherever the
-- caller wants to be told about the edge rather than silently pushed back from
-- it.
--
-- Each axis applies its own boundary policy, through
-- 'SizedGrid.Coord.Class.offsetIsCoord'. The whole offset succeeds only if
-- every axis does, so on a coord mixing a bounded axis with a torus axis the
-- torus half can wrap while the bounded half refuses:
--
-- > offsetCoord (0 :| 0 :| EmptyCoord :: Coord '[Clamped 5, Periodic 5])
-- >             (0 :| (-1) :| EmptyCoord)
-- >   == Just (0 :| 4 :| EmptyCoord)
-- > offsetCoord (0 :| 0 :| EmptyCoord :: Coord '[Clamped 5, Periodic 5])
-- >             ((-1) :| (-1) :| EmptyCoord)
-- >   == Nothing
offsetCoord ::
       ( All IsCoordLifted cs
       , AllDiffSame Integer cs
       )
    => Coord cs
    -> Diff (Coord cs)
    -> Maybe (Coord cs)
offsetCoord (Coord cs) (Coord d) = Coord <$> helper cs d
  where
    -- The coord drives the recursion, not the displacement: matching 'Nil' or
    -- ':*' on the first argument is what refines @xs@ far enough for GHC to
    -- reduce @MapDiff xs@ and match the second. The same shape as the helpers
    -- in the 'AffineSpace' instance above, and for the same reason.
    helper ::
           (All IsCoordLifted xs, AllDiffSame Integer xs)
        => NP I xs
        -> NP I (MapDiff xs)
        -> Maybe (NP I xs)
    helper Nil Nil = Just Nil
    helper (I x :* xs) (I dx :* dxs) =
        (\y ys -> I y :* ys) <$> offsetIsCoord x dx <*> helper xs dxs

-- | The values one axis can reach within @r@ steps of @c@, paired with the
-- number of steps it actually takes to get there.
--
-- Every value appears once. Where two offsets reach the same value --- which a
-- torus axis does as soon as @2 * r >= n@ --- the one that took fewer steps
-- wins, so the recorded distance is the true distance on that axis rather than
-- whichever offset the enumeration happened to try first. That is what makes
-- 'vonNeumannNeighbours' correct on a small torus instead of accidentally
-- right, and it is also what stops a bounded axis from reporting the same edge
-- cell several times.
--
-- Ordering is by the surviving offset, ascending, so the centre sits in the
-- middle and the caller sees a coordinate order that does not depend on the
-- boundary policy.
axisSteps :: forall x. IsCoordLifted x => Int -> x -> [(Int, x)]
axisSteps r c =
    [(abs d, v) | (d, v) <- reachable, not (any (beats (d, v)) reachable)]
  where
    reachable :: [(Int, x)]
    reachable =
        [(d, v) | d <- [-r .. r], Just v <- [offsetIsCoord c (toInteger d)]]
    -- Compared as an 'Int' through 'asOrdinal', so no 'Eq' is needed on the
    -- axis type itself.
    key :: x -> Int
    key v = ordinalToInt (v ^. asOrdinal)
    -- Fewer steps wins; an exact tie in distance goes to the lower offset, so
    -- the choice is total and the result does not depend on list order.
    beats :: (Int, x) -> (Int, x) -> Bool
    beats (d, v) (d', v') = key v' == key v && (abs d', d') < (abs d, d)

-- | Every coordinate within @r@ steps on each axis, paired with the total
-- number of steps taken across all axes.
--
-- The centre is the only entry whose total is zero, because a distance is never
-- negative, which is how both neighbourhood functions exclude it without ever
-- comparing coordinates for equality.
--
-- The axes are walked with the first outermost, so results come out in the same
-- row-major order 'coordPosition' lays a grid out in.
stepsWithin ::
       forall cs. All IsCoordLifted cs
    => Int
    -> Coord cs
    -> [(Int, Coord cs)]
stepsWithin r (Coord cs) = fmap Coord <$> go cs
  where
    go :: All IsCoordLifted xs => NP I xs -> [(Int, NP I xs)]
    go Nil = [(0, Nil)]
    go (I x :* xs) = do
        (d, v) <- axisSteps r x
        (s, rest) <- go xs
        return (d + s, I v :* rest)

-- | The Moore neighbourhood: every coordinate within @r@ steps on each axis
-- independently, excluding the centre.
--
-- Each axis applies its own boundary policy, so a bounded axis simply has fewer
-- neighbours near its edges while a torus axis always has the full complement:
--
-- > length (mooreNeighbours 1 c)   -- Coord '[Clamped 5, Clamped 5]
-- >   == 3 at a corner, 5 on an edge, 8 in the interior
-- > length (mooreNeighbours 1 c)   -- Coord '[Periodic 5, Periodic 5]
-- >   == 8 everywhere
--
-- The result never contains duplicates and never contains the centre, neither
-- of which the caller has to repair. This is the operation 'moorePoints' was
-- reaching for; that one was built on @('.+^')@, so on a bounded coord it
-- clamped rather than stopped, and a corner came back with nine results of
-- which four were distinct.
mooreNeighbours :: All IsCoordLifted cs => Int -> Coord cs -> [Coord cs]
mooreNeighbours r c = [n | (s, n) <- stepsWithin r c, s > 0]

-- | The von Neumann neighbourhood: the coordinates whose distances, summed over
-- every axis, come to at most @r@, excluding the centre. In two dimensions
-- @vonNeumannNeighbours 1@ is the four orthogonally adjacent cells; in three
-- dimensions it is six.
--
-- There is deliberately no @neighbours4@. The count is a fact about how many
-- dimensions the coordinate has, not about the operation, so a name carrying it
-- would state a two-dimensional assumption this library does not make.
--
-- On a torus axis with @2 * r >= n@ a cell can be nearer the other way round
-- than its offset suggests, and this counts the shorter route, so the result is
-- smaller than "every combination of offsets summing to @r@" would give. That
-- is the honest distance on a torus: on a 3-cycle every other cell really is
-- one step away.
vonNeumannNeighbours :: All IsCoordLifted cs => Int -> Coord cs -> [Coord cs]
vonNeumannNeighbours r c = [n | (s, n) <- stepsWithin r c, s > 0, s <= r]

-- | The Moore neighbourhood at radius one: the surrounding cells, diagonals
-- included, excluding the centre. The overwhelmingly common case of
-- 'mooreNeighbours'.
neighbours :: All IsCoordLifted cs => Coord cs -> [Coord cs]
neighbours = mooreNeighbours 1

-- | The number of steps between two values on a single axis, by the shorter
-- route if the axis offers more than one.
--
-- The lifted form of 'axisDistanceIsCoord', so each axis type answers for its
-- own boundary policy: a bounded axis measures straight, a
-- 'SizedGrid.Coord.Periodic.Periodic' one takes the shorter way round.
axisDistance :: forall x. IsCoordLifted x => x -> x -> Int
axisDistance = axisDistanceIsCoord @(CoordContainer x) @(CoordNat x)

-- | The per-axis distances between two coords, first axis first.
--
-- Both metrics below are folds of this, and it is the honest primitive when a
-- caller wants something neither of them provides --- a weighted metric, or the
-- axis that differs most.
axisDistances :: forall cs. All IsCoordLifted cs => Coord cs -> Coord cs -> [Int]
axisDistances (Coord as) (Coord bs) =
    hcollapse $ hczipWith (Proxy @IsCoordLifted) step as bs
  where
    step :: IsCoordLifted x => I x -> I x -> K Int x
    step (I a) (I b) = K (axisDistance a b)

-- | The Chebyshev distance: the largest per-axis distance, counting a diagonal
-- step as one. This is the metric 'mooreNeighbours' is a ball in ---
-- @mooreNeighbours r c@ is every coord whose 'coordDistance' from @c@ is in
-- @[1, r]@.
--
-- On a coord with no axes at all the distance is zero, which is the only answer
-- a space with one point can give.
--
-- Each axis applies its own boundary policy, so on a
-- @Coord '[Clamped 5, Periodic 5]@ the bounded axis measures straight while the
-- torus axis takes the shorter way round. That mixture is the case a caller
-- cannot easily write by hand, and it is the reason this is exported rather
-- than left as something every consumer reimplements against 'natVal'.
coordDistance :: All IsCoordLifted cs => Coord cs -> Coord cs -> Int
coordDistance a b = foldl' max 0 (axisDistances a b)

-- | The Manhattan distance: the per-axis distances summed, counting a diagonal
-- step as two. This is the metric 'vonNeumannNeighbours' is a ball in ---
-- @vonNeumannNeighbours r c@ is every coord whose 'coordManhattan' from @c@ is
-- in @[1, r]@.
coordManhattan :: All IsCoordLifted cs => Coord cs -> Coord cs -> Int
coordManhattan a b = sum (axisDistances a b)

-- | Which end of its axis a single coordinate sits at, or 'Nothing' if it is in
-- the interior.
--
-- The lifted form of 'SizedGrid.Coord.Class.axisBoundaryIsCoord', so each axis
-- answers for its own boundary policy: a bounded axis has two ends, a
-- 'SizedGrid.Coord.Periodic.Periodic' one has none.
axisBoundary :: forall x. IsCoordLifted x => x -> Maybe Extremum
axisBoundary = axisBoundaryIsCoord @(CoordContainer x) @(CoordNat x)

-- | Where each axis of a coord sits relative to its own ends, first axis first.
--
-- A list rather than an @NP (K (Maybe Extremum)) cs@, for the same reason
-- 'axisDistances' is one: the answer for an axis carries nothing of that axis's
-- type, so the indexed shape would cost every caller an @hcollapse@ and buy
-- back no information.
--
-- 'onBoundary' and 'isCorner' are both folds of this. It is the honest
-- primitive for the questions they do not answer --- /which/ corner, or which
-- edge a walker just met, which is what any reflect-or-stop rule needs.
axisBoundaries ::
       forall cs. All IsCoordLifted cs
    => Coord cs
    -> [Maybe Extremum]
axisBoundaries (Coord cs) = hcollapse $ hcmap (Proxy @IsCoordLifted) step cs
  where
    step :: IsCoordLifted x => I x -> K (Maybe Extremum) x
    step (I a) = K (axisBoundary a)

-- | Whether any axis is at one of its ends: the coordinate is somewhere on the
-- edge of the space.
--
-- The negation is the interior, and on a bounded coord that is exactly the set
-- of cells whose full Moore neighbourhood exists --- @not (onBoundary c)@ iff
-- @length ('neighbours' c) == 3 ^ d - 1@ in @d@ dimensions. That equivalence is
-- what a cellular automaton is reaching for when it special-cases edge cells,
-- and 'interiorCoords' hands it over directly.
--
-- On a coord with no axes at all the answer is 'False': there is no axis
-- reporting an end, and a space with one point has no edge for that point to be
-- on.
onBoundary :: All IsCoordLifted cs => Coord cs -> Bool
onBoundary = any isJust . axisBoundaries

-- | Whether every axis is at one of its ends: the coordinate is a corner of the
-- space.
--
-- An axis with no ends takes every corner with it, so this is 'False'
-- everywhere on an all-'SizedGrid.Coord.Periodic.Periodic' coord, and 'False'
-- on any coord with even one torus axis. That is the answer a check written
-- against 'GHC.TypeLits.natVal' gets wrong: comparing each axis to @0@ and
-- @n - 1@ finds four corners on a torus, which has none.
--
-- The empty coord is 'False' rather than a vacuous 'True'. A corner is a
-- boundary point, so @isCorner c@ implying @'onBoundary' c@ has to hold, and
-- 'onBoundary' has nothing to report on a space of one point.
isCorner :: All IsCoordLifted cs => Coord cs -> Bool
isCorner c =
    case axisBoundaries c of
        [] -> False
        bs -> all isJust bs

-- | Every coordinate that is not 'onBoundary', in 'allCoord' order.
--
-- The cells a neighbourhood-based rule can be applied to without deciding what
-- happens at an edge, because on a bounded coord these are precisely the cells
-- whose full Moore neighbourhood exists. On a torus that is every cell, and
-- this is 'allCoord'.
--
-- This enumerates. To read or write the interior of a
-- `SizedGrid.Grid.Grid.Grid` in place, compose 'onBoundary' with the indexed
-- traversal that grid already has --- there is no separate function for it
-- because there does not need to be:
--
-- > interior = itraversed . indices (not . onBoundary)
-- > boundary = itraversed . indices onBoundary
-- >
-- > lengthOf interior g        -- 9, on a Clamped 5 x Clamped 5
-- > g & interior .~ x          -- rewrite the interior, boundary untouched
--
-- @Grid cs@ is @TraversableWithIndex (Coord cs)@, so the index is the
-- coordinate and 'Control.Lens.indices' does the filtering. That composition is
-- read-write, which an enumeration cannot be; this function is for when the
-- coordinates themselves are what is wanted.
--
-- Being interior is not the same as having @3 ^ d - 1@ neighbours, and only
-- coincides with it on a bounded coord. A torus axis shorter than the
-- neighbourhood is the difference: on a @Coord '[Periodic 2, Periodic 2]@ every
-- cell is interior, and every cell has three neighbours rather than eight,
-- because offsets @-1@ and @+1@ wrap onto the same cell and 'neighbours' counts
-- it once. Nothing is missing there --- three is the whole of that space
-- besides the centre --- but a caller sizing a buffer from @3 ^ d - 1@ wants to
-- know.
interiorCoords :: All IsCoordLifted cs => [Coord cs]
interiorCoords = filter (not . onBoundary) allCoord

-- | Swap x and y for a coord in 2D space
tranposeCoord :: Coord '[a,b] -> Coord '[b,a]
tranposeCoord (Coord (a :* b :* Nil)) = Coord (b :* a :* Nil)

-- | The zero position for a coord
zeroCoord :: All IsCoordLifted cs => Coord cs
zeroCoord = Coord $ hcpure (Proxy :: Proxy IsCoordLifted) (I $ zeroPosition)

class AllSizedKnown (cs :: [Type]) where
  sizeProof :: Dict (KnownNat (MaxCoordSize cs))

instance AllSizedKnown '[] where
    sizeProof = Dict

instance (KnownNat n, AllSizedKnown as) =>
         AllSizedKnown ((c n) ': as) where
    sizeProof =
        withDict
            (sizeProof @as)
            (Dict \\ (timesNat @n @(MaxCoordSize as)))

class WeakenCoord as bs where
  weakenCoord :: Coord as -> Maybe (Coord bs)

instance WeakenCoord '[] '[] where
  weakenCoord c = Just c

instance (WeakenCoord as bs, IsCoord c, KnownNat m) =>
         WeakenCoord ((c n) ': as) ((c m) ': bs) where
    weakenCoord (a :| as) = do
        bs <- weakenCoord as
        b <- weakenIsCoord a
        return (b :| bs)

class StrengthenCoord as bs where
  strengthenCoord :: Coord as -> Coord bs

instance StrengthenCoord '[] '[] where
  strengthenCoord c = c

instance ( StrengthenCoord as bs
         , IsCoord c
         , n <= m
         , KnownNat m
         ) =>
         StrengthenCoord ((c n) ': as) ((c m) ': bs) where
  strengthenCoord (a :| as) = strengthenIsCoord a :| strengthenCoord as
