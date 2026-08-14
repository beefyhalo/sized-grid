{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Unlike `Grid.Sized.Grid.Grid.Grid`, `Coord`\'s constructor is safe to export:
-- an @NP I cs@ is correct by construction for its own index, so there is no
-- length invariant a caller could break.
module Grid.Sized.Coord
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
    -- * Rays
  , OffGrid(..)
  , offsetCoordUpTo
  , coordRay
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
    -- | Like 'IsCoordList', 'AffineCoordList' is re-exported without its
    -- methods: they are the fold itself, and the two instances below already
    -- cover every type-level list. What a caller needs is the constraint.
  , AffineCoordList
  , AllDiffSame
  , AllSizedKnown(..)
    -- | 'IsCoordList' is re-exported without its methods: they are folds over
    -- the axis list, and its two instances already cover every type-level
    -- list, so there is none left to write. Import "Grid.Sized.Coord.Class" if
    -- you want to see them anyway.
  , IsCoordList
  ) where

import           Grid.Sized.Coord.Class
import           Grid.Sized.Ordinal

import           Control.Applicative   (empty)
import           Control.Lens          hiding (from, to)
import           Control.Monad.State
import           Data.AdditiveGroup
import           Data.Aeson
import           Data.AffineSpace
import           Data.Constraint
import           Data.Kind (Type)
import           Data.List             (intercalate, unfoldr)
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

-- | The axis-list fold behind @('.+^')@ and @('.-.')@ on a 'Coord', as an
-- instance method so that it unrolls.
--
-- == Why this is a class and not two @where@ helpers
--
-- It used to be the helpers, and they were the whole cost of offsetting. This
-- is the defect the note on 'Grid.Sized.Coord.Class.IsCoordList' describes,
-- met a second time: a fold written as a self-recursive function cannot
-- unroll, because GHC does not inline a self-recursive binding and each call
-- is at a shorter list, so specialising rewrites only the outermost call and
-- every axis after the first goes through a generic worker holding the
-- @All AffineSpace@ dictionary at run time.
--
-- Measured on 360,000 offsets through a two-axis 'Coord', against the same
-- 360,000 on a bare @'Grid.Sized.Coord.Clamped.Clamped' 300@ axis as a control:
--
-- > through a Coord     28.5 ms / 126 MB   ->   2.02 ms / 53 B
-- > bare axis, control   2.27 ms /  94 KB  ->   2.30 ms / 94 KB
--
-- The control does not move, which is what says the change is the fold and not
-- the arithmetic. sized-grid-0tj had already made the per-axis step
-- allocation-free by giving 'Data.AffineSpace.Diff' an 'Int' representation;
-- what was left was what a 'Coord' added on top, and it is now nothing. 53
-- bytes is the whole benchmark rather than per call: the axes unroll, the
-- @NP@ is built and consumed in registers, and 360,000 offsets allocate less
-- than one of them used to.
--
-- The 'Coord' loop now beats its own control, which is not a paradox: the
-- control walks a list of 360,000 'Int's to have something to offset, and that
-- list is the 94 KB.
--
-- == Why not a method of 'Grid.Sized.Coord.Class.IsCoordList'
--
-- The per-axis step needs @'AffineSpace' x@, which
-- 'Grid.Sized.Coord.Class.IsCoordLifted' does not supply, and adding
-- @All AffineSpace cs@ to the /method/ signature there would hand back the
-- run-time dictionary that moving the fold into a class exists to remove. An
-- axis list is also offsettable under conditions that have nothing to do with
-- being indexable: 'Grid.Sized.Ordinal.Ordinal' is an
-- 'Grid.Sized.Coord.Class.IsCoord' with no 'AffineSpace' instance at all.
--
-- == What it costs a caller
--
-- @All AffineSpace cs@ is a superclass, so every /use/ of the 'AffineSpace'
-- instance still typechecks unchanged. Only polymorphic code that wrote
-- @All AffineSpace cs@ in a signature in order to /discharge/ that instance
-- has to say 'AffineCoordList' instead; code at a concrete axis list --- which
-- is most code, including all of ../aoc --- resolves it by instance
-- resolution and never names it.
class All AffineSpace cs => AffineCoordList cs where
    -- | Add a displacement to a coordinate, one axis at a time.
    npAdd :: NP I cs -> NP I (MapDiff cs) -> NP I cs
    -- | The displacement between two coordinates, one axis at a time.
    npSub :: NP I cs -> NP I cs -> NP I (MapDiff cs)

instance AffineCoordList '[] where
    npAdd Nil Nil = Nil
    npSub Nil Nil = Nil
    {-# INLINE npAdd #-}
    {-# INLINE npSub #-}

instance (AffineSpace x, AffineCoordList xs) => AffineCoordList (x ': xs) where
    -- The coord drives the match, as in 'offsetCoord': matching ':*' on the
    -- first argument refines @xs@ far enough for @MapDiff@ to reduce and the
    -- second to match.
    npAdd (I x :* xs) (I y :* ys) = I (x .+^ y) :* npAdd xs ys
    npSub (I x :* xs) (I y :* ys) = I (x .-. y) :* npSub xs ys
    {-# INLINE npAdd #-}
    {-# INLINE npSub #-}

instance ( AffineCoordList cs
         , All AdditiveGroup (MapDiff cs)
         ) =>
         AffineSpace (Coord cs) where
    type Diff (Coord cs) = Coord (MapDiff cs)
    Coord a .-. Coord b = Coord (npSub a b)
    Coord a .+^ Coord b = Coord (npAdd a b)

-- | Generate all possible coords in order
allCoord ::
       forall cs. (IsCoordList cs)
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
coordPosition :: forall cs. IsCoordList cs => Coord cs -> Int
coordPosition (Coord a) = snd (sizeAndPosition a)

-- The fold this used to carry as a @where@ helper is now 'sizeAndPosition', an
-- 'IsCoordList' method, and the reason is entirely about what GHC can compile
-- rather than about where the code reads best; see the note on that class. The
-- short of it: a fold written as a function is polymorphically recursive over
-- the axis list, GHC unrolls one level of it and leaves the rest to a shared
-- worker holding the dictionary at run time, and so every axis after the first
-- pays for dictionary peeling, an 'Integer' from @natVal@ and a trip through
-- the 'asOrdinal' 'Control.Lens.Iso'. As a method the dictionaries are known at
-- compile time and the whole thing constant-folds.
--
-- 'INLINE' rather than the 'INLINABLE' that used to be here: what has to reach
-- the consumer is the instance chain, which does that on its own, and this
-- wrapper is now small enough to want to disappear at the call site.
{-# INLINE coordPosition #-}

-- | The number of positions a @'Coord' cs@ ranges over: the product of the
-- sizes of its axes, and so the length of the vector inside a @'Grid' cs@.
--
-- This is 'MaxCoordSize' as a value. It asks only for @IsCoordList cs@
-- rather than @KnownNat (MaxCoordSize cs)@, so it is available wherever a
-- coordinate can be taken apart at all --- in particular in the indexed
-- traversals, which do not carry the @KnownNat@.
coordSpaceSize :: forall cs. IsCoordList cs => Int
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
       forall cs. IsCoordList cs
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
       forall xs. IsCoordList xs
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

-- | Move by a signed displacement, or 'Nothing' if that leaves the grid.
--
-- The checked counterpart of @('.+^')@, and a drop-in for it: it takes the same
-- @'Diff' ('Coord' cs)@, so @offsetCoord c d@ replaces @c '.+^' d@ wherever the
-- caller wants to be told about the edge rather than silently pushed back from
-- it.
--
-- Each axis applies its own boundary policy, through
-- 'Grid.Sized.Coord.Class.offsetIsCoord'. The whole offset succeeds only if
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
       ( IsCoordList cs
       , AllDiffSame Int cs
       )
    => Coord cs
    -> Diff (Coord cs)
    -> Maybe (Coord cs)
-- The fold this used to carry as a @where@ helper is now
-- 'Grid.Sized.Coord.Class.npOffset', an 'IsCoordList' method, for the reason
-- given on that class and on 'AffineCoordList': a fold written as a
-- self-recursive function cannot unroll, so every axis after the first peeled
-- the axis-list dictionary at run time. Same defect as the one
-- 'AffineCoordList' fixed, one function along.
--
-- Measured on @offsetCoord x360000, checked@ in bench/Main.hs, which had to be
-- written for this: nothing in the suite reached 'offsetCoord' at all, because
-- @('.+^')@ and this are folds over two different classes. 268 MB and 73.0 ms
-- before, 199 MB and 57.0 ms after, and the Core at a two-axis list unrolls to
-- two 'Grid.Sized.Coord.Class.offsetIsCoord' calls with no axis-list dictionary
-- and no evidence passed for @'AllDiffSame' Int cs@.
--
-- What did /not/ move is @extend neighbourSum 50x50@, the benchmark
-- sized-grid-135 was filed against on the assumption that neighbourhoods
-- offset. They do not: 'neighbours' goes through 'stepsWithin', which
-- enumerates each axis with 'axisSteps' and never calls this function. That
-- fold is a third instance of the same defect and is filed separately.
--
-- The 199 MB that remain are below this function rather than in it. The Core
-- shows @offsetByPosition@ receiving its @KnownNat@ as a run-time 'Natural'
-- and doing @integerToInt# (integerFromNatural ...)@ on every call, per axis,
-- where the @('.+^')@ path has the size folded to a literal. Also filed
-- separately; it is the per-axis step, not the fold this note is about.
offsetCoord (Coord cs) (Coord d) = Coord <$> npOffset cs d

-- | Where a walk left the grid: the last coordinate that was still on it, and
-- how many whole steps it managed before the next one would have left.
--
-- This is precisely what @'offsetCoord' c d == Nothing@ throws away. It follows
-- @manifolds@' @(.+^|)@, which answers a displacement with
-- @Either (Boundary m, Scalar (Needle m)) (Interior m)@ --- either you arrived
-- in the interior, or here is the boundary point you met and how far along the
-- displacement you got.
data OffGrid cs = OffGrid
    { lastInside :: Coord cs
      -- ^ The final coordinate still on the grid, which is where the walk
      -- started if it never got anywhere.
    , stepsTaken :: Int
      -- ^ How many whole steps succeeded, and so @0@ exactly when the first
      -- step was already off the grid.
    } deriving (Generic)

deriving instance All Eq cs => Eq (OffGrid cs)

deriving instance All Show cs => Show (OffGrid cs)

-- | Take up to @n@ steps of @d@ from @c@, and say where the grid ran out if it
-- did: @Right@ is the coordinate @n@ whole steps away, @Left@ is how far the
-- walk got.
--
-- The informative 'offsetCoord'. That one answers a walk that leaves with
-- 'Nothing', which says neither where it left, nor how many steps succeeded
-- first, so a caller who needs either has to rediscover it by stepping one cell
-- at a time and keeping the last success --- which is this function.
--
-- > offsetCoordUpTo 2 (0 :| 0 :| EmptyCoord :: Coord '[Clamped 5, Clamped 5])
-- >                 (1 :| 1 :| EmptyCoord)
-- >   == Right (2 :| 2 :| EmptyCoord)
-- > offsetCoordUpTo 5 (2 :| 2 :| EmptyCoord :: Coord '[Clamped 5, Clamped 5])
-- >                 (1 :| 1 :| EmptyCoord)
-- >   == Left (OffGrid (4 :| 4 :| EmptyCoord) 2)
--
-- 'offsetCoord' is the one-step case with the edge forgotten, and that is a law
-- worth stating even though the definition runs the other way round:
--
-- > offsetCoord c d == either (const Nothing) Just (offsetCoordUpTo 1 c d)
--
-- The single displacement is the primitive because it is the one operation the
-- axes answer for --- 'Grid.Sized.Coord.Class.offsetIsCoord' applies a whole
-- 'Diff' per axis --- so counting steps is iteration on top of it rather than
-- something 'offsetCoord' could be carved out of.
--
-- A step is a whole @d@, not a subdivision of one. On a lattice there is
-- nothing between @c@ and @c '.+^' d@ to stop at unless @d@ is itself a
-- multiple of some shorter displacement, so "how far along the displacement"
-- counts copies of @d@; a caller who wants the finer walk passes the finer @d@.
-- That is also why @'lastInside'@ is on the boundary when @d@ is one cell wide
-- and need not be otherwise: a step of three cells can leave the grid from the
-- interior, and then the last coordinate that was inside is an interior one.
-- Where the step is unit-sized, @'axisBoundaries' . 'lastInside'@ names the edge
-- the walk met.
--
-- @n <= 0@ is @Right c@: a walk of no steps cannot leave.
--
-- A 'Grid.Sized.Coord.Periodic.Periodic' axis never refuses a step, so on an
-- all-@Periodic@ coord the answer is @Right@ for every @n@ and every @d@ --- a
-- torus has no boundary to report, which is the same fact 'axisBoundary' states
-- for a single value. Mixed coords are the interesting case: the bounded axis
-- decides when the walk ends while the torus axis keeps wrapping.
offsetCoordUpTo ::
       ( IsCoordList cs
       , AllDiffSame Int cs
       )
    => Int
    -> Coord cs
    -> Diff (Coord cs)
    -> Either (OffGrid cs) (Coord cs)
offsetCoordUpTo n c d = go n c 0
  where
    go k x s
        | k <= 0 = Right x
        | otherwise =
            case offsetCoord x d of
                Nothing -> Left (OffGrid x s)
                Just y  -> go (k - 1) y (s + 1)

-- | The ray from @c@ in direction @d@: @c '.+^' d@, then @c '.+^' 2d@, and so on
-- for as long as the grid lasts.
--
-- The start is not included, for the same reason the neighbourhoods exclude
-- their centre: the caller has it already, and every use --- reading the three
-- cells beyond a letter, tracing a beam to the wall, casting a line of sight
-- --- wants what is ahead. So @take k (coordRay c d)@ is the first @k@ cells
-- along the ray, and it is shorter than @k@ exactly when the grid ran out
-- first.
--
-- > coordRay (0 :| 0 :| EmptyCoord :: Coord '[Clamped 5, Clamped 5])
-- >          (1 :| 1 :| EmptyCoord)
-- >   == [1 :| 1, 2 :| 2, 3 :| 3, 4 :| 4]
--
-- Infinite when nothing can stop it, which is every ray on an
-- all-'Grid.Sized.Coord.Periodic.Periodic' coord and every ray with a zero
-- displacement. That is not a special case to guard: it is what a torus is, and
-- the list is lazy, so take what you need.
--
-- The counted walk is 'offsetCoordUpTo', and the two are one walk seen twice:
-- @offsetCoordUpTo n c d@ succeeds exactly when @coordRay c d@ has at least @n@
-- cells, and then names the @n@th.
coordRay ::
       ( IsCoordList cs
       , AllDiffSame Int cs
       )
    => Coord cs
    -> Diff (Coord cs)
    -> [Coord cs]
coordRay c d = unfoldr (\x -> (\y -> (y, y)) <$> offsetCoord x d) c

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
       forall cs. IsCoordList cs
    => Int
    -> Coord cs
    -> [(Int, Coord cs)]
-- The fold this used to carry as a @where@ helper is now
-- 'Grid.Sized.Coord.Class.npStepsWithin', an 'IsCoordList' method, for the
-- reason given on that class: a fold written as a self-recursive function
-- cannot unroll, so every axis after the first peeled the axis-list dictionary
-- at run time. The third instance of that defect, after 'AffineCoordList' and
-- 'offsetCoord', and the one the neighbourhood workloads actually run ---
-- neither of the other two fixes moved @extend neighbourSum@ at all, because
-- neither fold is on this path.
stepsWithin r (Coord cs) = fmap Coord <$> npStepsWithin r cs

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
mooreNeighbours :: IsCoordList cs => Int -> Coord cs -> [Coord cs]
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
vonNeumannNeighbours :: IsCoordList cs => Int -> Coord cs -> [Coord cs]
vonNeumannNeighbours r c = [n | (s, n) <- stepsWithin r c, s > 0, s <= r]

-- | The Moore neighbourhood at radius one: the surrounding cells, diagonals
-- included, excluding the centre. The overwhelmingly common case of
-- 'mooreNeighbours'.
neighbours :: IsCoordList cs => Coord cs -> [Coord cs]
neighbours = mooreNeighbours 1

-- | The number of steps between two values on a single axis, by the shorter
-- route if the axis offers more than one.
--
-- The lifted form of 'axisDistanceIsCoord', so each axis type answers for its
-- own boundary policy: a bounded axis measures straight, a
-- 'Grid.Sized.Coord.Periodic.Periodic' one takes the shorter way round.
axisDistance :: forall x. IsCoordLifted x => x -> x -> Int
axisDistance = axisDistanceIsCoord @(CoordContainer x) @(CoordNat x)

-- | The per-axis distances between two coords, first axis first.
--
-- Both metrics below are folds of this, and it is the honest primitive when a
-- caller wants something neither of them provides --- a weighted metric, or the
-- axis that differs most.
axisDistances :: forall cs. IsCoordList cs => Coord cs -> Coord cs -> [Int]
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
coordDistance :: IsCoordList cs => Coord cs -> Coord cs -> Int
coordDistance a b = foldl' max 0 (axisDistances a b)

-- | The Manhattan distance: the per-axis distances summed, counting a diagonal
-- step as two. This is the metric 'vonNeumannNeighbours' is a ball in ---
-- @vonNeumannNeighbours r c@ is every coord whose 'coordManhattan' from @c@ is
-- in @[1, r]@.
coordManhattan :: IsCoordList cs => Coord cs -> Coord cs -> Int
coordManhattan a b = sum (axisDistances a b)

-- | Which end of its axis a single coordinate sits at, or 'Nothing' if it is in
-- the interior.
--
-- The lifted form of 'Grid.Sized.Coord.Class.axisBoundaryIsCoord', so each axis
-- answers for its own boundary policy: a bounded axis has two ends, a
-- 'Grid.Sized.Coord.Periodic.Periodic' one has none.
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
       forall cs. IsCoordList cs
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
onBoundary :: IsCoordList cs => Coord cs -> Bool
onBoundary = any isJust . axisBoundaries

-- | Whether every axis is at one of its ends: the coordinate is a corner of the
-- space.
--
-- An axis with no ends takes every corner with it, so this is 'False'
-- everywhere on an all-'Grid.Sized.Coord.Periodic.Periodic' coord, and 'False'
-- on any coord with even one torus axis. That is the answer a check written
-- against 'GHC.TypeLits.natVal' gets wrong: comparing each axis to @0@ and
-- @n - 1@ finds four corners on a torus, which has none.
--
-- The empty coord is 'False' rather than a vacuous 'True'. A corner is a
-- boundary point, so @isCorner c@ implying @'onBoundary' c@ has to hold, and
-- 'onBoundary' has nothing to report on a space of one point.
isCorner :: IsCoordList cs => Coord cs -> Bool
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
-- `Grid.Sized.Grid.Grid.Grid` in place, compose 'onBoundary' with the indexed
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
interiorCoords :: IsCoordList cs => [Coord cs]
interiorCoords = filter (not . onBoundary) allCoord

-- | Swap x and y for a coord in 2D space
tranposeCoord :: Coord '[a,b] -> Coord '[b,a]
tranposeCoord (Coord (a :* b :* Nil)) = Coord (b :* a :* Nil)

-- | The zero position for a coord
zeroCoord :: IsCoordList cs => Coord cs
zeroCoord = Coord $ hcpure (Proxy :: Proxy IsCoordLifted) (I $ zeroPosition)

class AllSizedKnown (cs :: [Type]) where
  sizeProof :: Dict (KnownNat (MaxCoordSize cs))

instance AllSizedKnown '[] where
    sizeProof = Dict

instance (KnownNat n, AllSizedKnown as) =>
         AllSizedKnown ((c n) ': as) where
    sizeProof = withDict (sizeProof @as) Dict

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
