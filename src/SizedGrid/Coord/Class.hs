{-# LANGUAGE AllowAmbiguousTypes #-}

module SizedGrid.Coord.Class
  ( IsCoord(..)
  , IsCoordLifted(..)
  , IsCoordList(..)
  , IsCoordListF
  , MapDiff
  , AllDiffSame
  , Extremum(..)
  , maxCoordSize
  , allCoordLike
  ) where

import           SizedGrid.Ordinal

import           Control.Lens
import           Data.AffineSpace (Diff)
import           Data.Kind        (Constraint, Type)
import           Generics.SOP     (All, I (..), NP (..))
import           GHC.TypeLits

-- | The largest value a coord of size @n@ can hold, which is @n - 1@.
--
-- This used to be a method of `IsCoord` taking a @Proxy (c n)@. It never
-- depended on @c@ and no instance overrode it, and it could not: `asOrdinal` is
-- an @Iso' (c n) (Ordinal n)@, so a lawful coord type has exactly @n@
-- inhabitants whatever else it does. The size is now given visibly:
--
-- > maxCoordSize 10 == 9
--
-- The rule this follows throughout the library: a type argument is visible when
-- inference cannot recover it, and absent when it can. Compare `maxCoord`,
-- which needs no argument at all because its result type says everything.
maxCoordSize :: forall n -> KnownNat n => Integer
maxCoordSize n = fromIntegral (ordinalSize @n) - 1

-- | Which end of an axis a value sits at. The result of
-- 'axisBoundaryIsCoord' is a @Maybe Extremum@, where 'Nothing' is the interior:
-- an axis has two ends and a coordinate is at one of them, the other, or
-- neither.
--
-- Deliberately not a @Bool@. A caller that only wants "am I on an edge" has
-- 'SizedGrid.Coord.onBoundary', while a caller that has to /do/ something about
-- the edge --- reflect a walker, wrap a texture, place a border glyph --- needs
-- to know which end it met, and recovering that from a @Bool@ means going back
-- to the arithmetic this method exists to replace.
data Extremum
    = AtMin
    | AtMax
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | Everything that can be uses as a Coordinate. The only required function is `asOrdinal` and the type instance of `CoordSized`: the rest can be derived automatically.
class IsCoord (c :: Nat -> Type) where
  -- | As each coord represents a finite number of states, it must be isomorphic to an Ordinal
  asOrdinal :: Iso' (c n) (Ordinal n)

  -- | The origin. If c is an instance of `Monoid`, this should be mempty
  zeroPosition :: (1 <= n, KnownNat n) => c n
  default zeroPosition :: Monoid (c n) => c n
  zeroPosition = mempty

  -- | The maximum value of a coord.
  --
  -- The result type fixes both the coord type and its size, so there is nothing
  -- left to pass: the @Proxy n@ this used to take carried no information the
  -- caller had not already written down in the type of the result.
  --
  -- @1 <= n@ is required because there is no such value otherwise: a @c 0@ has
  -- no inhabitants at all. Without it this was 'Data.Maybe.fromJust' on
  -- 'Nothing' for @n ~ 0@.
  maxCoord :: (KnownNat n, 1 <= n) => c n
  maxCoord = review asOrdinal maxCoord

  -- | Recover the coord's value as a type-level 'Nat', with the evidence that
  -- it is in range, and hand it to the continuation as a required type
  -- argument:
  --
  -- > reifyCoord c $ \\m -> ...   -- m is a type, with KnownNat m and m + 1 <= n
  --
  -- This was @asSizeProxy@, whose continuation took a @Proxy m@. The name went
  -- with the 'Data.Proxy.Proxy'; it reifies a value as a type, which is what
  -- 'SizedGrid.Ordinal.reifyOrdinal' is called for doing to an
  -- 'SizedGrid.Ordinal.Ordinal'.
  --
  -- @KnownNat n@ is required because the evidence is produced by comparing
  -- against @n@ at runtime ('SizedGrid.Ordinal.reifyOrdinal'). It used to come
  -- from unpacking the 'SizedGrid.Ordinal.Ordinal' GADT, which is precisely the
  -- dictionary every ordinal was paying to carry.
  reifyCoord ::
         KnownNat n
      => c n
      -> (forall m -> (KnownNat m, m + 1 <= n) => x)
      -> x
  reifyCoord c = reifyCoord (view asOrdinal c)

  -- | Offset by a signed displacement, or 'Nothing' if that leaves the space.
  --
  -- This is the checked counterpart of @('Data.AffineSpace..+^')@. That
  -- operation has to be total to satisfy 'Data.AffineSpace.AffineSpace', so on
  -- a bounded coord it clamps, and a caller who wanted to know it had gone off
  -- the edge cannot tell: every off-grid offset folds back onto an edge cell.
  -- This one reports it instead.
  --
  -- The default is the bounds check, which is what a coord type with real
  -- edges wants. A coord whose space has no edges overrides it and is total:
  -- 'SizedGrid.Coord.Periodic.Periodic' wraps, so its 'offsetIsCoord' is
  -- always 'Just'.
  --
  -- The displacement is an 'Int', which is what
  -- @'Data.AffineSpace.Diff' ('SizedGrid.Coord.Clamped.Clamped' n)@ is, so this
  -- really is a drop-in for the operation it checks. It was an 'Integer' until
  -- sized-grid-0tj, so that a displacement too wide for an 'Int' would be
  -- rejected rather than narrowed into range; that width is not reachable now
  -- that the whole 'Data.AffineSpace.Diff' is an 'Int', and it was being paid
  -- for on every call by the arithmetic it forced. See 'offsetByPosition'.
  --
  -- @1 <= n@ because a @c 0@ has no inhabitants to offset, and because the
  -- instances that delegate to @('Data.AffineSpace..+^')@ need it.
  -- No @default@ signature: the bounds check needs only @KnownNat n@, which
  -- the method already provides. A @default@ line is for a default that needs
  -- /more/ than the method promises, and writing one here would only restate
  -- @1 <= n@ where it is unused.
  offsetIsCoord :: (KnownNat n, 1 <= n) => c n -> Int -> Maybe (c n)
  offsetIsCoord = offsetByPosition

  -- | The number of steps between two values on this axis, by the shorter
  -- route if the axis offers more than one.
  --
  -- This is the scalar the neighbourhood functions are built on. It is a method
  -- for the same reason 'offsetIsCoord' is: the answer is a property of the
  -- boundary policy, not of the two values. The default measures straight,
  -- which is right for an axis with real edges, and
  -- 'SizedGrid.Coord.Periodic.Periodic' overrides it to take the shorter way
  -- round --- on a 3-cycle every other cell really is one step away.
  --
  -- Consistency with 'offsetIsCoord' is the law: @axisDistanceIsCoord a b@ is
  -- the least @abs d@ for which @offsetIsCoord a d == Just b@. That is what
  -- 'SizedGrid.Coord.axisSteps' computes by enumeration, and the two agreeing
  -- is a property test rather than something the types can enforce.
  axisDistanceIsCoord :: (KnownNat n, 1 <= n) => c n -> c n -> Int
  axisDistanceIsCoord a b =
      abs (ordinalToInt (a ^. asOrdinal) - ordinalToInt (b ^. asOrdinal))

  -- | Which end of the axis this value sits at, or 'Nothing' if it is in the
  -- interior.
  --
  -- The third method whose answer is a property of the boundary policy rather
  -- than of the value, alongside 'offsetIsCoord' and 'axisDistanceIsCoord', and
  -- it is in the class for the same reason they are. The default is the bounds
  -- check, which is what an axis with real edges wants;
  -- 'SizedGrid.Coord.Periodic.Periodic' overrides it to 'Nothing' everywhere,
  -- because a torus has no edges and so no value is at one. That symmetry is
  -- the whole argument: a free function over 'SizedGrid.Coord.Coord' would have
  -- to pick one answer for every axis type, and the right answer differs.
  --
  -- Consistency with 'offsetIsCoord' is the law, and it is what makes this
  -- worth having rather than leaving each caller to compare against
  -- 'GHC.TypeLits.natVal': the result is @Just 'AtMin'@ exactly when stepping
  -- down leaves the space and @Just 'AtMax'@ exactly when stepping up does. A
  -- one-cell axis is the one place both hold at once, and there the answer is
  -- 'AtMin'. Like 'axisDistanceIsCoord', that agreement is a property test
  -- rather than something the types can enforce.
  --
  -- No @1 <= n@, unlike its two neighbours. Neither the default nor any
  -- instance needs a value to exist beyond the one it was handed, and a @c 0@
  -- has none to hand.
  axisBoundaryIsCoord :: KnownNat n => c n -> Maybe Extremum
  axisBoundaryIsCoord = axisBoundaryByPosition

  weakenIsCoord :: KnownNat m => c n -> Maybe (c m)
  weakenIsCoord = fmap (review asOrdinal) . weakenOrdinal . view asOrdinal

  strengthenIsCoord :: (KnownNat m, (n <= m)) => c n -> c m
  strengthenIsCoord = review asOrdinal . strengthenOrdinal . view asOrdinal

-- | The bounds check that 'offsetIsCoord' takes as its default.
--
-- Two comparisons rather than the @'numToOrdinal' (i + d)@ this used to be, and
-- the reason is allocation rather than correctness: 'numToOrdinal' compares in
-- 'Integer', so the old body converted both the position and the sum on every
-- call. This one stays in 'Int' throughout.
--
-- Both bounds are built from the coord and the size alone --- @hi - i@ lies in
-- @[0, hi]@ and @negate i@ in @[-hi, 0]@ --- so neither can overflow, and @d@ is
-- only ever compared against them, never added to anything until it is known to
-- be small. The surviving branch has @0 <= i + d <= hi@, which is
-- 'unsafeOrdinal''s precondition.
--
-- Worth being exact about what the 'Integer' was buying, because
-- @('Data.AffineSpace..+^')@ on 'SizedGrid.Coord.Clamped.Clamped' needed it and
-- this did not. There, an overflowing @i + b@ lands on a large negative number
-- and the clamp folds it to the /low/ edge, which is the wrong answer for a
-- large positive offset. Here every wrong answer is the same wrong answer ---
-- 'Nothing' --- and an overflowing sum is negative, so it was refused anyway.
-- Both are written to compare rather than add, so neither depends on that
-- argument holding.
--
-- Same shape as the 'SizedGrid.Coord.Clamped.Clamped' operation otherwise: it
-- meets the two out-of-range cases with the near edge where this one meets them
-- with 'Nothing'. That is the whole difference between the checked and the total
-- operation, and the two agreeing is the law on 'offsetIsCoord'.
--
-- Unexported, like 'axisBoundaryByPosition': it is the default's
-- implementation, not a second way to ask the question.
--
-- == Why this one is @INLINE@ (sized-grid-bw8)
--
-- Without the pragma the size is read from a run-time 'Natural' on every call,
-- per axis. 'IsCoord' is indexed by @c@ alone, so @n@ cannot arrive through the
-- instance the way it does for @('Data.AffineSpace..+^')@ on
-- 'SizedGrid.Coord.Clamped.Clamped' --- there @KnownNat n@ is in the /instance
-- context/, is fixed by instance resolution at a concrete axis, and the size
-- constant-folds. Here it is on the /method/, so it is a dictionary argument,
-- and out of line the body has nothing to fold against:
--
-- > $woffsetByPosition
-- >   = \\ @c @n $dIsCoord $dKnownNat c1 ww ->
-- >       case integerToInt# (integerFromNatural ($dKnownNat `cast` ...)) of ds
--
-- @INLINE@ is enough because the call sites already have the dictionary. Since
-- sized-grid-135 made the axis fold an instance method ('npOffset'), the fold
-- unrolls and each per-axis call sees a statically resolved @KnownNat@ --- at
-- @'[Clamped 300, Clamped 300]@ that is @main81 = NS 300##@, sitting right next
-- to the arithmetic and never folded only because the worker was out of line.
--
-- This is /not/ the case the note on 'IsCoordList' rules out. There @INLINE@
-- cannot rescue a fold that is self-recursive and polymorphic in the axis list,
-- because no amount of inlining gives the recursion a concrete list to unroll
-- at. This function is small and not recursive at all, and the work of making
-- its call sites concrete was already done. The two facts are compatible: a
-- class indexed by the axis list fixes the /fold/, and inlining fixes the
-- /step/ the fold now calls directly.
--
-- MEASURED on @offsetCoord x360000, checked, over Clamped 300x300@:
-- 56.4 ms and 199 MB to 6.85 ms and 35 MB. In the Core the call goes from
-- @$w$coffsetIsCoord main81 ds3 ww2@ to @'>#' x ('-#' 299# y1)@ inline, with
-- 'asOrdinal' reduced to coercions instead of applications of
-- @$fProfunctorFUN@ and @$fFunctorConst@, and the 'Maybe' to join points. What
-- is left is not the bounds check, which now allocates nothing: it is the
-- closure the benchmark's own comprehension builds per position.
--
-- @extend neighbourSum 50x50@ fell 8.88 ms and 28 MB to 7.01 ms and 25 MB in
-- the same change, without the neighbourhood fold being touched, because
-- 'SizedGrid.Coord.axisSteps' calls 'offsetIsCoord' per candidate. The rest of
-- that cost is the fold above it, which is sized-grid-5uq.
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
--
-- Written out here rather than inline in the class because the body needs @n@
-- to ask for 'ordinalSize', and a default method body has nowhere to bind it:
-- the @forall@ that would bring it into scope belongs to a signature the class
-- has no way to give a default. 'offsetByPosition' is here for the same reason
-- --- its default used to reach the size implicitly through 'numToOrdinal' and
-- so could stay inline, and asking for the size by name is what moved it out.
--
-- Unexported: it is the default's implementation, not a second way to ask the
-- question. An instance that wants the bounds check already has it.
axisBoundaryByPosition ::
       forall c n. (IsCoord c, KnownNat n)
    => c n
    -> Maybe Extremum
axisBoundaryByPosition c
    -- Order matters only on a one-cell axis, where the single value is both
    -- ends at once and this reports 'AtMin'. What downstream depends on is that
    -- it is not 'Nothing': the one cell of a 1x1 grid is all boundary and no
    -- interior.
    | i == 0 = Just AtMin
    | i == ordinalSize @n - 1 = Just AtMax
    | otherwise = Nothing
  where
    i = ordinalToInt (c ^. asOrdinal)

-- | Sometimes it useful to work with Coords of type *, not Nat -> *. This is away of doing so.
-- |
-- | It should be autogenerated for all valid instances of `IsCoord`
class ( x ~ ((CoordContainer x) (CoordNat x))
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

-- | Apply 'Diff' to each element of a type level list. This is required as type
-- families can't be partially applied.
--
-- The displacement between two coords is itself coord-shaped: a
-- @'SizedGrid.Coord.Coord' cs@ is displaced by a
-- @'SizedGrid.Coord.Coord' ('MapDiff' cs)@, one 'Diff' per axis.
--
-- This follows @manifolds@, where @Needle@ is an associated type and a product
-- gets its own structurally --- @Needle (a,b) = (Needle a, Needle b)@. The
-- displacement there is a 'SizedGrid.Coord.Coord' rather than a new product
-- type, so it inherits @(':|')@, 'SizedGrid.Coord.EmptyCoord', 'Show', 'Eq',
-- 'Data.AdditiveGroup.AdditiveGroup' and 'System.Random.Random' from the
-- 'SizedGrid.Coord.Coord' instances at no cost.
--
-- It replaces a @CoordDiff@ family whose instances were written out one per
-- arity, up to six. That was a real ceiling: a seven-axis
-- 'SizedGrid.Coord.Coord' had no 'Diff', so no 'Data.AffineSpace.AffineSpace'
-- instance and no 'SizedGrid.Coord.offsetCoord'. 'MapDiff' recurses, so there
-- is no ceiling and no per-arity code. It also drops
-- @IsProductType (CoordDiff cs) (MapDiff cs)@ from the context, which is why
-- @generics-sop@ no longer appears in the signature of everything that offsets.
--
-- Tuple literals no longer typecheck as displacements. Use @(':|')@, or
-- 'SizedGrid.Coord.coordFromTuple' where a tuple reads better.
--
-- It lives here rather than in "SizedGrid.Coord" because 'npOffset' is stated
-- in terms of it, and that method has to be in this module to be a method of
-- 'IsCoordList'.
type family MapDiff xs where
  MapDiff '[] = '[]
  MapDiff (x ': xs) = Diff x ': MapDiff xs

-- | All Diffs of the members of the list must be equal.
--
-- At a concrete list this reduces to one @~@ per axis and nothing else, so it
-- is discharged by the coercions that are already there and costs nothing at
-- run time. That is what makes it safe to write on 'npOffset', where a class
-- constraint would have handed back the run-time dictionary the method exists
-- to remove; see the note there.
type family AllDiffSame a xs :: Constraint where
  AllDiffSame _ '[] = ()
  AllDiffSame a (x ': xs) = (Diff x ~ a, AllDiffSame a xs)

-- | The per-axis obligations of 'IsCoordList', as a type family so that they
-- can be a superclass of it.
--
-- Without this, @'IsCoordList' (x ': xs)@ would not hand back
-- @'IsCoordList' xs@, and every induction over the axis list --- including
-- 'IsCoordList''s own instance below --- would stop typechecking one step in.
-- This is the shape @generics-sop@ gives its own @All@, for the same reason,
-- and it is what @UndecidableSuperclasses@ is on for.
type family IsCoordListF (cs :: [Type]) :: Constraint where
  IsCoordListF '[]        = ()
  IsCoordListF (x ': xs)  = (IsCoordLifted x, IsCoordList xs)

-- | A list of axes that a 'SizedGrid.Coord.Coord' can be built from, with the
-- row-major fold over that list available as a method.
--
-- This is @'All' 'IsCoordLifted' cs@ --- a superclass, so every body that held
-- that constraint before still holds it, and it is discharged by instance
-- resolution at a concrete list exactly as that constraint was --- plus the one
-- thing it cannot supply: an /instance method/ for the per-axis step.
--
-- == Why the fold has to be a method
--
-- This is a compilation fact rather than a matter of taste, and it was measured
-- rather than guessed.
--
-- 'SizedGrid.Coord.coordPosition' folds over the axis list. Written as a
-- /self-recursive function/ --- the @where@ helper this used to have --- that
-- fold can never unroll, because GHC does not inline a self-recursive binding
-- and the recursion is polymorphic: each call is at a shorter list. Specialising
-- at a concrete list rewrites the outermost call and leaves the tail going
-- through the same generic worker, which takes the @All IsCoordLifted@
-- dictionary at run time. So the first axis constant-folds and every axis after
-- it pays, in the Core, for two thunks to peel the dictionary, an 'Integer' from
-- @natVal@, a call through the 'asOrdinal' 'Control.Lens.Iso', and two boxed
-- 'Int's. @INLINE@ on the wrapper does not rescue it and neither does
-- @-fpolymorphic-specialisation -fspecialise-aggressively@; both were tried and
-- leave the worker byte-for-byte identical.
--
-- Written as an instance method the dictionary is resolved at compile time, one
-- instance per axis, so the fold unrolls and the sizes become literals: at
-- @'[Clamped 50, Clamped 50]@ the Core for 'SizedGrid.Coord.coordPosition' is
-- @+# (*# x 50#) y@ and nothing else. Measured on @extract 50x50@, that is 320
-- bytes a call against none at all; on @index x90000@, 27 MB against 34 bytes.
--
-- == Why not @cpara_SList@
--
-- Worth stating precisely, because the obvious objection is that
-- @generics-sop@ already ships an eliminator and this class reinvents it.
--
-- @cpara_SList@ is genuinely different from the @where@ helper: it is not
-- self-recursive in the Core, since each instance's method body calls the method
-- at a different, statically known dictionary. It /does/ unroll. A
-- 'SizedGrid.Coord.coordSpaceSize'-shaped fold through it constant-folds all the
-- way to a literal.
--
-- It unrolls only where the axis list is concrete, though, and that is the
-- catch: reaching such a place means @INLINE@ the whole way down. This function
-- is called from polymorphic instance methods --- @index@ on
-- @Representable (Grid cs)@, @gridIndex@ on @IsGrid@ --- so marking it @INLINE@
-- splices the eliminator into a body where @cs@ is still a variable and nothing
-- can resolve, and it builds the closure chain per call instead. Measured, that
-- is 584 bytes a call and 50 MB on @index x90000@: worse than the 320 bytes and
-- 27 MB it started at.
--
-- A method of a class indexed by the axis list has no such dependency on the
-- inliner. The chain resolves whenever the /dictionary/ is known, which happens
-- at specialisation and not only at inlining, and that is what makes it hold up
-- through the library's own polymorphic call path.
--
-- == On the method
--
-- 'sizeAndPosition' returns the size of the axes alongside the position because
-- the stride of an axis is exactly the size of the axes below it, so one pass
-- yields both. It is a fold accumulator rather than anything a caller wants,
-- and the two instances below cover every type-level list, so there is no
-- instance left for anyone to write. The same goes for 'npOffset'.
-- "SizedGrid.Coord" therefore re-exports the class without its methods.
class (IsCoordListF cs, All IsCoordLifted cs) => IsCoordList cs where
    -- | The product of the axis sizes, and the row-major position of the given
    -- coordinate within them.
    sizeAndPosition :: NP I cs -> (Int, Int)

    -- | Offset each axis by its own displacement, or 'Nothing' if any axis
    -- refuses. The fold behind 'SizedGrid.Coord.offsetCoord'.
    --
    -- == Why it is here and not on 'SizedGrid.Coord.AffineCoordList'
    --
    -- The per-axis step is 'offsetIsCoord', whose only obligation is
    -- 'IsCoordLifted' --- which is precisely what this class already supplies
    -- per axis. 'SizedGrid.Coord.AffineCoordList' is a separate class because
    -- /its/ step is @('Data.AffineSpace..+^')@ and needs
    -- @'Data.AffineSpace.AffineSpace' x@, which 'IsCoordLifted' does not give.
    -- The checked offset needs no such thing, so it belongs to the class that
    -- was already there.
    --
    -- == Why the displacement constraint is a type family
    --
    -- @'AllDiffSame' Int cs@ on a method signature is the one shape that does
    -- not undo the point of the exercise. It reduces at a concrete list to one
    -- @'Diff' x ~ Int@ per axis and stops --- equality evidence, erased before
    -- code generation. A /class/ constraint here, say @All Something cs@,
    -- would be a dictionary the method takes at run time, and peeling it per
    -- axis is exactly the cost that moving the fold into a class removes. That
    -- distinction was checked in the Core rather than assumed.
    npOffset ::
           AllDiffSame Int cs
        => NP I cs
        -> NP I (MapDiff cs)
        -> Maybe (NP I cs)

instance IsCoordList '[] where
    sizeAndPosition Nil = (1, 0)
    npOffset Nil Nil = Just Nil
    {-# INLINE sizeAndPosition #-}
    {-# INLINE npOffset #-}

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
    {-# INLINE sizeAndPosition #-}
    {-# INLINE npOffset #-}

instance IsCoord Ordinal where
    asOrdinal = id
    zeroPosition = minBound
    reifyCoord = reifyOrdinal
    maxCoord = maxBound

-- | Enumerate all possible values of a coord, in order
allCoordLike :: (1 <= n, IsCoord c, KnownNat n) => [c n]
allCoordLike = toListOf (traverse . re asOrdinal) [minBound .. maxBound]
