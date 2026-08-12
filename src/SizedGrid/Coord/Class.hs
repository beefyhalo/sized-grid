{-# LANGUAGE AllowAmbiguousTypes #-}

module SizedGrid.Coord.Class
  ( IsCoord(..)
  , IsCoordLifted(..)
  , IsCoordList(..)
  , IsCoordListF
  , Extremum(..)
  , maxCoordSize
  , allCoordLike
  ) where

import           SizedGrid.Ordinal

import           Control.Lens
import           Data.Kind      (Constraint, Type)
import           Generics.SOP   (All, I (..), NP (..))
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
  -- @1 <= n@ because a @c 0@ has no inhabitants to offset, and because the
  -- instances that delegate to @('Data.AffineSpace..+^')@ need it.
  -- No @default@ signature: the bounds check needs only @KnownNat n@, which
  -- the method already provides. A @default@ line is for a default that needs
  -- /more/ than the method promises, and writing one here would only restate
  -- @1 <= n@ where it is unused.
  offsetIsCoord :: (KnownNat n, 1 <= n) => c n -> Integer -> Maybe (c n)
  offsetIsCoord c d =
      -- Through 'Integer' rather than 'Int': 'numToOrdinal' compares against
      -- the size in 'Integer', so a displacement too wide for an 'Int' is
      -- rejected rather than wrapped into range.
      review asOrdinal <$>
      numToOrdinal (toInteger (ordinalToInt (c ^. asOrdinal)) + d)

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

-- | The bounds check that 'axisBoundaryIsCoord' takes as its default.
--
-- Written out here rather than inline in the class because the body needs @n@
-- to ask for 'ordinalSize', and a default method body has nowhere to bind it:
-- the @forall@ that would bring it into scope belongs to a signature the class
-- has no way to give a default. Compare 'offsetIsCoord', whose default gets the
-- size implicitly through 'numToOrdinal' and so can stay inline.
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
-- instance left for anyone to write. "SizedGrid.Coord" therefore re-exports the
-- class without it.
class (IsCoordListF cs, All IsCoordLifted cs) => IsCoordList cs where
    -- | The product of the axis sizes, and the row-major position of the given
    -- coordinate within them.
    sizeAndPosition :: NP I cs -> (Int, Int)

instance IsCoordList '[] where
    sizeAndPosition Nil = (1, 0)
    {-# INLINE sizeAndPosition #-}

instance (IsCoordLifted x, IsCoordList xs) => IsCoordList (x ': xs) where
    sizeAndPosition (I c :* cs) =
        case sizeAndPosition cs of
            (stride, rest) ->
                ( ordinalSize @(CoordNat x) * stride
                , ordinalToInt (c ^. asOrdinal) * stride + rest)
    {-# INLINE sizeAndPosition #-}

instance IsCoord Ordinal where
    asOrdinal = id
    zeroPosition = minBound
    reifyCoord = reifyOrdinal
    maxCoord = maxBound

-- | Enumerate all possible values of a coord, in order
allCoordLike :: (1 <= n, IsCoord c, KnownNat n) => [c n]
allCoordLike = toListOf (traverse . re asOrdinal) [minBound .. maxBound]
