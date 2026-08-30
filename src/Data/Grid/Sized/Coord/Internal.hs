{-# LANGUAGE AllowAmbiguousTypes #-}

-- | The 'Coord' representation and the instances that belong with it.
--
-- Hidden, because it exports the raw @Coord@ constructor. That constructor is
-- no more powerful than the public
-- 'Data.Grid.Sized.Coord.unsafeCoordFromPosition' \/ 'coordPosition' pair --- a
-- coordinate /is/ its position (sized-grid-adr.16) --- but keeping it behind a
-- hidden module means the range invariant has one documented public entrance
-- rather than two, and the sibling @Data.Grid.Sized.Coord.*@ modules that do
-- arithmetic on the position can still reach it without going through a
-- checked constructor they would only have to discharge again.
--
-- Everything here is re-exported from "Data.Grid.Sized.Coord"; import that.
module Data.Grid.Sized.Coord.Internal
  ( -- * Representation
    Coord(..)
  , unCoord
  , pattern (:|)
  , pattern EmptyCoord
  , coordSplit
    -- * Building and taking apart
  , singleCoord
  , appendCoord
  , coordFromTuple
  , coordToTuple
  , transposeCoord
  , zeroCoord
  , allCoord
  , coordPosition
  , coordIndices
  , coordIndices2
  , coordFromPosition
  , unsafeCoordFromPosition
  , coordSpaceSize
  , axisCount
    -- * Affine structure
  , AffineCoordList(..)
    -- * Type-level machinery
  , Length
  , MaxCoordSize
  , AllSizedKnown(..)
  , SizeProof(..)
  ) where

import           Data.Grid.Sized.Coord.Class
import           Data.Grid.Sized.Coord.Delta

import           Control.Applicative   (empty)
import           Control.DeepSeq       (NFData (..))
import           Control.Monad.State
import           Data.AdditiveGroup
import           Data.Aeson
import           Data.AffineSpace
import           Data.Finitary          (Finitary (..))
import           Data.Group            (Abelian, Group (..))
import           Data.Hashable         (Hashable (..))
import           Data.Ix               (Ix (..))
import qualified Data.Ix               as Ix
import           Data.Kind (Type)
import           Data.List             (intercalate)
import           Data.Universe.Class   (universe, universeF)
import qualified Data.Universe.Class   as U
import qualified Data.Vector           as V
import           Generics.SOP          hiding (Generic, S, Z)
import qualified Generics.SOP          as SOP
import qualified GHC.TypeLits          as GHC
import           System.Random         (Random (..))

type family Length cs where
  Length '[] = 0
  Length (c ': cs) = (GHC.+) 1 (Length cs)

-- | A coordinate: its row-major position within @cs@, and nothing else.
--
-- __Invariant:__ the position is in @[0, 'MaxCoordSize' cs)@, maintained by
-- every constructor in this module. The raw constructor is deliberately not
-- exported; build one with @(':|')@, 'coordFromPosition', 'zeroCoord' or
-- 'allCoord', and take one apart with @(':|')@ or 'unCoord'.
--
-- 'Eq' and 'Ord' come from the 'Int'. Two coordinates are equal exactly when
-- every axis agrees, which is exactly when their positions do, so the
-- @All Eq cs@ context the spine version needed is gone along with the
-- per-axis dictionary fold behind it. 'Ord' is unchanged in meaning too:
-- row-major position order /is/ lexicographic order on the axis indices, with
-- the first axis most significant.
-- The kind signature is not decoration: the old representation, @NP I cs@,
-- pinned @cs@ to @[Type]@ on its own. An 'Int' mentions nothing, so without
-- this GHC generalises @cs@ over any kind and @Coord '[]@ at two different
-- kinds stops being one type.
newtype Coord (cs :: [Type]) = Coord Int
  deriving newtype (Eq, Ord, NFData)

-- | Nominal, not the phantom role GHC would infer from a representation that
-- no longer mentions @cs@. A phantom role would let
-- @coerce :: Coord '[Clamped 9] -> Coord '[Clamped 3]@ forge an out-of-range
-- coordinate --- exactly the hole
-- 'Data.Grid.Sized.Ordinal.Ordinal'\'s own role annotation closes, and the one
-- @tests\/compile-fail@ checks stays shut.
type role Coord nominal


-- | The coordinate's value axis by axis, rebuilt.
--
-- Was a field accessor. It costs one 'quotRem' per axis now, which adr.8
-- measured against the worst case it could construct and found repaid several
-- times over: producing a coordinate costs more than decoding one.
unCoord :: forall cs. IsCoordList cs => Coord cs -> NP I cs
unCoord (Coord p) = npFromPosition p
{-# INLINE unCoord #-}

-- | Peel the first axis off a coordinate: a division by the stride of the
-- axes to its right.
coordSplit ::
       forall c cs. (IsCoordLifted c, IsCoordList cs)
    => Coord (c ': cs)
    -> (c, Coord cs)
coordSplit (Coord p) =
    case p `quotRem` coordListSize @cs of
        (i, r) -> (unsafeFromAxisIndex i, Coord r)
{-# INLINE coordSplit #-}

-- | Cons. A field read in each direction until sized-grid-adr.16; a 'quotRem'
-- one way and a multiply-add the other now.
--
-- The @('IsCoordLifted' c, 'IsCoordList' cs)@ context is what pays for that
-- arithmetic. Any caller that already has @'IsCoordList' (c ': cs)@ has it,
-- since 'Data.Grid.Sized.Coord.Class.IsCoordListF' hands both halves back.
--
-- __The @INLINE@ is load-bearing and was measured.__ This is the first
-- version of @(':|')@ to carry a context at all, and a pattern synonym with a
-- required context compiles to a matcher that takes those dictionaries. Left
-- to itself GHC does not inline it, so every match allocates the pair
-- 'coordSplit' returns /and/ boxes the axis value inside it -- on
-- @tabulate 300x300@ with a rule that destructures its coordinate, 9.20 ms
-- and 36 MB against 1.13 ms and 4.8 MB with the pragma, i.e. 400 bytes a
-- cell for a match that should cost a 'quotRem'. The same body reached
-- through 'coordSplit' directly was 1.08 ms either way, which is how the
-- matcher rather than the arithmetic was identified: nothing about the
-- 'quotRem', 'Data.Grid.Sized.Ordinal.unsafeOrdinal'\'s guard or the 'Control.Lens.Iso' costs
-- anything measurable.
--
-- @tabulate 300x300  [rule destructures the coord]@ in @bench\/Main.hs@ is
-- there to keep it that way.
pattern (:|) ::
        (IsCoordLifted c, IsCoordList cs) => c -> Coord cs -> Coord (c ': cs)
pattern (:|) a as <- (coordSplit -> (a, as))
  where (:|) = appendCoord
{-# INLINE (:|) #-}

-- | The coordinate with no axes. Its position is zero, the only 'Int' in
-- @[0, 'MaxCoordSize' '[])@ = @[0, 1)@, so matching it is matching anything.
pattern EmptyCoord :: Coord '[]
pattern EmptyCoord <- Coord _
  where EmptyCoord = Coord 0
{-# INLINE EmptyCoord #-}

-- | Needed because GHC's coverage checker cannot see these view patterns are exhaustive.
{-# COMPLETE (:|) #-}

{-# COMPLETE EmptyCoord #-}

infixr 5 :|

instance (IsCoordList cs, All Show cs) => Show (Coord cs) where
    show c =
        "Coord [" ++
        intercalate
            ", "
            (hcollapse $
             hcliftA (Proxy :: Proxy Show) (\(I x) -> K $ show x) (unCoord c)) ++
        "]"

instance (IsCoordList cs, All ToJSON cs) => ToJSON (Coord cs) where
    toJSON c =
        Array $
        V.fromList $
        hcollapse $ hcmap (Proxy @ToJSON) (\(I x) -> K $ toJSON x) (unCoord c)

-- | The annotation on 'SOP.fromList' is load-bearing: 'Coord' no longer
-- mentions @cs@ in its field, so building one through 'npToPosition' leaves
-- nothing to tie the parsed @NP@'s list to the instance head.
instance forall cs. (IsCoordList cs, All FromJSON cs) => FromJSON (Coord cs) where
    parseJSON =
        withArray "Coord" $ \v ->
            case SOP.fromList (V.toList v) :: Maybe (NP (K Value) cs) of
                Just a ->
                    Coord . npToPosition <$>
                    hsequence
                        (hcmap (Proxy @FromJSON) (\(K x) -> parseJSON x) a)
                Nothing -> empty

-- | The three pointwise lifts.
--
-- Every instance below that is /the axes' instance, applied axis by axis/ has
-- the same body: take the coordinate apart with 'unCoord', apply the axis
-- operation under 'I', and put the position back together with
-- 'npToPosition'. Written out that was twelve copies of one of three shapes
-- across 'Semigroup', 'Monoid', 'AdditiveGroup', 'Group' and 'Bounded'.
--
-- The class is passed as a type argument -- @'pointwise2' \@Semigroup ('<>')@
-- -- and reaches @hcpure@\/@hcliftA@\/@hcliftA2@ as the 'Proxy' each of them
-- wants. All three are @INLINE@ and have no arguments GHC cannot see, so a
-- call site gets the same Core the hand-written body did.

-- | A coordinate built from a constant the axis class supplies: @mempty@,
-- @zeroV@, @minBound@, @maxBound@.
pointwise0 ::
     forall k cs. (IsCoordList cs, All k cs)
  => (forall c. k c => c)
  -> Coord cs
pointwise0 x = Coord $ npToPosition @cs $ hcpure (Proxy @k) (I x)
{-# INLINE pointwise0 #-}

-- | One coordinate in, one out: @negateV@, @invert@.
pointwise1 ::
     forall k cs. (IsCoordList cs, All k cs)
  => (forall c. k c => c -> c)
  -> Coord cs
  -> Coord cs
pointwise1 f a = Coord $ npToPosition $ hcliftA (Proxy @k) (fmap f) (unCoord a)
{-# INLINE pointwise1 #-}

-- | Two coordinates in, one out: @(<>)@, @(^+^)@, @(^-^)@.
pointwise2 ::
     forall k cs. (IsCoordList cs, All k cs)
  => (forall c. k c => c -> c -> c)
  -> Coord cs
  -> Coord cs
  -> Coord cs
pointwise2 f a b =
    Coord $ npToPosition $ hcliftA2 (Proxy @k) (liftA2 f) (unCoord a) (unCoord b)
{-# INLINE pointwise2 #-}

instance (IsCoordList cs, All Semigroup cs) => Semigroup (Coord cs) where
  (<>) = pointwise2 @Semigroup (<>)

instance forall cs. (IsCoordList cs, All Semigroup cs, All Monoid cs) =>
         Monoid (Coord cs) where
  mappend = (<>)
  mempty = pointwise0 @Monoid mempty

instance forall cs. (IsCoordList cs, All AdditiveGroup cs) =>
         AdditiveGroup (Coord cs) where
    zeroV = pointwise0 @AdditiveGroup zeroV
    (^+^) = pointwise2 @AdditiveGroup (^+^)
    negateV = pointwise1 @AdditiveGroup negateV
    (^-^) = pointwise2 @AdditiveGroup (^-^)

-- | The dense position is a perfect hash within a coordinate shape. Hashing
-- it directly avoids paying for each axis and preserves the shape in the type.
instance forall cs. IsCoordList cs => Hashable (Coord cs) where
    hashWithSalt salt = hashWithSalt salt . coordPosition

instance forall cs. (IsCoordList cs, All Bounded cs) => Bounded (Coord cs) where
    minBound = pointwise0 @Bounded minBound
    maxBound = pointwise0 @Bounded maxBound

class IxCoordList cs where
    ixRangeNP :: NP I cs -> NP I cs -> [NP I cs]
    ixIndexNP :: NP I cs -> NP I cs -> NP I cs -> Int
    ixInRangeNP :: NP I cs -> NP I cs -> NP I cs -> Bool
    ixRangeSizeNP :: NP I cs -> NP I cs -> Int

instance IxCoordList '[] where
    ixRangeNP Nil Nil = [Nil]
    ixIndexNP Nil Nil Nil = 0
    ixInRangeNP Nil Nil Nil = True
    ixRangeSizeNP Nil Nil = 1

instance (Ix c, IxCoordList cs) => IxCoordList (c ': cs) where
    ixRangeNP (I lower :* lowers) (I upper :* uppers) =
      [I value :* rest | value <- range (lower, upper),
                 rest <- ixRangeNP lowers uppers]
    ixIndexNP (I lower :* lowers) (I upper :* uppers) (I value :* values) =
      Ix.index (lower, upper) value * ixRangeSizeNP lowers uppers +
      ixIndexNP lowers uppers values
    ixInRangeNP (I lower :* lowers) (I upper :* uppers) (I value :* values) =
      inRange (lower, upper) value && ixInRangeNP lowers uppers values
    ixRangeSizeNP (I lower :* lowers) (I upper :* uppers) =
      rangeSize (lower, upper) * ixRangeSizeNP lowers uppers

-- | 'Ix' treats its pair of bounds as a bounding sub-box. The coordinate
-- instance therefore uses each axis's contiguous range and combines the
-- resulting offsets in row-major order. For 'Periodic', this range is
-- deliberately not cyclic: 'Ix' describes contiguous ordered sub-ranges.
instance forall cs. (IsCoordList cs, IxCoordList cs) => Ix (Coord cs) where
    range (Coord lower, Coord upper) =
      map (Coord . npToPosition @cs)
        (ixRangeNP @cs (unCoord @cs (Coord lower))
               (unCoord @cs (Coord upper)))
    index (Coord lower, Coord upper) (Coord value) =
      ixIndexNP @cs
        (unCoord @cs (Coord lower))
        (unCoord @cs (Coord upper))
        (unCoord @cs (Coord value))
    inRange (Coord lower, Coord upper) (Coord value) =
      ixInRangeNP @cs
        (unCoord @cs (Coord lower))
        (unCoord @cs (Coord upper))
        (unCoord @cs (Coord value))
    rangeSize (Coord lower, Coord upper) =
      ixRangeSizeNP @cs
        (unCoord @cs (Coord lower))
        (unCoord @cs (Coord upper))

instance forall cs. IsCoordList cs => Enum (Coord cs) where
    toEnum p =
        case coordFromPosition @cs p of
            Just c -> c
            Nothing ->
                error $
                "toEnum: " ++
                show p ++
                " is out of range for Coord " ++ show (coordListSize @cs)
    fromEnum = coordPosition
    succ a =
        let p = coordPosition a
        in if p >= coordListSize @cs - 1
             then error "Prelude.Enum.succ: tried to take succ of maxBound"
             else toEnum (p + 1)
    pred a =
        let p = coordPosition a
        in if p <= 0
             then error "Prelude.Enum.pred: tried to take pred of minBound"
             else toEnum (p - 1)
    enumFromTo a b = map Coord [coordPosition a .. coordPosition b]
    enumFromThenTo a b c =
        map Coord [coordPosition a, coordPosition b .. coordPosition c]
    enumFrom a = enumFromTo a (Coord (coordListSize @cs - 1))
    enumFromThen a b
      | coordPosition b >= coordPosition a =
        enumFromThenTo a b (Coord (coordListSize @cs - 1))
      | otherwise = enumFromThenTo a b (Coord 0)

-- | Per axis, not over the flat position: 'randomR' between two coordinates
-- gives the box they span, which a flat range would not.
instance forall cs. (IsCoordList cs, All Random cs) => Random (Coord cs) where
    random g =
        let (c, g') =
                runState
                    (hsequence $ hcpure (Proxy :: Proxy Random) (state random))
                    g
        in (Coord (npToPosition @cs c), g')
    randomR (mi, ma) g =
        let (c, g') =
                runState
                    (hsequence $
                     hcliftA2
                         (Proxy :: Proxy Random)
                         (\(I a) (I b) -> state (randomR (a, b)))
                         (unCoord mi)
                         (unCoord ma))
                    g
        in (Coord (npToPosition c), g')

-- | The 'Group' this 'Coord' has is exactly the one its axes have,
-- pointwise: 'invert' negates axis by axis, same as 'AdditiveGroup's
-- 'negateV' does but through 'Data.Group.Group' so consumers who reach for
-- @groups@ rather than @vector-space@ find it too. Not every axis type
-- qualifies -- 'Data.Grid.Sized.Coord.Clamped.Clamped' has no 'Group'
-- because clamping is not invertible -- so this instance is only as wide as
-- @All Group cs@ lets it be.
instance forall cs. (IsCoordList cs, All Semigroup cs, All Monoid cs, All Group cs) =>
         Group (Coord cs) where
    invert = pointwise1 @Group invert

-- | Pointwise again: a product of abelian groups is abelian, since swapping
-- the order of '<>' on the whole coordinate is swapping it independently on
-- each axis.
instance (IsCoordList cs, All Semigroup cs, All Monoid cs, All Group cs, All Abelian cs) =>
         Abelian (Coord cs)

singleCoord :: forall a. IsCoordLifted a => a -> Coord '[a]
singleCoord a = Coord (toAxisIndex a)

appendCoord ::
       forall a as. (IsCoordLifted a, IsCoordList as)
    => a
    -> Coord as
    -> Coord (a ': as)
appendCoord a (Coord as) = Coord (toAxisIndex a * coordListSize @as + as)

coordFromTuple :: (IsProductType t xs, IsCoordList xs) => t -> Coord xs
coordFromTuple = Coord . npToPosition . productTypeFrom

coordToTuple :: (IsProductType t xs, IsCoordList xs) => Coord xs -> t
coordToTuple = productTypeTo . unCoord
-- | A class, not a pair of @where@ helpers: a self-recursive fold cannot
-- unroll per axis, so the dictionary would be carried at run time instead of
-- resolved at compile time.
--
-- 'IsCoordList' is a superclass because the fold now works on a position and
-- so needs the axis strides, which is where they come from.
class (IsCoordList cs, All AffineSpace cs) => AffineCoordList cs where
    posAdd :: Int -> NP I (MapDiff cs) -> Int
    posSub :: Int -> Int -> NP I (MapDiff cs)

instance AffineCoordList '[] where
    posAdd p Nil = p
    posSub _ _ = Nil
    {-# INLINE posAdd #-}
    {-# INLINE posSub #-}

instance (IsCoordLifted x, AffineSpace x, AffineCoordList xs) =>
         AffineCoordList (x ': xs) where
    -- Match the displacement so MapDiff reduces before the recursive call.
    posAdd p (I d :* ds) =
        case p `quotRem` stride of
            (i, r) -> toAxisIndex (unsafeFromAxisIndex @x i .+^ d) * stride + posAdd @xs r ds
      where
        stride = coordListSize @xs
    posSub p q =
        case (p `quotRem` stride, q `quotRem` stride) of
            ((i, r), (j, s)) ->
                I (unsafeFromAxisIndex @x i .-. unsafeFromAxisIndex @x j) :* posSub @xs r s
      where
        stride = coordListSize @xs
    {-# INLINE posAdd #-}
    {-# INLINE posSub #-}

instance ( AffineCoordList cs
         , All AdditiveGroup (MapDiff cs)
         ) =>
         AffineSpace (Coord cs) where
    type Diff (Coord cs) = Delta (MapDiff cs)
    Coord a .-. Coord b = Delta (posSub @cs a b)
    Coord a .+^ Delta d = Coord (posAdd @cs a d)
-- | Every coordinate, in row-major order --- which after sized-grid-adr.16 is
-- just every position in range, so this is an 'Int' enumeration rather than a
-- cartesian product of per-axis values built through @hsequence@.
--
-- The order is unchanged, and load-bearing: 'Data.Grid.Sized.permuteGrid'
-- builds its table by zipping this against @[0 ..]@, so entry @k@ must be the
-- coordinate whose 'coordPosition' is @k@.
allCoord ::
       forall cs. (IsCoordList cs)
    => [Coord cs]
allCoord = map Coord [0 .. coordListSize @cs - 1]
{-# INLINE allCoord #-}

instance IsCoordList cs => U.Universe (Coord cs) where
  universe = allCoord

instance IsCoordList cs => U.Finite (Coord cs) where
  universeF = allCoord

instance (IsCoordList cs, AllSizedKnown cs) => Finitary (Coord cs) where
  type Cardinality (Coord cs) = MaxCoordSize cs
  toFinite = fromIntegral . coordPosition
  fromFinite = unsafeCoordFromPosition . fromIntegral

type family MaxCoordSize (cs :: [k]) :: GHC.Nat where
  MaxCoordSize '[] = 1
  MaxCoordSize (c n ': cs) = n GHC.* MaxCoordSize cs

-- | Row-major: the first axis is most significant, so a step along the last
-- axis moves one place in the vector.
--
-- The identity after sized-grid-adr.16, which is the point of it: this used to
-- be a fold over the axis list, and it is reached by 'Data.Grid.Sized.index',
-- 'Data.Grid.Sized.tabulate' and every stencil.
coordPosition :: Coord cs -> Int
coordPosition (Coord p) = p
{-# INLINE coordPosition #-}

-- | Each axis's index as a plain 'Int', first axis first: /where/ this cell
-- is, one number per axis.
--
-- This is the answer to \"which row and which column is this\", and it is
-- here rather than left to each caller because every obvious alternative is
-- either wrong or fragile (sized-grid-bzzy):
--
--   * @c '.-.' 'zeroCoord'@ is a 'Delta' --- a /displacement/, not a
--     position. On a 'Data.Grid.Sized.Coord.Periodic.Periodic' axis the
--     shortest signed route from the origin to cell 59 of 60 is @-1@, so the
--     two demos that reached for it drew half their board off-window
--     (sized-grid-23y3). @('.-.')@ answers \"how do I get there from here\";
--     this answers \"where is here\".
--   * @'coordPosition' c \`divMod\` side@ is right only while the literal
--     @side@ and the axis type agree, and nothing makes them.
--   * Matching @(':|')@ and unwrapping the boundary policy by hand is
--     correct, but it is a detour through the policy --- @Clamped@ to
--     'Data.Grid.Sized.Ordinal.Ordinal' to 'Int' --- to answer a question
--     about position, and it is three lines where this is one.
--
-- A list rather than a heterogeneous n-ary structure because every entry is
-- an 'Int': there is nothing a type could say about the @k@-th entry beyond
-- what @'axisCount' \@cs@ already says about the length. Where there are two
-- axes --- which is every consumer in this repository --- 'coordIndices2'
-- gives the same numbers as a pair.
coordIndices :: forall cs. IsCoordList cs => Coord cs -> [Int]
coordIndices (Coord p) = posIndices @cs p
{-# INLINE coordIndices #-}

-- | 'coordIndices' at two axes, as a pair: one 'quotRem' by the second axis's
-- size, with no list built and no length to case on.
--
-- First axis first, as everywhere else --- row-major, so on a grid drawn with
-- the first axis down the page this is @(row, column)@, and on one drawn with
-- it across the page it is @(x, y)@. The library does not name the axes; the
-- order is all it promises.
coordIndices2 ::
       forall a b. IsCoordList '[ a, b]
    => Coord '[ a, b]
    -> (Int, Int)
coordIndices2 (Coord p) = p `quotRem` coordListSize @'[ b]
{-# INLINE coordIndices2 #-}

-- | The product of axis sizes: the length of the vector inside a @'Grid' cs@.
-- Needs only 'IsCoordList', not @KnownNat@, so it works in the indexed
-- traversals too.
coordSpaceSize :: forall cs. IsCoordList cs => Int
coordSpaceSize = coordListSize @cs
{-# INLINE coordSpaceSize #-}

-- | How many axes @cs@ has: /d/ in @(2r+1)^d@, the exponent
-- 'Data.Grid.Sized.Stencil.mooreStencil' and
-- 'Data.Grid.Sized.Stencil.vonNeumannStencil' need to turn a radius into an
-- upper bound on a row's width.
axisCount :: forall cs. IsCoordList cs => Int
axisCount = coordListLength @cs
{-# INLINE axisCount #-}

-- | The inverse of 'coordPosition': a range check and nothing else.
coordFromPosition ::
       forall cs. IsCoordList cs
    => Int
    -> Maybe (Coord cs)
coordFromPosition p
    | p < 0 || p >= coordListSize @cs = Nothing
    | otherwise = Just (Coord p)
{-# INLINE coordFromPosition #-}

-- | 'coordFromPosition' without the range check.
--
-- __Precondition:__ @0 <= p < 'MaxCoordSize' cs@, /unchecked/. Breaking it
-- forges a coordinate outside its own space, which
-- 'Data.Grid.Sized.indexGrid' will then read through @unsafeIndex@ -- the
-- same class of hole 'Data.Grid.Sized.Unsafe.unsafeGridFromVector' opens, and
-- the reason both live behind that name.
--
-- It exists because after sized-grid-adr.16 a coordinate /is/ a position, so
-- a caller that already holds an in-range index -- one the vector it came
-- from supplies, say -- has nothing left to compute and no reason to pay a
-- bounds check it can already discharge. That is exactly the case in the
-- indexed traversals: @'Data.Vector.Generic.imap'@ hands over an index it
-- guarantees is below the vector's length, and a @'Data.Grid.Sized.GridOf' v
-- cs a@\'s length is @'coordSpaceSize' \@cs@ by construction.
--
-- Prefer 'coordFromPosition' anywhere the index came from outside.
unsafeCoordFromPosition :: Int -> Coord cs
unsafeCoordFromPosition = Coord
{-# INLINE unsafeCoordFromPosition #-}


transposeCoord ::
       (IsCoordLifted a, IsCoordLifted b) => Coord '[a, b] -> Coord '[b, a]
transposeCoord (a :| b :| EmptyCoord) = b :| a :| EmptyCoord

zeroCoord :: forall cs. IsCoordList cs => Coord cs
zeroCoord = pointwise0 @IsCoordLifted zeroPosition

-- | Evidence that every axis of @cs@, and every suffix of it, has a statically known size.
class GHC.KnownNat (MaxCoordSize cs) => AllSizedKnown (cs :: [Type]) where
  sizeProof :: SizeProof cs

-- | Matching this refines @cs@ to nil or cons and brings the tail's own instance into scope, which a bare @Data.Constraint.Dict@ could not do.
data SizeProof (cs :: [Type]) where
  SizeNil :: SizeProof '[]
  SizeCons ::
       forall c n cs. (GHC.KnownNat n, AllSizedKnown cs)
    => SizeProof (c n ': cs)

instance AllSizedKnown '[] where
    sizeProof = SizeNil

instance (GHC.KnownNat n, AllSizedKnown as) =>
         AllSizedKnown (c n ': as) where
    sizeProof = SizeCons
