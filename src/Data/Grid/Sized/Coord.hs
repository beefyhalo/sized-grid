{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Grid coordinates indexed by a type-level list of axes.
--
-- == The representation
--
-- A @'Coord' cs@ /is/ its row-major position: one 'Int' in
-- @[0, 'MaxCoordSize' cs)@ and nothing else (sized-grid-adr.16). The axis list
-- still indexes the type and still carries the boundary policy --
-- @Coord '[Clamped 5, Periodic 3]@ means exactly what it meant -- but there is
-- no longer a spine of boxes behind it, so 'coordPosition' is free and every
-- operation that used to build a coordinate only to collapse it to an 'Int'
-- now works on the 'Int' directly.
--
-- The invariant is that the position is in range, maintained by the
-- constructors: the same trade 'Data.Grid.Sized.Ordinal.Ordinal' made when it
-- stopped being a GADT, and the guard on every axis value is still paid on the
-- way in.
--
-- @(':|')@ and 'EmptyCoord' are still the interface, and still @COMPLETE@;
-- they carry the 'IsCoordList' evidence they need to divide and multiply by
-- the axis strides.
--
-- == Displacements live elsewhere
--
-- @'Diff' ('Coord' cs)@ is 'Delta' @('MapDiff' cs)@, not a @Coord@ any more: a
-- displacement is unbounded and signed and so cannot be a position. See
-- "Data.Grid.Sized.Coord.Delta", which this module re-exports.
module Data.Grid.Sized.Coord
  ( -- * Coordinates
    Coord
  , unCoord
  , pattern (:|)
  , pattern EmptyCoord
  , coordSplit
    -- * Displacements
  , Delta(..)
  , pattern (:^)
  , pattern NoDelta
  , deltaSplit
  , singleDelta
  , appendDelta
  , deltaFromTuple
  , deltaToTuple
    -- * Building and taking apart
  , singleCoord
  , appendCoord
  , coordFromTuple
  , coordToTuple
  , transposeCoord
  , zeroCoord
  , allCoord
  , coordPosition
  , coordFromPosition
  , unsafeCoordFromPosition
  , coordSpaceSize
    -- * Centred coordinates
  , CentredAxis
  , centreCoord
    -- * Punctured coordinates
  , PuncturedCoord
  , puncturedToCoord
  , allPunctured
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
    -- * Paths
  , Path(..)
  , walkPath
  , walkPathTotal
  , pathOffset
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
    -- * Frame transform
  , axisFrameFlips
  , transportCoord
  , TransportCoordList
    -- * Changing the size of a coord
  , WeakenCoord(..)
  , StrengthenCoord(..)
    -- * Type-level machinery
  , Length
  , MaxCoordSize
  , MapDiff
  , AffineCoordList
  , AllDiffSame
  , AllSizedKnown(..)
  , SizeProof(..)
  , IsCoordList
  ) where

import           Data.Grid.Sized.Coord.Class
import           Data.Grid.Sized.Coord.Delta
import           Data.Grid.Sized.Internal.Type (requiring)
import           Data.Grid.Sized.Ordinal

import           Control.Applicative   (empty)
import           Control.DeepSeq       (NFData (..))
import           Control.Monad         (foldM)
import           Control.Monad.State
import           Data.AdditiveGroup
import           Data.Aeson
import           Data.AffineSpace
import           Data.Constraint
import           Data.Finitary          (Finitary (..))
import           Data.Group            (Abelian, Group (..))
import           Data.Hashable         (Hashable (..))
import           Data.Ix               (Ix (..))
import qualified Data.Ix               as Ix
import           Data.Kind (Type)
import           Data.List             (intercalate, unfoldr)
import           Data.Universe.Class   (universe, universeF)
import qualified Data.Universe.Class   as U
import qualified Data.Vector           as V
import           Generics.SOP          hiding (Generic, S, Z)
import qualified Generics.SOP          as SOP
import           GHC.Generics          (Generic)
import           GHC.TypeLits
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
-- 'quotRem', 'unsafeOrdinal'\'s guard or the 'Control.Lens.Iso' costs
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

instance (IsCoordList cs, All Semigroup cs) => Semigroup (Coord cs) where
  a <> b =
      Coord $
      npToPosition $
      hcliftA2 (Proxy :: Proxy Semigroup) (liftA2 (<>)) (unCoord a) (unCoord b)

instance forall cs. (IsCoordList cs, All Semigroup cs, All Monoid cs) =>
         Monoid (Coord cs) where
  mappend = (<>)
  mempty = Coord $ npToPosition @cs $ hcpure (Proxy :: Proxy Monoid) (pure mempty)

instance forall cs. (IsCoordList cs, All AdditiveGroup cs) =>
         AdditiveGroup (Coord cs) where
    zeroV =
        Coord $ npToPosition @cs $ hcpure (Proxy :: Proxy AdditiveGroup) (pure zeroV)
    a ^+^ b =
        Coord $
        npToPosition $
        hcliftA2 (Proxy :: Proxy AdditiveGroup) (liftA2 (^+^)) (unCoord a) (unCoord b)
    negateV a =
        Coord $
        npToPosition $
        hcliftA (Proxy :: Proxy AdditiveGroup) (fmap negateV) (unCoord a)
    a ^-^ b =
        Coord $
        npToPosition $
        hcliftA2 (Proxy :: Proxy AdditiveGroup) (liftA2 (^-^)) (unCoord a) (unCoord b)

-- | The dense position is a perfect hash within a coordinate shape. Hashing
-- it directly avoids paying for each axis and preserves the shape in the type.
instance forall cs. IsCoordList cs => Hashable (Coord cs) where
    hashWithSalt salt = hashWithSalt salt . coordPosition

instance forall cs. (IsCoordList cs, All Bounded cs) => Bounded (Coord cs) where
    minBound =
        Coord $
        npToPosition @cs $
        hcpure (Proxy :: Proxy Bounded) (pure minBound)
    maxBound =
        Coord $
        npToPosition @cs $
        hcpure (Proxy :: Proxy Bounded) (pure maxBound)

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
    invert a =
        Coord $
        npToPosition $
        hcliftA (Proxy :: Proxy Group) (fmap invert) (unCoord a)

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

-- | A separate class from 'AffineCoordList': the fold needs both '.+^' and
-- 'Data.Grid.Sized.Coord.Class.axisFrameFlipsIsCoord' obligations at once,
-- which no single existing class states.
class (AffineCoordList cs, All IsCoordLifted cs) => TransportCoordList cs where
    posTransport ::
           AllDiffSame Int cs
        => Int
        -> NP I (MapDiff cs)
        -> (Int, NP I (MapDiff cs))

instance TransportCoordList '[] where
    posTransport p Nil = (p, Nil)
    {-# INLINE posTransport #-}

instance (AffineSpace x, IsCoordLifted x, TransportCoordList xs) =>
         TransportCoordList (x ': xs) where
    -- Match the displacement, as in 'posAdd', so MapDiff reduces first.
    posTransport p (I d :* ds) =
        case p `quotRem` stride of
            (i, r) ->
                case posTransport @xs r ds of
                    (r', ds') ->
                        ( toAxisIndex (x .+^ d) * stride + r'
                        , I (if axisFrameFlipsIsCoord x d
                                 then negate d
                                 else d) :*
                          ds')
              where
                x = unsafeFromAxisIndex @x i
      where
        stride = coordListSize @xs
    {-# INLINE posTransport #-}

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

-- | The product of axis sizes: the length of the vector inside a @'Grid' cs@.
-- Needs only 'IsCoordList', not @KnownNat@, so it works in the indexed
-- traversals too.
coordSpaceSize :: forall cs. IsCoordList cs => Int
coordSpaceSize = coordListSize @cs
{-# INLINE coordSpaceSize #-}

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

-- | The checked counterpart of '.+^': succeeds only if every axis's own
-- boundary policy allows the step, so a torus axis can wrap while a bounded
-- axis in the same coord refuses.
offsetCoord ::
       forall cs. ( IsCoordList cs
       , AllDiffSame Int cs
       )
    => Coord cs
    -> Diff (Coord cs)
    -> Maybe (Coord cs)
offsetCoord (Coord p) (Delta d) = Coord <$> posOffset @cs p d

-- | Where a walk left the grid: the last coordinate still on it, and how many whole steps it took to get there.
data OffGrid cs = OffGrid
    { lastInside :: Coord cs
    , stepsTaken :: Int
    } deriving (Generic)

deriving instance Eq (OffGrid cs)

deriving instance (IsCoordList cs, All Show cs) => Show (OffGrid cs)

-- | Take up to @n@ steps of @d@ from @c@: 'Right' the coordinate @n@ steps away, or 'Left' how far the walk got before the grid ran out.
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

-- | The ray from @c@ in direction @d@, not including @c@ itself; infinite on a torus or with a zero displacement.
coordRay ::
       ( IsCoordList cs
       , AllDiffSame Int cs
       )
    => Coord cs
    -> Diff (Coord cs)
    -> [Coord cs]
coordRay c d = unfoldr (\x -> (\y -> (y, y)) <$> offsetCoord x d) c

-- | An ordered sequence of displacements, kept separate rather than summed into one 'Diff': only matters where a boundary policy is not separable per axis.
newtype Path cs = Path
    { pathSteps :: [Diff (Coord cs)]
    }

deriving instance Eq (Diff (Coord cs)) => Eq (Path cs)

deriving instance Show (Diff (Coord cs)) => Show (Path cs)

instance Semigroup (Path cs) where
    Path a <> Path b = Path (a <> b)

instance Monoid (Path cs) where
    mempty = Path []

-- | Walk a 'Path' one step at a time through 'offsetCoord', stopping with 'Nothing' as soon as a step would leave the grid, so a route can fail even when its steps cancel out net.
walkPath ::
       ( IsCoordList cs
       , AllDiffSame Int cs
       )
    => Coord cs
    -> Path cs
    -> Maybe (Coord cs)
walkPath c (Path ds) = foldM offsetCoord c ds

-- | The single displacement a 'Path'\'s steps sum to, forgetting their order.
pathOffset :: All AdditiveGroup (MapDiff cs) => Path cs -> Diff (Coord cs)
pathOffset (Path ds) = foldl' (^+^) zeroV ds

-- | The total counterpart of 'walkPath': on a coord where every axis is
-- 'Boundaryless', a step can never leave the grid, so there is no 'Maybe'
-- for the caller to discharge. Agrees with 'walkPath' wherever both
-- typecheck: @walkPath c p == Just (walkPathTotal c p)@.
walkPathTotal ::
       forall cs.
       ( All Boundaryless cs
       , AffineCoordList cs
       , All AdditiveGroup (MapDiff cs)
       )
    => Coord cs
    -> Path cs
    -> Coord cs
walkPathTotal c p = requiring @(All Boundaryless cs) $ c .+^ pathOffset p

-- | Every coordinate within @r@ steps on each axis, paired with its total step count; the centre is the only entry with total zero.
stepsWithin ::
       forall cs. IsCoordList cs
    => Int
    -> Coord cs
    -> [(Int, Coord cs)]
stepsWithin r (Coord p) = fmap Coord <$> posStepsWithin @cs r p
{-# INLINE stepsWithin #-}

-- | The Moore neighbourhood: every coordinate within @r@ steps on each axis independently, excluding the centre.
--
-- Reads 'posStepsWithin' directly rather than going through 'stepsWithin'.
-- The two differ by one intermediate list -- 'stepsWithin' has to rebuild
-- every @(steps, position)@ pair as a @(steps, 'Coord')@ one to honour its own
-- type, and this then drops the steps again. Worth 995 us \/ 8.0 MB to
-- 648 us \/ 2.6 MB on the 50x50 neighbour sweep -- the difference between
-- half of the allocation win sized-grid-adr.8 measured the ceiling at and all
-- of it (adr.8: 16.4 MB to 2.6 MB, 2.6x; this reaches 2.58x).
mooreNeighbours :: forall cs. IsCoordList cs => Int -> Coord cs -> [Coord cs]
mooreNeighbours r (Coord p) =
    [Coord n | (s, n) <- posStepsWithin @cs r p, s > 0]
{-# INLINE mooreNeighbours #-}

-- | The von Neumann neighbourhood: coordinates whose per-axis distances sum to at most @r@, excluding the centre.
vonNeumannNeighbours :: forall cs. IsCoordList cs => Int -> Coord cs -> [Coord cs]
vonNeumannNeighbours r (Coord p) =
    [Coord n | (s, n) <- posStepsWithin @cs r p, s > 0, s <= r]
{-# INLINE vonNeumannNeighbours #-}

-- | 'mooreNeighbours' at radius one: the surrounding cells, diagonals included.
neighbours :: IsCoordList cs => Coord cs -> [Coord cs]
neighbours = mooreNeighbours 1
{-# INLINE neighbours #-}

-- | The number of steps between two values on a single axis, by the shorter route if the axis offers more than one.
axisDistance :: forall x. IsCoordLifted x => x -> x -> Int
axisDistance = axisDistanceIsCoord @(CoordContainer x) @(CoordNat x)

-- | The per-axis distances between two coords, first axis first.
axisDistances :: forall cs. IsCoordList cs => Coord cs -> Coord cs -> [Int]
axisDistances (Coord a) (Coord b) = posDistances @cs a b

-- | The Chebyshev distance: the largest per-axis distance. Folded by the
-- 'posMaxDistance' method rather than over 'axisDistances', so the @['Int']@
-- that was built only to be consumed immediately is gone (measured: 135 MB to
-- 60 MB over 360,000 calls).
coordDistance :: forall cs. IsCoordList cs => Coord cs -> Coord cs -> Int
coordDistance (Coord a) (Coord b) = posMaxDistance @cs a b

-- | The Manhattan distance: the per-axis distances summed, likewise without
-- the intermediate list.
coordManhattan :: forall cs. IsCoordList cs => Coord cs -> Coord cs -> Int
coordManhattan (Coord a) (Coord b) = posSumDistance @cs a b

-- | Which end of its axis a single coordinate sits at, or 'Nothing' if interior.
axisBoundary :: forall x. IsCoordLifted x => x -> Maybe Extremum
axisBoundary = axisBoundaryIsCoord @(CoordContainer x) @(CoordNat x)

-- | Whether stepping this axis by this displacement reverses its own sense of direction. 'False' for every axis type except 'Data.Grid.Sized.Coord.Reflective.Reflective' and 'Data.Grid.Sized.Coord.Reflect101.Reflect101'.
axisFrameFlips :: forall x. IsCoordLifted x => x -> Int -> Bool
axisFrameFlips = axisFrameFlipsIsCoord @(CoordContainer x) @(CoordNat x)

-- | Move a coordinate by a heading, and report the heading a walker facing it would have after the step.
transportCoord ::
       forall cs. (TransportCoordList cs, AllDiffSame Int cs)
    => Coord cs
    -> Diff (Coord cs)
    -> (Coord cs, Diff (Coord cs))
transportCoord (Coord c) (Delta d) =
    case posTransport @cs c d of
        (c', d') -> (Coord c', Delta d')

-- | Where each axis of a coord sits relative to its own ends, first axis first.
axisBoundaries ::
       forall cs. IsCoordList cs
    => Coord cs
    -> [Maybe Extremum]
axisBoundaries (Coord p) = posBoundaries @cs p

-- | Whether any axis is at one of its ends. 'False' on a coord with no axes.
onBoundary :: forall cs. IsCoordList cs => Coord cs -> Bool
onBoundary (Coord p) = posAnyBoundary @cs p

-- | Whether every axis is at one of its ends. 'False' on any coord with a torus axis, and 'False' rather than a vacuous 'True' on the empty coord.
--
-- The 'SList' match is what keeps the empty coord 'False': 'posAllBoundary' is
-- a fold and so vacuously 'True' there. It replaces a match on the coord's own
-- 'Nil', which there is no longer a spine to perform --- but the emptiness of
-- @cs@ is a property of the type, so 'SList' answers it without one.
isCorner :: forall cs. IsCoordList cs => Coord cs -> Bool
isCorner (Coord p) =
  case sList :: SList cs of
    SNil  -> False
    SCons -> posAllBoundary @cs p

-- | Every coordinate that is not 'onBoundary', in 'allCoord' order.
interiorCoords :: IsCoordList cs => [Coord cs]
interiorCoords = filter (not . onBoundary) allCoord

transposeCoord ::
       (IsCoordLifted a, IsCoordLifted b) => Coord '[a, b] -> Coord '[b, a]
transposeCoord (a :| b :| EmptyCoord) = b :| a :| EmptyCoord

zeroCoord :: forall cs. IsCoordList cs => Coord cs
zeroCoord =
    Coord $
    npToPosition @cs $ hcpure (Proxy :: Proxy IsCoordLifted) (I zeroPosition)

-- | An axis contributing to 'centreCoord': lifted and odd-sized, via 'Data.Grid.Sized.Coord.Class.OddC'.
class (IsCoordLifted x, OddC x) => CentredAxis x

instance (IsCoordLifted x, OddC x) => CentredAxis x

-- | The middle value of a single axis: index @(n - 1) \`div\` 2@, equidistant from both ends when @n@ is odd.
centreAxis :: forall x. IsCoordLifted x => x
centreAxis = unsafeFromAxisIndex @x ((ordinalSize @(CoordNat x) - 1) `div` 2)

-- | The coordinate sitting at the middle of every axis at once.
centreCoord :: forall cs. (IsCoordList cs, All CentredAxis cs) => Coord cs
centreCoord =
    Coord $ npToPosition @cs $ hcpure (Proxy :: Proxy CentredAxis) (I centreAxis)

-- | A coordinate other than the centre of a window: a flat position, not axis-by-axis, so recovering which neighbour it is goes through 'puncturedToCoord'.
newtype PuncturedCoord cs =
    PuncturedCoord (Ordinal (MaxCoordSize cs - 1))
    deriving (Eq, Ord)

-- | Not @deriving Show@: 'Ordinal'\'s 'Show' instance needs a @KnownNat@ this module deliberately does not require of every caller.
instance Show (PuncturedCoord cs) where
    show (PuncturedCoord o) = "PuncturedCoord " ++ show (ordinalToInt o)

-- | The 'Coord' a 'PuncturedCoord' names. Total: skips over 'centreCoord'\'s own position so every 'PuncturedCoord' names a real coordinate other than the centre.
puncturedToCoord ::
       forall cs. (IsCoordList cs, All CentredAxis cs)
    => PuncturedCoord cs
    -> Coord cs
puncturedToCoord (PuncturedCoord o) =
    case coordFromPosition flat of
        Just c -> c
        Nothing ->
            error
                "Data.Grid.Sized.Coord.puncturedToCoord: impossible: a \
                \PuncturedCoord's flat index landed outside Coord's range"
  where
    k = coordPosition (centreCoord @cs)
    i = ordinalToInt o
    flat
        | i < k = i
        | otherwise = i + 1

-- | Every axis contributes at least one value, so the coordinate space is never empty. Not exported: nothing outside this module constructs a 'PuncturedCoord'.
coordSpaceNonEmpty :: forall cs. AllSizedKnown cs => Dict (1 <= MaxCoordSize cs)
coordSpaceNonEmpty =
    case cmpNat (Proxy @1) (Proxy @(MaxCoordSize cs)) of
        LTI -> Dict
        EQI -> Dict
        GTI ->
            error
                "Data.Grid.Sized.Coord.coordSpaceNonEmpty: impossible: \
                \MaxCoordSize came out below one, though every axis \
                \contributes at least one value"

-- | Every 'PuncturedCoord', in the same row-major order 'allCoord' visits, with the centre left out.
allPunctured ::
       forall cs. (IsCoordList cs, AllSizedKnown cs)
    => [PuncturedCoord cs]
allPunctured =
    case coordSpaceNonEmpty @cs of
        Dict ->
            [PuncturedCoord (unsafeOrdinal i) | i <- [0 .. coordSpaceSize @cs - 2]]

-- | Evidence that every axis of @cs@, and every suffix of it, has a statically known size.
class GHC.KnownNat (MaxCoordSize cs) => AllSizedKnown (cs :: [Type]) where
  sizeProof :: SizeProof cs

-- | Matching this refines @cs@ to nil or cons and brings the tail's own instance into scope, which a bare 'Dict' could not do.
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

class WeakenCoord as bs where
  weakenCoord :: Coord as -> Maybe (Coord bs)

instance WeakenCoord '[] '[] where
  weakenCoord = Just

instance ( WeakenCoord as bs
         , IsCoordLifted (c n)
         , IsCoordLifted (c m)
         , IsCoordList as
         , IsCoordList bs
         ) =>
         WeakenCoord (c n ': as) (c m ': bs) where
    weakenCoord (a :| as) = do
        bs <- weakenCoord as
        b <- weakenIsCoord a
        return (b :| bs)

class StrengthenCoord as bs where
  strengthenCoord :: Coord as -> Coord bs

instance StrengthenCoord '[] '[] where
  strengthenCoord c = c

instance ( StrengthenCoord as bs
         , IsCoordLifted (c n)
         , IsCoordLifted (c m)
         , IsCoordList as
         , IsCoordList bs
         , n <= m
         ) =>
         StrengthenCoord (c n ': as) (c m ': bs) where
  strengthenCoord (a :| as) = strengthenIsCoord a :| strengthenCoord as
