{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Grid coordinates indexed by a type-level list of axes.
module Data.Grid.Sized.Coord
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
import           Data.Grid.Sized.Ordinal

import           Control.Applicative   (empty)
import           Control.Lens          hiding (from, to)
import           Control.Monad         (foldM)
import           Control.Monad.State
import           Data.AdditiveGroup
import           Data.Aeson
import           Data.AffineSpace
import           Data.Constraint
import           Data.Kind (Type)
import           Data.List             (intercalate, unfoldr)
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

newtype Coord cs = Coord {unCoord :: NP I cs}
  deriving (Generic)

coordSplit:: Coord (c ': cs) -> (c, Coord cs)
coordSplit (Coord (I x :* xs)) = (x, Coord xs)

pattern (:|) :: c -> Coord cs -> Coord (c ': cs)
pattern (:|) a as <- (coordSplit -> (a,as))
  where (:|) a (Coord as) = Coord (I a :* as)

pattern EmptyCoord :: Coord '[]
pattern EmptyCoord = Coord Nil

-- | Needed because GHC's coverage checker cannot see these view patterns are exhaustive.
{-# COMPLETE (:|) #-}

{-# COMPLETE EmptyCoord #-}

infixr 5 :|

_WrappedCoord :: Iso' (Coord cs) (NP I cs)
_WrappedCoord = dimap unCoord (fmap Coord)

instance All Eq cs => Eq (Coord cs) where
    Coord a == Coord b =
        and $
        hcollapse $ hcliftA2 (Proxy :: Proxy Eq) (\(I x) (I y) -> K (x == y)) a b

-- | @All Eq cs@ does not follow from @All Ord cs@: superclass evidence must be resolved at instance-declaration time, so both constraints are required.
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

coordHead :: Lens (Coord (a ': as)) (Coord (a' ': as)) a a'
coordHead f (Coord (I a :* as)) = (\a' -> Coord (I a' :* as)) <$> f a

coordTail :: Lens (Coord (a ': as)) (Coord (a ': as')) (Coord as) (Coord as')
coordTail f (Coord (a :* as)) = (\(Coord as') -> Coord (a :* as')) <$> f (Coord as)

singleCoord :: a -> Coord '[a]
singleCoord a = Coord (I a :* Nil)

appendCoord :: a -> Coord as -> Coord (a ': as)
appendCoord a (Coord as) = Coord (I a :* as)

coordFromTuple :: IsProductType t xs => t -> Coord xs
coordFromTuple = Coord . productTypeFrom

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

-- | A class, not a pair of @where@ helpers: a self-recursive fold cannot unroll per axis, so the dictionary would be carried at run time instead of resolved at compile time.
class All AffineSpace cs => AffineCoordList cs where
    npAdd :: NP I cs -> NP I (MapDiff cs) -> NP I cs
    npSub :: NP I cs -> NP I cs -> NP I (MapDiff cs)

instance AffineCoordList '[] where
    npAdd Nil Nil = Nil
    npSub Nil Nil = Nil
    {-# INLINE npAdd #-}
    {-# INLINE npSub #-}

instance (AffineSpace x, AffineCoordList xs) => AffineCoordList (x ': xs) where
    -- Match the coord first so MapDiff reduces before the second pattern is checked.
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

-- | A separate class from 'AffineCoordList': the fold needs both '.+^' and 'Data.Grid.Sized.Coord.Class.axisFrameFlipsIsCoord' obligations at once, which no single existing class states.
class (AffineCoordList cs, All IsCoordLifted cs) => TransportCoordList cs where
    npTransport ::
           AllDiffSame Int cs
        => NP I cs
        -> NP I (MapDiff cs)
        -> (NP I cs, NP I (MapDiff cs))

instance TransportCoordList '[] where
    npTransport Nil Nil = (Nil, Nil)
    {-# INLINE npTransport #-}

instance (AffineSpace x, IsCoordLifted x, TransportCoordList xs) =>
         TransportCoordList (x ': xs) where
    -- Match the coord first, as in 'npAdd', so MapDiff reduces before the second pattern is checked.
    npTransport (I x :* xs) (I d :* ds) =
        (I (x .+^ d) :* ys, I d' :* ds')
      where
        (ys, ds') = npTransport xs ds
        d' = if axisFrameFlipsIsCoord x d then negate d else d
    {-# INLINE npTransport #-}

allCoord ::
       forall cs. (IsCoordList cs)
    => [Coord cs]
allCoord =
    Coord <$>
    hsequence
        (hcpure (Proxy :: Proxy IsCoordLifted) allCoordLike)

type family MaxCoordSize (cs :: [k]) :: GHC.Nat where
  MaxCoordSize '[] = 1
  MaxCoordSize (c n ': cs) = n GHC.* MaxCoordSize cs

-- | Row-major: the first axis is most significant, so a step along the last axis moves one place in the vector.
coordPosition :: forall cs. IsCoordList cs => Coord cs -> Int
coordPosition (Coord a) = snd (sizeAndPosition a)
{-# INLINE coordPosition #-}

-- | The product of axis sizes: the length of the vector inside a @'Grid' cs@. Needs only 'IsCoordList', not @KnownNat@, so it works in the indexed traversals too.
coordSpaceSize :: forall cs. IsCoordList cs => Int
coordSpaceSize = coordListSize @cs
{-# INLINE coordSpaceSize #-}

-- | The inverse of 'coordPosition'.
coordFromPosition ::
       forall cs. IsCoordList cs
    => Int
    -> Maybe (Coord cs)
coordFromPosition p
    | p < 0 = Nothing
    | otherwise =
        case coordDigits p of
            -- A nonzero leftover means p was at least coordSpaceSize, so no separate bounds check is needed.
            (np, 0) -> Just $ Coord np
            _       -> Nothing

-- | Least-significant axis first: the tail is decoded before the head so no stride needs to be known in advance.
coordDigits ::
       forall xs. IsCoordList xs
    => Int
    -> (NP I xs, Int)
coordDigits p =
  case sList :: SList xs of
    SNil -> (Nil, p)
    -- unsafeOrdinal is safe here: quotRem by a positive divisor with a non-negative numerator gives 0 <= r < size.
    SCons @ys @y ->
      case coordDigits @ys p of
        (rest, q) ->
          case q `quotRem` ordinalSize @(CoordNat y) of
            (q', r) ->
              (I (review asOrdinal (unsafeOrdinal r)) :* rest, q')

-- | The checked counterpart of '.+^': succeeds only if every axis's own boundary policy allows the step, so a torus axis can wrap while a bounded axis in the same coord refuses.
offsetCoord ::
       ( IsCoordList cs
       , AllDiffSame Int cs
       )
    => Coord cs
    -> Diff (Coord cs)
    -> Maybe (Coord cs)
offsetCoord (Coord cs) (Coord d) = Coord <$> npOffset cs d

-- | Where a walk left the grid: the last coordinate still on it, and how many whole steps it took to get there.
data OffGrid cs = OffGrid
    { lastInside :: Coord cs
    , stepsTaken :: Int
    } deriving (Generic)

deriving instance All Eq cs => Eq (OffGrid cs)

deriving instance All Show cs => Show (OffGrid cs)

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

-- | Every coordinate within @r@ steps on each axis, paired with its total step count; the centre is the only entry with total zero.
stepsWithin ::
       forall cs. IsCoordList cs
    => Int
    -> Coord cs
    -> [(Int, Coord cs)]
stepsWithin r (Coord cs) = fmap Coord <$> npStepsWithin r cs

-- | The Moore neighbourhood: every coordinate within @r@ steps on each axis independently, excluding the centre.
mooreNeighbours :: IsCoordList cs => Int -> Coord cs -> [Coord cs]
mooreNeighbours r c = [n | (s, n) <- stepsWithin r c, s > 0]

-- | The von Neumann neighbourhood: coordinates whose per-axis distances sum to at most @r@, excluding the centre.
vonNeumannNeighbours :: IsCoordList cs => Int -> Coord cs -> [Coord cs]
vonNeumannNeighbours r c = [n | (s, n) <- stepsWithin r c, s > 0, s <= r]

-- | 'mooreNeighbours' at radius one: the surrounding cells, diagonals included.
neighbours :: IsCoordList cs => Coord cs -> [Coord cs]
neighbours = mooreNeighbours 1

-- | The number of steps between two values on a single axis, by the shorter route if the axis offers more than one.
axisDistance :: forall x. IsCoordLifted x => x -> x -> Int
axisDistance = axisDistanceIsCoord @(CoordContainer x) @(CoordNat x)

-- | The per-axis distances between two coords, first axis first.
axisDistances :: forall cs. IsCoordList cs => Coord cs -> Coord cs -> [Int]
axisDistances (Coord as) (Coord bs) = npDistances as bs

-- | The Chebyshev distance: the largest per-axis distance. Folded by the
-- 'npMaxDistance' method rather than over 'axisDistances', so the @['Int']@
-- that was built only to be consumed immediately is gone (measured: 135 MB to
-- 60 MB over 360,000 calls).
coordDistance :: IsCoordList cs => Coord cs -> Coord cs -> Int
coordDistance (Coord as) (Coord bs) = npMaxDistance as bs

-- | The Manhattan distance: the per-axis distances summed, likewise without
-- the intermediate list.
coordManhattan :: IsCoordList cs => Coord cs -> Coord cs -> Int
coordManhattan (Coord as) (Coord bs) = npSumDistance as bs

-- | Which end of its axis a single coordinate sits at, or 'Nothing' if interior.
axisBoundary :: forall x. IsCoordLifted x => x -> Maybe Extremum
axisBoundary = axisBoundaryIsCoord @(CoordContainer x) @(CoordNat x)

-- | Whether stepping this axis by this displacement reverses its own sense of direction. 'False' for every axis type except 'Data.Grid.Sized.Coord.Reflective.Reflective' and 'Data.Grid.Sized.Coord.Reflect101.Reflect101'.
axisFrameFlips :: forall x. IsCoordLifted x => x -> Int -> Bool
axisFrameFlips = axisFrameFlipsIsCoord @(CoordContainer x) @(CoordNat x)

-- | Move a coordinate by a heading, and report the heading a walker facing it would have after the step.
transportCoord ::
       (TransportCoordList cs, AllDiffSame Int cs)
    => Coord cs
    -> Diff (Coord cs)
    -> (Coord cs, Diff (Coord cs))
transportCoord (Coord c) (Coord d) =
    case npTransport c d of
        (c', d') -> (Coord c', Coord d')

-- | Where each axis of a coord sits relative to its own ends, first axis first.
axisBoundaries ::
       forall cs. IsCoordList cs
    => Coord cs
    -> [Maybe Extremum]
axisBoundaries (Coord cs) = npBoundaries cs

-- | Whether any axis is at one of its ends. 'False' on a coord with no axes.
onBoundary :: IsCoordList cs => Coord cs -> Bool
onBoundary (Coord cs) = npAnyBoundary cs

-- | Whether every axis is at one of its ends. 'False' on any coord with a torus axis, and 'False' rather than a vacuous 'True' on the empty coord.
--
-- The match on 'Nil' is what keeps the empty coord 'False': 'npAllBoundary' is
-- a fold and so vacuously 'True' there, and there is no longer a list whose
-- emptiness could be tested instead.
isCorner :: IsCoordList cs => Coord cs -> Bool
isCorner (Coord cs) =
  case cs of
    Nil -> False
    _   -> npAllBoundary cs

-- | Every coordinate that is not 'onBoundary', in 'allCoord' order.
interiorCoords :: IsCoordList cs => [Coord cs]
interiorCoords = filter (not . onBoundary) allCoord

tranposeCoord :: Coord '[a,b] -> Coord '[b,a]
tranposeCoord (Coord (a :* b :* Nil)) = Coord (b :* a :* Nil)

zeroCoord :: IsCoordList cs => Coord cs
zeroCoord = Coord $ hcpure (Proxy :: Proxy IsCoordLifted) (I zeroPosition)

-- | An axis contributing to 'centreCoord': lifted and odd-sized, via 'Data.Grid.Sized.Coord.Class.OddC'.
class (IsCoordLifted x, OddC x) => CentredAxis x

instance (IsCoordLifted x, OddC x) => CentredAxis x

-- | The middle value of a single axis: index @(n - 1) \`div\` 2@, equidistant from both ends when @n@ is odd.
centreAxis :: forall x. IsCoordLifted x => x
centreAxis =
    review asOrdinal $
    unsafeOrdinal @(CoordNat x) ((ordinalSize @(CoordNat x) - 1) `div` 2)

-- | The coordinate sitting at the middle of every axis at once.
centreCoord :: forall cs. All CentredAxis cs => Coord cs
centreCoord = Coord $ hcpure (Proxy :: Proxy CentredAxis) (I centreAxis)

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

instance (WeakenCoord as bs, IsCoord c, KnownNat m) =>
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
         , IsCoord c
         , n <= m
         , KnownNat m
         ) =>
         StrengthenCoord (c n ': as) (c m ': bs) where
  strengthenCoord (a :| as) = strengthenIsCoord a :| strengthenCoord as
