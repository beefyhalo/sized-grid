-- | sized-grid-adr.8, the other end of the design space: a coordinate that
-- /is/ its row-major position.
--
-- Not a port of grid-sized and not intended to become one. Just enough of a
-- grid to run the existing benchmark bodies against, so the ceiling the
-- representation change is worth can be measured rather than argued.
--
-- Deliberate correspondences with the real library, so the comparison is
-- like-for-like:
--
--   * The axis operations are the same arithmetic as 'Clamped' and
--     'Periodic'\'s @IsCoord@\/@AffineSpace@ instances, clamp-by-comparison
--     and reduce-before-adding included.
--   * The fold over the axis list is a class method, not a self-recursive
--     function, for the reason recorded on @IsCoordList@: a self-recursive
--     fold cannot unroll.
--   * 'axisStepsI' is 'Data.Grid.Sized.Coord.Class.axisSteps' transliterated,
--     dedup filter and all.
--
-- The one structural difference is the whole point: a @Coord@ is an @Int@,
-- so a per-axis operation must divide its way in and multiply its way back
-- out, and 'coordPosI' is @coerce@.
module IntCoord
    ( -- * Axes
      Pol(..)
    , Ax(..)
    , C
    , P
    , KnownAxis(..)
    , axisStepsI
      -- * Coordinates
    , Coord(..)
    , Diffs(..)
    , Shape(..)
    , coordSplitI
    , allCoordI
    , zeroCoordI
    , addI
    , offsetI
    , onBoundaryI
    , coordDistanceI
    , neighboursI
      -- * Grids
    , IGrid(..)
    , tabulateI
    , indexI
    , imapI
    , ifoldlI'
    , transposeI
    ) where

import           Control.DeepSeq (NFData (..))
import           Data.Proxy      (Proxy (..))
import qualified Data.Vector     as V
import           GHC.Natural     (naturalToWord)
import           GHC.TypeLits    (KnownNat, Nat)
import qualified GHC.TypeNats    as TN

-- | The boundary policy, as a promoted constructor rather than a type of
-- kind @Nat -> Type@: this representation has no per-axis /value/ to attach
-- a class to, only a slot in an @Int@.
data Pol
    = Cl
    | Pe

-- | One axis: a policy and a size.
data Ax = Ax Pol Nat

type C n = 'Ax 'Cl n

type P n = 'Ax 'Pe n

-- | @ordinalSize@'s route to the size: 'naturalToWord', not the 'Integer'
-- one, for the reason recorded on it (sized-grid-adr.14).
natI :: forall n. KnownNat n => Int
natI = fromIntegral (naturalToWord (TN.natVal (Proxy @n)))
{-# INLINE natI #-}

-- | 'Data.Grid.Sized.Ordinal.unsafeOrdinal'\'s guard, transliterated: every
-- axis value this representation constructs is range-checked, exactly as
-- every 'Ordinal' the real library constructs is (sized-grid-adr.14). The
-- failure branch is a nullary CAF, which that issue measured to be
-- load-bearing --- giving it free variables so the message could name the
-- offending index put 'offsetCoord' back from 35 MB to 73 MB a sweep.
--
-- Without this the spike would be measuring a cheaper invariant as well as a
-- different representation. It costs ~1% here; see the note in the issue.
inRange :: Int -> Int -> Int
inRange n i
    | i < 0 || i >= n = outOfRange
    | otherwise = i
{-# INLINE inRange #-}

outOfRange :: Int
outOfRange = error "IntCoord: axis index out of range"
{-# NOINLINE outOfRange #-}

-- | The per-axis operations, the counterpart of @IsCoord@ --- but taking and
-- returning the axis's index as a bare 'Int', since that is all this
-- representation has.
class KnownAxis (a :: Ax) where
    axisSize :: Int
    -- | The total offset: @('Data.AffineSpace..+^')@ on one axis.
    axisAdd :: Int -> Int -> Int
    -- | The checked offset: @offsetIsCoord@ on one axis.
    axisOffset :: Int -> Int -> Maybe Int
    -- | Whether this index is at either end.
    axisAtEdge :: Int -> Bool
    -- | Distance along this axis, the short way round where there is one.
    axisDist :: Int -> Int -> Int

instance KnownNat n => KnownAxis ('Ax 'Cl n) where
    axisSize = natI @n
    -- Clamped by comparison, not addition-then-clamp, exactly as
    -- 'Clamped'\'s AffineSpace instance.
    axisAdd i d
        | d > hi - i = hi
        | d < negate i = 0
        | otherwise = inRange (natI @n) (i + d)
      where
        hi = natI @n - 1
    axisOffset i d
        | d > hi - i = Nothing
        | d < negate i = Nothing
        | otherwise = Just (inRange (natI @n) (i + d))
      where
        hi = natI @n - 1
    axisAtEdge i = i == 0 || i == natI @n - 1
    axisDist i j = abs (i - j)
    {-# INLINE axisSize #-}
    {-# INLINE axisAdd #-}
    {-# INLINE axisOffset #-}
    {-# INLINE axisAtEdge #-}
    {-# INLINE axisDist #-}

instance KnownNat n => KnownAxis ('Ax 'Pe n) where
    axisSize = natI @n
    -- Displacement reduced before adding, as in 'Periodic'.
    axisAdd i d = inRange n ((i + d `mod` n) `mod` n)
      where
        n = natI @n
    axisOffset i d = Just (axisAdd @('Ax 'Pe n) i d)
    axisAtEdge _ = False
    axisDist i j = min d (n - d)
      where
        n = natI @n
        d = abs (i - j)
    {-# INLINE axisSize #-}
    {-# INLINE axisAdd #-}
    {-# INLINE axisOffset #-}
    {-# INLINE axisAtEdge #-}
    {-# INLINE axisDist #-}

-- | 'Data.Grid.Sized.Coord.Class.axisSteps', transliterated: every value
-- within @r@ steps, each reached by the fewest steps, ordered by the
-- surviving offset.
axisStepsI :: forall a. KnownAxis a => Int -> Int -> [(Int, Int)]
{-# INLINE axisStepsI #-}
axisStepsI r c =
    [(abs d, v) | (d, v) <- reachable, not (any (beats (d, v)) reachable)]
  where
    reachable = [(d, v) | d <- [-r .. r], Just v <- [axisOffset @a c d]]
    beats (d, v) (d', v') = v' == v && (abs d', d') < (abs d, d)

-- | The coordinate: its row-major position and nothing else.
newtype Coord (cs :: [Ax]) = Coord
    { coordPosI :: Int
    } deriving newtype (Eq, Ord, NFData)

-- | Nominal, for the same reason 'Data.Grid.Sized.Ordinal.Ordinal' is: a
-- phantom role would let @coerce@ move a coordinate between shapes.
type role Coord nominal

infixr 5 :.

-- | A displacement: still one 'Int' per axis, since a displacement is
-- unbounded and so cannot be packed into a position. The counterpart of
-- @Coord (MapDiff cs)@.
data Diffs (cs :: [Ax]) where
    DEnd :: Diffs '[]
    (:.) :: !Int -> !(Diffs cs) -> Diffs (c ': cs)

-- | The row-major fold over the axis list, as a class for the reason
-- recorded on @IsCoordList@. Every method takes a position and divides its
-- way in.
class Shape (cs :: [Ax]) where
    shapeSize :: Int
    addDiffs :: Int -> Diffs cs -> Int
    offsetDiffs :: Int -> Diffs cs -> Maybe Int
    anyEdge :: Int -> Bool
    allEdge :: Int -> Bool
    maxDist :: Int -> Int -> Int
    sumDist :: Int -> Int -> Int
    -- | @(steps, position)@ for every coordinate within @r@ steps on each
    -- axis: @npStepsWithin@.
    stepsWithinI :: Int -> Int -> [(Int, Int)]

instance Shape '[] where
    shapeSize = 1
    addDiffs p DEnd = p
    offsetDiffs p DEnd = Just p
    anyEdge _ = False
    allEdge _ = True
    maxDist _ _ = 0
    sumDist _ _ = 0
    stepsWithinI _ _ = [(0, 0)]
    {-# INLINE shapeSize #-}
    {-# INLINE addDiffs #-}
    {-# INLINE offsetDiffs #-}
    {-# INLINE anyEdge #-}
    {-# INLINE allEdge #-}
    {-# INLINE maxDist #-}
    {-# INLINE sumDist #-}
    {-# INLINE stepsWithinI #-}

instance (KnownAxis a, Shape cs) => Shape (a ': cs) where
    shapeSize = axisSize @a * shapeSize @cs
    addDiffs p (d :. ds) =
        case p `quotRem` stride of
            (i, r) -> axisAdd @a i d * stride + addDiffs @cs r ds
      where
        stride = shapeSize @cs
    offsetDiffs p (d :. ds) =
        case p `quotRem` stride of
            (i, r) ->
                (\i' r' -> i' * stride + r') <$> axisOffset @a i d <*>
                offsetDiffs @cs r ds
      where
        stride = shapeSize @cs
    anyEdge p =
        case p `quotRem` shapeSize @cs of
            (i, r) -> axisAtEdge @a i || anyEdge @cs r
    allEdge p =
        case p `quotRem` shapeSize @cs of
            (i, r) -> axisAtEdge @a i && allEdge @cs r
    maxDist p q =
        case (p `quotRem` stride, q `quotRem` stride) of
            ((i, r), (j, s)) -> max (axisDist @a i j) (maxDist @cs r s)
      where
        stride = shapeSize @cs
    sumDist p q =
        case (p `quotRem` stride, q `quotRem` stride) of
            ((i, r), (j, s)) -> axisDist @a i j + sumDist @cs r s
      where
        stride = shapeSize @cs
    stepsWithinI r p =
        case p `quotRem` stride of
            (i, rest) ->
                [ (d + s, v * stride + vs)
                | (d, v) <- axisStepsI @a r i
                , (s, vs) <- stepsWithinI @cs r rest
                ]
      where
        stride = shapeSize @cs
    {-# INLINE shapeSize #-}
    {-# INLINE addDiffs #-}
    {-# INLINE offsetDiffs #-}
    {-# INLINE anyEdge #-}
    {-# INLINE allEdge #-}
    {-# INLINE maxDist #-}
    {-# INLINE sumDist #-}
    {-# INLINE stepsWithinI #-}

-- | The view function behind the @(:|)@ pattern synonym this representation
-- would need: peel the first axis off a coordinate. Free in the real
-- library (a field read); a division here. Every consumer that destructures
-- a coordinate rather than passing it along pays this.
coordSplitI :: forall a cs. Shape cs => Coord (a ': cs) -> (Int, Coord cs)
coordSplitI (Coord p) =
    case p `quotRem` shapeSize @cs of
        (i, r) -> (i, Coord r)
{-# INLINE coordSplitI #-}

allCoordI :: forall cs. Shape cs => [Coord cs]
allCoordI = map Coord [0 .. shapeSize @cs - 1]
{-# INLINE allCoordI #-}

zeroCoordI :: Coord cs
zeroCoordI = Coord 0
{-# INLINE zeroCoordI #-}

-- | @('Data.AffineSpace..+^')@.
addI :: forall cs. Shape cs => Coord cs -> Diffs cs -> Coord cs
addI (Coord p) ds = Coord (addDiffs @cs p ds)
{-# INLINE addI #-}

-- | 'Data.Grid.Sized.Coord.offsetCoord'.
offsetI :: forall cs. Shape cs => Coord cs -> Diffs cs -> Maybe (Coord cs)
offsetI (Coord p) ds = Coord <$> offsetDiffs @cs p ds
{-# INLINE offsetI #-}

onBoundaryI :: forall cs. Shape cs => Coord cs -> Bool
onBoundaryI (Coord p) = anyEdge @cs p
{-# INLINE onBoundaryI #-}

-- | The Chebyshev distance, as 'Data.Grid.Sized.Coord.coordDistance'.
coordDistanceI :: forall cs. Shape cs => Coord cs -> Coord cs -> Int
coordDistanceI (Coord p) (Coord q) = maxDist @cs p q
{-# INLINE coordDistanceI #-}

neighboursI :: forall cs. Shape cs => Coord cs -> [Coord cs]
neighboursI (Coord p) = [Coord v | (s, v) <- stepsWithinI @cs 1 p, s > 0]
{-# INLINE neighboursI #-}

newtype IGrid (cs :: [Ax]) a = IGrid
    { igridVector :: V.Vector a
    }

instance NFData a => NFData (IGrid cs a) where
    rnf (IGrid v) = rnf v

tabulateI :: forall cs a. Shape cs => (Coord cs -> a) -> IGrid cs a
tabulateI f = IGrid (V.generate (shapeSize @cs) (f . Coord))
{-# INLINABLE tabulateI #-}

indexI :: IGrid cs a -> Coord cs -> a
indexI (IGrid v) (Coord i) = V.unsafeIndex v i
{-# INLINE indexI #-}

-- | 'V.imap', which is what an @Int@ coordinate wants: the index the vector
-- combinator already has /is/ the coordinate, so there is nothing to zip
-- against. Three forms were measured (@bench/Main.hs@ keeps the raw-vector
-- controls that settled it): this one, @V.zipWith f (V.generate n Coord) v@
-- (8.9 MB --- generate does not fuse into zipWith, so a 90,000-element boxed
-- vector of coordinates is materialised) and @V.zipWith f (V.fromList ...)@
-- (12 MB). The real library's @imap@ is the zipWith form and pays none of
-- that, because its coordinate list fuses; ours has no list to fuse.
imapI :: (Coord cs -> a -> b) -> IGrid cs a -> IGrid cs b
imapI f (IGrid v) = IGrid (V.imap (\i x -> f (Coord i) x) v)
{-# INLINE imapI #-}

ifoldlI' :: (Coord cs -> b -> a -> b) -> b -> IGrid cs a -> b
ifoldlI' f z (IGrid v) = V.ifoldl' (\acc i x -> f (Coord i) acc x) z v
{-# INLINABLE ifoldlI' #-}

-- | 'Data.Grid.Sized.transposeGrid'. The permutation table the real one
-- builds out of @map (coordPosition . tranposeCoord) allCoord@ is index
-- arithmetic here, so there is nothing to build.
transposeI ::
       forall w h a. (KnownAxis w, KnownAxis h)
    => IGrid '[ w, h] a
    -> IGrid '[ h, w] a
transposeI (IGrid v) =
    IGrid $
    V.unsafeBackpermute
        v
        (V.generate
             (x * y)
             (\k ->
                  case k `quotRem` x of
                      (b, a) -> a * y + b))
  where
    x = axisSize @w
    y = axisSize @h
{-# INLINABLE transposeI #-}
