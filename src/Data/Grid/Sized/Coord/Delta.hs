-- |
-- Module      :  Data.Grid.Sized.Coord.Delta
-- License     :  MIT -style (see the file LICENSE)
--
-- A displacement: one 'Data.AffineSpace.Diff' per axis, and the difference
-- space that @'Data.Grid.Sized.Coord.Coord' cs@ is an affine space over.
--
-- == Why this is not a @Coord@
--
-- It used to be. @'Data.AffineSpace.Diff' ('Data.Grid.Sized.Coord.Coord' cs)@
-- was @Coord ('MapDiff' cs)@ -- a coordinate again, holding one 'Diff' per
-- axis -- so a displacement was written the same way a position was and
-- inherited every @Coord@ instance.
--
-- sized-grid-adr.16 ended that, because a position and a displacement stopped
-- being able to share a representation. A @Coord@ is now its row-major
-- position, one 'Int' in @[0, 'Data.Grid.Sized.Coord.MaxCoordSize' cs)@, which
-- only means anything for a list of /axes/ with known sizes. A displacement is
-- unbounded and signed, and @'MapDiff' '[Clamped 5, Periodic 3]@ is
-- @'[Int, Int]@ -- a list with no sizes in it at all, one that
-- @MaxCoordSize@ does not even reduce over. One type constructor cannot carry
-- both, so the displacement moved here and kept the spine.
--
-- == Indexed by the diff list, not the axis list
--
-- @'Data.AffineSpace.Diff' ('Data.Grid.Sized.Coord.Coord' cs) = Delta ('MapDiff' cs)@,
-- so a @Delta@ names its own components and says nothing about which grid it
-- came from. That is the honest shape: the difference space of an affine
-- space is genuinely shared, and @Coord '[Clamped 3, Clamped 3]@ and
-- @Coord '[Clamped 5, Clamped 5]@ have the /same/ one -- Z^2. A direction
-- table can therefore be written once and used at every grid it fits:
--
-- > data Move = U | D | L | R
-- >
-- > delta :: Move -> Delta '[Int, Int]
-- > delta = \case
-- >     U -> deltaFromTuple (-1, 0)
-- >     D -> deltaFromTuple ( 1, 0)
-- >     L -> deltaFromTuple (0, -1)
-- >     R -> deltaFromTuple (0,  1)
--
-- Indexing by the axis list instead would have made that a distinct type per
-- grid shape, each a copy of Z^2 with a nominal role (@cs@ would appear only
-- under a type family application, so @coerce@ could not move between them),
-- and would have needed an escape hatch to convert. The bounded, genuinely
-- shape-dependent offsets in this library --
-- 'Data.Grid.Sized.Coord.PuncturedCoord' and
-- 'Data.Grid.Sized.Stencil.Stencil' -- already have their own types and their
-- own flat representations. This one is the unbounded case, and unbounded
-- means shape-free.
--
-- See sized-grid-qfg for the type this deliberately is /not/: on an
-- all-'Data.Grid.Sized.Coord.Class.Boundaryless' shape the displacements do
-- form a finite group, and that is a separate, bounded type.
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Grid.Sized.Coord.Delta
  ( Delta (..),
    pattern (:^),
    pattern NoDelta,
    deltaSplit,
    Rotation (..),
    rotateDelta,
    turnLeft,
    turnRight,
    mooreDeltas,
    vonNeumannDeltas,
    kingDeltas,

    -- * Building and taking apart
    singleDelta,
    appendDelta,
    deltaFromTuple,
    deltaToTuple,
  )
where

import Control.Applicative (empty)
import Control.DeepSeq (NFData (..))
import Control.Monad.State
import Data.AdditiveGroup
import Data.AffineSpace (Diff)
import Data.Aeson
import Data.Grid.Sized.Coord.Class (MapDiff)
import Data.List (intercalate)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Generics.SOP hiding (Generic, S, Z)
import Generics.SOP qualified as SOP
import System.Random (Random (..))

-- | A displacement, one component per axis. @ds@ is the list of
-- 'Data.AffineSpace.Diff's -- in this library always @'[Int, Int, ...]@ --
-- not the list of axes.
newtype Delta ds = Delta {unDelta :: NP I ds}
  deriving (Generic)

deltaSplit :: Delta (d ': ds) -> (d, Delta ds)
deltaSplit (Delta (I x :* xs)) = (x, Delta xs)

-- | Cons for displacements, the counterpart of
-- @('Data.Grid.Sized.Coord.:|')@ on positions. A field read in both
-- directions: unlike a @Coord@\'s, a @Delta@\'s spine is still there.
pattern (:^) :: d -> Delta ds -> Delta (d ': ds)
pattern (:^) a as <- (deltaSplit -> (a, as))
  where
    (:^) a (Delta as) = Delta (I a :* as)
{-# INLINE (:^) #-}

-- | The displacement with no components: the zero of the empty difference
-- space, and the end of every @(':^')@ chain.
pattern NoDelta :: Delta '[]
pattern NoDelta = Delta Nil
{-# INLINE NoDelta #-}

-- | Needed because GHC's coverage checker cannot see this view pattern is exhaustive.
{-# COMPLETE (:^) #-}

{-# COMPLETE NoDelta #-}

infixr 5 :^

instance (All Eq ds) => Eq (Delta ds) where
  Delta a == Delta b =
    and $
      hcollapse $
        hcliftA2 (Proxy :: Proxy Eq) (\(I x) (I y) -> K (x == y)) a b

-- | @All Eq ds@ does not follow from @All Ord ds@: superclass evidence must be resolved at instance-declaration time, so both constraints are required.
instance (All Eq ds, All Ord ds) => Ord (Delta ds) where
  compare (Delta a) (Delta b) =
    mconcat $
      hcollapse $
        hcliftA2 (Proxy :: Proxy Ord) (\(I x) (I y) -> K (compare x y)) a b

instance (All Show ds) => Show (Delta ds) where
  show (Delta a) =
    "Delta ["
      ++ intercalate
        ", "
        (hcollapse $ hcliftA (Proxy :: Proxy Show) (\(I x) -> K $ show x) a)
      ++ "]"

instance (All ToJSON ds) => ToJSON (Delta ds) where
  toJSON (Delta a) =
    Array $
      V.fromList $
        hcollapse $
          hcmap (Proxy @ToJSON) (\(I x) -> K $ toJSON x) a

instance (All FromJSON ds) => FromJSON (Delta ds) where
  parseJSON =
    withArray "Delta" $ \v ->
      case SOP.fromList $ V.toList v of
        Just a ->
          Delta
            <$> hsequence
              (hcmap (Proxy @FromJSON) (\(K x) -> parseJSON x) a)
        Nothing -> empty

instance (All Semigroup ds) => Semigroup (Delta ds) where
  Delta a <> Delta b = Delta $ hcliftA2 (Proxy :: Proxy Semigroup) (liftA2 (<>)) a b

instance (All Semigroup ds, All Monoid ds) => Monoid (Delta ds) where
  mappend = (<>)
  mempty = Delta $ hcpure (Proxy :: Proxy Monoid) (pure mempty)

instance (All NFData ds) => NFData (Delta ds) where
  rnf (Delta a) =
    foldr seq () $
      hcollapse $
        hcliftA (Proxy :: Proxy NFData) (\(I x) -> K (rnf x)) a

instance (All AdditiveGroup ds) => AdditiveGroup (Delta ds) where
  zeroV = Delta $ hcpure (Proxy :: Proxy AdditiveGroup) (pure zeroV)
  Delta a ^+^ Delta b =
    Delta $ hcliftA2 (Proxy :: Proxy AdditiveGroup) (liftA2 (^+^)) a b
  negateV (Delta a) =
    Delta $ hcliftA (Proxy :: Proxy AdditiveGroup) (fmap negateV) a
  Delta a ^-^ Delta b =
    Delta $ hcliftA2 (Proxy :: Proxy AdditiveGroup) (liftA2 (^-^)) a b

instance (All Random ds) => Random (Delta ds) where
  random g =
    let (c, g') =
          runState
            (hsequence $ hcpure (Proxy :: Proxy Random) (state random))
            g
     in (Delta c, g')
  randomR (Delta mi, Delta ma) g =
    let (c, g') =
          runState
            ( hsequence $
                hcliftA2
                  (Proxy :: Proxy Random)
                  (\(I a) (I b) -> state (randomR (a, b)))
                  mi
                  ma
            )
            g
     in (Delta c, g')

singleDelta :: a -> Delta '[a]
singleDelta a = Delta (I a :* Nil)

appendDelta :: a -> Delta as -> Delta (a ': as)
appendDelta a (Delta as) = Delta (I a :* as)

-- | A quarter-turn in the 2D square-symmetry group, in the same orientation
-- convention as the hand-written walkers in the example apps: a right turn is
-- @(x, y) -> (y, -x)@, so the first axis is the horizontal one and the second
-- axis is drawn downward.
data Rotation
  = Rotate0
  | Rotate90
  | Rotate180
  | Rotate270
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Rotate a 2D displacement by one of the quarter turns of the square.
rotateDelta :: Num a => Rotation -> Delta '[a, a] -> Delta '[a, a]
rotateDelta rot (x :^ y :^ NoDelta) =
  case rot of
    Rotate0 -> x :^ y :^ NoDelta
    Rotate90 -> y :^ negate x :^ NoDelta
    Rotate180 -> negate x :^ negate y :^ NoDelta
    Rotate270 -> negate y :^ x :^ NoDelta
{-# INLINE rotateDelta #-}

-- | Turn right by 90 degrees.
turnRight :: Num a => Delta '[a, a] -> Delta '[a, a]
turnRight = rotateDelta Rotate90
{-# INLINE turnRight #-}

-- | Turn left by 90 degrees.
turnLeft :: Num a => Delta '[a, a] -> Delta '[a, a]
turnLeft = rotateDelta Rotate270
{-# INLINE turnLeft #-}

-- | Enumerate every delta with each component in @[-r, r]@, excluding the
-- zero delta.
class DeltaList cs where
  deltaList :: Int -> [Delta (MapDiff cs)]

instance DeltaList '[] where
  deltaList _ = [NoDelta]

instance (DeltaList xs, Diff x ~ Int) => DeltaList (x ': xs) where
  deltaList r = [appendDelta d ds | d <- [-r .. r], ds <- deltaList @xs r]

class IsZeroDelta ds where
  isZeroDelta :: Delta ds -> Bool

instance IsZeroDelta '[] where
  isZeroDelta NoDelta = True

instance (Eq d, Num d, IsZeroDelta ds) => IsZeroDelta (d ': ds) where
  isZeroDelta (d :^ ds) = d == 0 && isZeroDelta ds

-- | The L1 distance of a delta: the sum of the magnitudes of each component.
class DeltaManhattan ds where
  deltaManhattan :: Delta ds -> Int

instance DeltaManhattan '[] where
  deltaManhattan _ = 0

instance (d ~ Int, DeltaManhattan ds) => DeltaManhattan (d ': ds) where
  deltaManhattan (Delta (I x :* xs)) = abs x + deltaManhattan (Delta xs)

-- | The Moore neighbourhood deltas: every component is in @[-r, r]@, and the
-- zero delta is omitted.
mooreDeltas :: forall cs. (DeltaList cs, IsZeroDelta (MapDiff cs)) => Int -> [Delta (MapDiff cs)]
mooreDeltas r = filter (not . isZeroDelta) (deltaList @cs r)
{-# INLINE mooreDeltas #-}

-- | The von Neumann neighbourhood deltas: every component stays within @r@ in
-- total absolute distance, excluding the zero delta.
vonNeumannDeltas ::
  forall cs.
  (DeltaList cs, IsZeroDelta (MapDiff cs), DeltaManhattan (MapDiff cs)) =>
  Int ->
  [Delta (MapDiff cs)]
vonNeumannDeltas r =
  filter ((<= r) . deltaManhattan) (mooreDeltas @cs r)
{-# INLINE vonNeumannDeltas #-}

-- | The king's step set at unit radius.
kingDeltas :: forall cs. (DeltaList cs, IsZeroDelta (MapDiff cs)) => [Delta (MapDiff cs)]
kingDeltas = mooreDeltas @cs 1
{-# INLINE kingDeltas #-}

-- | Build a displacement from a tuple of the same arity, where that reads
-- better than a @(':^')@ chain.
deltaFromTuple :: (IsProductType t ds) => t -> Delta ds
deltaFromTuple = Delta . productTypeFrom

deltaToTuple :: (IsProductType t ds) => Delta ds -> t
deltaToTuple = productTypeTo . unDelta
