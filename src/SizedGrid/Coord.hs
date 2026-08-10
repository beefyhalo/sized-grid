{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module SizedGrid.Coord where

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

-- | The type of difference between two coords. A n-dimensional coord should have a `Diff` of an n-tuple of `Integers`. We use `Identity` and our 1-tuple. Unfortuantly, each instance is manual at the moment.
type family CoordDiff (cs :: [k]) :: Type

type instance CoordDiff '[] = ()
type instance CoordDiff '[a] = Identity (Diff a)
type instance CoordDiff '[a, b] = (Diff a, Diff b)
type instance CoordDiff '[a, b, c] = (Diff a, Diff b, Diff c)
type instance CoordDiff '[a, b, c, d] =
     (Diff a, Diff b, Diff c, Diff d)
type instance CoordDiff '[a, b, c, d, e] =
     (Diff a, Diff b, Diff c, Diff d, Diff e)
type instance CoordDiff '[a, b, c, d, e, f] =
     (Diff a, Diff b, Diff c, Diff d, Diff e, Diff f)

-- | Apply `Diff` to each element of a type level list. This is required as type families can't be partially applied.
type family MapDiff xs where
  MapDiff '[] = '[]
  MapDiff (x ': xs) = Diff x ': MapDiff xs

instance ( All AffineSpace cs
         , AdditiveGroup (CoordDiff cs)
         , IsProductType (CoordDiff cs) (MapDiff cs)
         ) =>
         AffineSpace (Coord cs) where
    type Diff (Coord cs) = CoordDiff cs
    -- 'productTypeTo' and 'productTypeFrom' rather than the generic 'to' and
    -- 'from': going through @SOP@ meant taking apart a sum that
    -- 'IsProductType' already guarantees has one arm, and GHC will not reduce
    -- @Code (CoordDiff cs)@ far enough to see the other arm is impossible. That
    -- forced an unreachable @error@ equation on ('.+^'). These two do the same
    -- job with no sum in the way.
    Coord a .-. Coord b =
        let helper ::
                   All AffineSpace xs => NP I xs -> NP I xs -> NP I (MapDiff xs)
            helper Nil Nil                 = Nil
            helper (I x :* xs) (I y :* ys) = I (x .-. y) :* helper xs ys
        in productTypeTo $ helper a b
    Coord a .+^ b =
        let helper :: All AffineSpace xs => NP I xs -> NP I (MapDiff xs) -> NP I xs
            helper Nil Nil                 = Nil
            helper (I x :* xs) (I y :* ys) = I (x .+^ y) :* helper xs ys
        in Coord $ helper a $ productTypeFrom b

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

-- | Calculate the Moore neighbourhood around a point. Includes the center
moorePoints ::
     forall a cs. (Enum a, Num a, AllDiffSame a cs, All AffineSpace cs)
  => a
  -> Coord cs
  -> [Coord cs]
moorePoints n (Coord cs) =
  let helper :: (All AffineSpace xs, AllDiffSame a xs) => NP I xs -> [NP I xs]
      helper Nil = [Nil]
      helper (I a :* as) = do
        delta :: a <- [-n .. n]
        next <- helper as
        return (I (a .+^ delta) :* next)
  in map Coord $ helper cs

-- | Calculate the von Neuman neighbourhood around a point. Includes the center
vonNeumanPoints ::
     forall a cs.
     ( Enum a
     , Num a
     , Ord a
     , All Integral (MapDiff cs)
     , AllDiffSame a cs
     , All AffineSpace cs
     , Ord (CoordDiff cs)
     , IsProductType (CoordDiff cs) (MapDiff cs)
     , AdditiveGroup (CoordDiff cs)
     )
  => a
  -> Coord cs
  -> [Coord cs]
vonNeumanPoints n c =
    let helper :: Coord cs -> Bool
        helper new =
            sum
                (hcollapse $
                 hcmap
                     (Proxy :: Proxy Integral)
                     (\(I a) -> K (abs $ fromIntegral a)) $
                 from (min (new .-. c) (c .-. new))) <= n
    in filter helper $ moorePoints n c

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
