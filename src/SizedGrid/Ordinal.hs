{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}

-- |
-- An 'Ordinal' is a number in @[0, m)@, where @m@ is a type-level 'Nat'.
--
-- == Representation
--
-- This used to be a GADT carrying the value at the type level:
--
-- > data Ordinal m where
-- >   Ordinal :: (KnownNat n, KnownNat m, n + 1 <= m) => Proxy n -> Ordinal m
--
-- which made every value a boxed 'Proxy' plus two 'KnownNat' dictionaries, and
-- every construction a call to 'someNatVal' (which builds a dictionary through
-- 'unsafeCoerce') followed by 'cmpNat'. Coordinate arithmetic therefore
-- allocated a type-level natural per operation: @tabulate@ on a 300x300 grid of
-- 'Int' allocated 93 MB for a payload well under 1 MB.
--
-- It is now a newtype over 'Int'. The size stays at the type level, the value
-- does not, and the invariant @0 <= i < m@ is maintained by the constructors in
-- this module rather than by the type checker. The constructor is not exported;
-- 'unsafeOrdinal' is the single unchecked way in, and it states and asserts its
-- precondition.
--
-- The one thing the GADT gave away for free was recovering the value as a type,
-- which 'SizedGrid.Coord.Class.asSizeProxy' needs. 'reifyOrdinal' does that on
-- demand, so the 'someNatVal' cost is paid at the one call site that wants it
-- instead of by every value that might.
module SizedGrid.Ordinal
    ( Ordinal
      -- * Conversion
    , ordinalToInt
    , ordinalToNum
    , numToOrdinal
    , unsafeOrdinal
    , _Ordinal
      -- * Sizes and evidence
    , ordinalSize
    , reifyOrdinal
    , strengthenOrdinal
    , weakenOrdinal
    ) where

import           SizedGrid.Internal.Type (requiring)

import           Control.Exception       (assert)
import           Control.Lens            (Prism', prism')
import           Control.Monad           (unless)
import           Data.Aeson
import           Data.Constraint
import           Data.Constraint.Nat
import           Data.Proxy
import           GHC.TypeLits
import           System.Random

-- | An 'Ordinal' can only hold @m@ different values, corresponding to
-- @0 .. m - 1@.
--
-- Despite representing a number, 'Ordinal' is not an instance of 'Num': most of
-- that class (negate, in particular) would only be partial.
newtype Ordinal (m :: Nat) = UnsafeOrdinal
    { ordinalToInt :: Int
    } deriving (Eq, Ord)

-- | Nominal, not the phantom role GHC would infer. With a phantom role
-- @coerce :: Ordinal 9 -> Ordinal 3@ typechecks, and it is exactly the sort of
-- thing that looks harmless: it produces a value whose type promises it is in
-- range when it is not, which is the invariant every unchecked read in this
-- library relies on.
type role Ordinal nominal

-- | The number of distinct values an @'Ordinal' m@ has, as an 'Int'.
--
-- 'GHC.TypeLits.natVal' returns an 'Integer'; the sizes here index a
-- 'Data.Vector.Vector', so they have to fit in an 'Int' anyway.
ordinalSize :: forall m. KnownNat m => Int
ordinalSize = fromInteger $ natVal (Proxy @m)
{-# INLINE ordinalSize #-}

-- | Build an 'Ordinal' without checking it.
--
-- __Precondition:__ @0 <= i < m@. This is the only unchecked construction in
-- the library, and everything else that builds an 'Ordinal' has just
-- established the precondition by a @mod@, a @min@/@max@ clamp or a comparison
-- against 'ordinalSize'.
--
-- The precondition is checked by 'assert', so it is live under
-- @-fno-ignore-asserts@ (and in an unoptimised build) and compiled away under
-- @-O@. Use 'numToOrdinal' when the value is not already known to be in range.
unsafeOrdinal :: forall m. KnownNat m => Int -> Ordinal m
unsafeOrdinal i =
    assert (i >= 0 && i < ordinalSize @m) $ UnsafeOrdinal i
{-# INLINE unsafeOrdinal #-}

-- | Convert a normal integral to an ordinal. If it is outside the range (< 0 or
-- >= m), Nothing is returned.
numToOrdinal ::
       forall a m. (KnownNat m, Integral a)
    => a
    -> Maybe (Ordinal m)
numToOrdinal n
    | i >= 0 && i < natVal (Proxy @m) = Just $ UnsafeOrdinal $ fromInteger i
    | otherwise = Nothing
  where
    -- Via 'Integer' rather than 'fromIntegral' straight to 'Int': @a@ may be
    -- wider than 'Int', and a wrapping conversion would turn an out-of-range
    -- input into an in-range one.
    i = toInteger n

-- | Transform an ordinal to a given number
ordinalToNum :: Num a => Ordinal m -> a
ordinalToNum = fromIntegral . ordinalToInt
{-# INLINE ordinalToNum #-}

-- | Recover an ordinal's value as a type-level 'Nat', with the evidence that it
-- is in range.
--
-- This is the operation the old GADT representation carried in every value. The
-- library needs it in exactly one place --- 'SizedGrid.Coord.Class.asSizeProxy',
-- used by 'SizedGrid.Grid.Grid.shrinkGrid' to turn a window offset into a
-- @dropGrid@ --- so it is reconstructed here on demand.
--
-- The evidence is real: 'cmpNat' compares the reified value against @n@ at
-- runtime and hands back a proof. The 'GTI' branch is reachable only if the
-- representation invariant has already been broken by a misuse of
-- 'unsafeOrdinal', which is why it reports that rather than the comparison.
reifyOrdinal ::
       forall n x. KnownNat n
    => Ordinal n
    -> (forall m. (KnownNat m, m + 1 <= n) =>
                      Proxy m -> x)
    -> x
reifyOrdinal (UnsafeOrdinal i) func =
    case someNatVal (toInteger i) of
        Nothing -> invariantViolated i (natVal (Proxy @n))
        Just (SomeNat (p :: Proxy k)) ->
            (case cmpNat (Proxy @(k + 1)) (Proxy @n) of
                 LTI -> func p
                 EQI -> func p
                 GTI -> invariantViolated i (natVal (Proxy @n))) \\
            plusNat @k @1

-- | Reported when a value that claims to be an @'Ordinal' m@ is not in
-- @[0, m)@. Only 'unsafeOrdinal' can produce one.
invariantViolated :: Int -> Integer -> a
invariantViolated i m =
    error $
    "SizedGrid.Ordinal: " ++
    show i ++
    " is not a valid Ordinal " ++
    show m ++
    ". Ordinals must satisfy 0 <= i < m; this one was built by unsafeOrdinal " ++
    "with its precondition violated."

-- | Reinterpret an ordinal at a larger size. Always succeeds: @i < n@ and
-- @n <= m@ give @i < m@.
strengthenOrdinal :: forall n m. (KnownNat m, n <= m) => Ordinal n -> Ordinal m
strengthenOrdinal (UnsafeOrdinal i) =
    -- Both constraints are the caller's contract rather than something this
    -- body can use: @n <= m@ is what makes the reinterpretation sound, and
    -- 'KnownNat' stays for source compatibility with the GADT version, which
    -- needed it to rebuild the proxy.
    requiring @(KnownNat m, n <= m) $ UnsafeOrdinal i

-- | Reinterpret an ordinal at a smaller size, if it fits.
weakenOrdinal :: KnownNat m => Ordinal n -> Maybe (Ordinal m)
weakenOrdinal = numToOrdinal . ordinalToInt

-- | Convert between an ordinal and a usual number. This is a `Prism` as it may fail as `Ordinals` can only exist in a certain range.
_Ordinal :: (KnownNat n, Integral a) => Prism' a (Ordinal n)
_Ordinal = prism' ordinalToNum numToOrdinal

instance KnownNat m => Show (Ordinal m) where
    show o =
        "Ordinal (" ++
        show (ordinalToInt o) ++ "/" ++ show (natVal (Proxy @m)) ++ ")"

instance (1 <= m, KnownNat m) => Random (Ordinal m) where
    randomR (mi, ma) g =
        let (n, g') = randomR (ordinalToInt mi, ordinalToInt ma) g
        in (unsafeOrdinal n, g')
    random = randomR (minBound, maxBound)

instance (1 <= m, KnownNat m) => Bounded (Ordinal m) where
    minBound = requiring @(1 <= m) $ UnsafeOrdinal 0
    maxBound = UnsafeOrdinal $ ordinalSize @m - 1

instance (1 <= m, KnownNat m) => Enum (Ordinal m) where
    toEnum n =
        case numToOrdinal n of
            Just o -> o
            Nothing ->
                error $
                "toEnum: " ++
                show n ++
                " is out of range for Ordinal " ++ show (natVal (Proxy @m))
    fromEnum = ordinalToInt
    -- The defaults for these three route every element through 'toEnum', which
    -- both re-checks a bound the endpoints already establish and --- for
    -- 'enumFrom', whose default counts up from @fromEnum x@ forever --- walks
    -- straight off the end and calls 'error'. @[minBound ..]@ used to throw.
    enumFromTo a b = map UnsafeOrdinal [ordinalToInt a .. ordinalToInt b]
    enumFromThenTo a b c =
        map UnsafeOrdinal [ordinalToInt a,ordinalToInt b .. ordinalToInt c]
    enumFrom a = enumFromTo a maxBound
    enumFromThen a b
        | ordinalToInt b >= ordinalToInt a = enumFromThenTo a b maxBound
        | otherwise = enumFromThenTo a b minBound

instance KnownNat m => ToJSON (Ordinal m) where
    toJSON o = object ["size" .= natVal (Proxy @m), "value" .= ordinalToInt o]

instance KnownNat m => FromJSON (Ordinal m) where
    parseJSON =
        withObject "Ordinal" $ \v -> do
            size <- v .: "size"
            let m = natVal (Proxy @m)
            unless (size == m) $
                fail $
                "Ordinal: expected size " ++ show m ++ ", got " ++ show size
            value <- v .: "value"
            case numToOrdinal @Integer value of
                Just o -> return o
                Nothing ->
                    fail $
                    "Ordinal: value " ++
                    show value ++ " is not in [0, " ++ show m ++ ")"

instance KnownNat m => ToJSONKey (Ordinal m)

instance KnownNat m => FromJSONKey (Ordinal m)
