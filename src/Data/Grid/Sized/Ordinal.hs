{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}

-- | A number in @[0, m)@, where @m@ is a type-level 'Nat'.
module Data.Grid.Sized.Ordinal
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

import           Data.Grid.Sized.Internal.Type (requiring)

import           Control.DeepSeq         (NFData (..))
import           Control.Exception       (assert)
import           Control.Lens            (Prism', prism')
import           Control.Monad           (unless)
import           Data.Aeson
import           Data.Proxy
import           GHC.TypeLits
import           System.Random

-- | 'Ordinal' is not an instance of 'Num': most of that class (negate, in
-- particular) would only be partial.
newtype Ordinal (m :: Nat) = UnsafeOrdinal
    { ordinalToInt :: Int
    } deriving (Eq, Ord)

-- | Nominal, not the phantom role GHC would infer: a phantom role would let
-- @coerce@ forge an out-of-range value.
type role Ordinal nominal

ordinalSize :: forall m. KnownNat m => Int
ordinalSize = fromInteger $ natVal (Proxy @m)
{-# INLINE ordinalSize #-}

-- | __Precondition:__ @0 <= i < m@, checked via 'assert'.
unsafeOrdinal :: forall m. KnownNat m => Int -> Ordinal m
unsafeOrdinal i =
    assert (i >= 0 && i < ordinalSize @m) $ UnsafeOrdinal i
{-# INLINE unsafeOrdinal #-}

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

ordinalToNum :: Num a => Ordinal m -> a
ordinalToNum = fromIntegral . ordinalToInt
{-# INLINE ordinalToNum #-}

-- | The value is handed to the continuation as a required type argument, so
-- the caller writes @reifyOrdinal o $ \\m -> ...@ and @m@ is a type.
reifyOrdinal ::
       forall n x. KnownNat n
    => Ordinal n
    -> (forall m -> (KnownNat m, m + 1 <= n) => x)
    -> x
reifyOrdinal (UnsafeOrdinal i) func =
    case someNatVal (toInteger i) of
        Nothing -> invariantViolated i (natVal (Proxy @n))
        Just (SomeNat (_ :: Proxy k)) ->
            case cmpNat (Proxy @(k + 1)) (Proxy @n) of
                LTI -> func k
                EQI -> func k
                GTI -> invariantViolated i (natVal (Proxy @n))

invariantViolated :: Int -> Integer -> a
invariantViolated i m =
    error $
    "Data.Grid.Sized.Ordinal: " ++
    show i ++
    " is not a valid Ordinal " ++
    show m ++
    ". Ordinals must satisfy 0 <= i < m; this one was built by unsafeOrdinal " ++
    "with its precondition violated."

-- | Always succeeds: @i < n@ and @n <= m@ give @i < m@.
strengthenOrdinal :: forall n m. (KnownNat m, n <= m) => Ordinal n -> Ordinal m
strengthenOrdinal (UnsafeOrdinal i) =
    requiring @(KnownNat m, n <= m) $ UnsafeOrdinal i

weakenOrdinal :: KnownNat m => Ordinal n -> Maybe (Ordinal m)
weakenOrdinal = numToOrdinal . ordinalToInt

_Ordinal :: (KnownNat n, Integral a) => Prism' a (Ordinal n)
_Ordinal = prism' ordinalToNum numToOrdinal

instance NFData (Ordinal m) where
    rnf = rnf . ordinalToInt

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
    -- Overridden because the default 'enumFrom' counts up from @fromEnum x@
    -- forever, walking off the end and calling 'error'.
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
