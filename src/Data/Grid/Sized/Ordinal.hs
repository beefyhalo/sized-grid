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
import           Control.Lens            (Prism', prism')
import           Control.Monad           (unless)
import           Data.Aeson
import           Data.Proxy
import           GHC.TypeLits
import           GHC.Natural             (naturalToWord)
import qualified GHC.TypeNats            as TN
import           System.Random

-- | 'Ordinal' is not an instance of 'Num': most of that class (negate, in
-- particular) would only be partial.
newtype Ordinal (m :: Nat) = UnsafeOrdinal
    { ordinalToInt :: Int
    } deriving (Eq, Ord)

-- | Nominal, not the phantom role GHC would infer: a phantom role would let
-- @coerce@ forge an out-of-range value.
type role Ordinal nominal

-- | sized-grid-adr.14. Via 'GHC.TypeNats.natVal' and 'naturalToWord', not
-- @GHC.TypeLits.natVal@ and 'fromInteger'. A @KnownNat@ dictionary /is/ a
-- 'Natural', so 'naturalToWord' is an unboxing and nothing else; the 'Integer'
-- route compiles to @integerToInt# (integerFromNatural d)@, and the
-- intermediate 'Integer' is a heap allocation on every call. That does not
-- matter while the caller inlines far enough for the dictionary to be a
-- literal and the whole thing to constant-fold -- which is why it went
-- unnoticed -- but it costs 16 bytes a call the moment the caller does not,
-- and the checked-offset sweeps are exactly that case. It was half of the
-- 32 B/call this issue set out to explain.
--
-- The truncation 'naturalToWord' does above @maxBound :: Word@ is the same
-- truncation 'fromInteger' was already doing into 'Int', on sizes no 'Ordinal'
-- could be indexed by anyway.
ordinalSize :: forall m. KnownNat m => Int
ordinalSize = fromIntegral $ naturalToWord $ TN.natVal (Proxy @m)
{-# INLINE ordinalSize #-}

-- | __Precondition:__ @0 <= i < m@, checked.
--
-- sized-grid-adr.14. The check is a guard on the raw 'Int' rather than
-- @assert@ on the constructed 'Ordinal', and that is a performance decision,
-- not a stylistic one. @assert@ under @-fno-ignore-asserts@ desugars to a call
-- to @assertError@, which is opaque to the simplifier and lazy in the value it
-- guards: the 'Ordinal' has to exist as a boxed thing to be passed to it, and
-- the call is big enough to stop the enclosing method inlining at all. Both
-- halves cost. Two full runs of the suite, back to back, assert against guard:
--
--   (.+^) x360000, four corner reads   11.52 MB, 3.49 ms  ->    145 B, 2.23 ms
--   (.+^) x360000, one Clamped axis    11.62 MB, 3.18 ms  ->  96.2 KB, 2.30 ms
--   onBoundary x360000                 59.65 MB, 11.3 ms  -> 36.72 MB, 7.20 ms
--   coordDistance x360000              59.76 MB, 12.9 ms  -> 36.72 MB, 8.80 ms
--   (.+^) x10000, Periodic              3.00 MB, 712 us   ->  2.68 MB,  650 us
--
-- Nothing else in the suite moves, in either column.
--
-- 'Periodic' paid almost none of it, because it overrides 'offsetIsCoord' and
-- so never reaches 'offsetByPosition'; that Periodic was free while Clamped
-- was not is what identified the assert rather than the fold. What Periodic
-- does gain above is the 'ordinalSize' half, which every policy pays.
--
-- Two details of the shape below are load-bearing, both measured:
--
--   * The bound comes from 'ordinalSize', which must not allocate -- see its
--     note. Half the 32 B/call was the 'Integer' it used to build.
--
--   * The failure branch is a nullary CAF and must stay one. Passing the
--     offending index and size to it, so the message could name them, put
--     'offsetCoord' back from 35 MB to 73 MB a sweep: free variables in the
--     cold branch keep boxed values alive across it and grow the body past
--     the point where callers still inline it. A less specific message is
--     worth 2x on the checked-offset path.
--
-- The check is therefore unconditional. Unlike @assert@ it cannot be stripped
-- by @-fignore-asserts@, so no optimisation flag -- on this library or
-- appended by a consumer's @cabal.project@ -- can silently turn it off. See
-- the notes in the .cabal and the check in @tests-downstream@.
unsafeOrdinal :: forall m. KnownNat m => Int -> Ordinal m
unsafeOrdinal i
    | i < 0 || i >= sz = preconditionViolated
    | otherwise = UnsafeOrdinal i
  where
    sz = ordinalSize @m
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
        Nothing -> invariantViolated i (ordinalSize @n)
        Just (SomeNat (_ :: Proxy k)) ->
            case cmpNat (Proxy @(k + 1)) (Proxy @n) of
                LTI -> func k
                EQI -> func k
                GTI -> invariantViolated i (ordinalSize @n)

-- | The 'unsafeOrdinal' failure branch. Nullary on purpose: see the note
-- there. It cannot name the index or the size, and that is the price.
preconditionViolated :: a
preconditionViolated =
    errorWithoutStackTrace
        "Data.Grid.Sized.Ordinal: unsafeOrdinal was called with its precondition violated. Ordinals must satisfy 0 <= i < m."
{-# NOINLINE preconditionViolated #-}

-- | An 'Ordinal' that is already out of range, discovered later by
-- 'reifyOrdinal'. That can only happen if 'unsafeOrdinal' was called with its
-- precondition violated /and/ the check above was somehow not run, so the
-- message says so. Unlike 'preconditionViolated' this one is free to name the
-- values: 'reifyOrdinal' is not on any hot path.
invariantViolated :: Int -> Int -> a
invariantViolated i m =
    error $
    "Data.Grid.Sized.Ordinal: " ++
    show i ++
    " is not a valid Ordinal " ++
    show m ++
    ". Ordinals must satisfy 0 <= i < m; this one was built by unsafeOrdinal " ++
    "with its precondition violated."
{-# NOINLINE invariantViolated #-}

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
