{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Recursing down the axis list, at a concrete boxed vector.
--
-- @collapseGrid@,
-- @gridFromList@ and the two JSON instances all
-- recurse /down the axis list/. A naive recursion keeping the grid generic in
-- @v@ needs a fresh @'VG.Vector' v a@ dictionary at each level, which GHC will
-- not specialise through (an @INLINABLE@ pragma does not help, measured),
-- leaving every 'VG.take'\/'VG.drop'\/'VG.concat' as an indirect call --
-- 60-300% slower than a monomorphic version. So the recursion here is
-- monomorphic on boxed "Data.Vector" ('splitBoxedBySize'), and the callers in
-- "Data.Grid.Sized.Internal.Grid.Core" convert at the boundary with
-- 'VG.convert'; plain-list recursion and closure-passing were both measured
-- and are worse.
--
-- That is also why this is a module of its own and not four @where@ clauses:
-- the recursion must not be able to see the vector parameter, and a module
-- boundary is the one way to say so that cannot be undone by accident.
--
-- @collapseGrid@ and
-- @gridFromList@ get the boxed case back for
-- free via a RULE matching @v ~ "Data.Vector".Vector@, bypassing 'VG.convert'
-- entirely (GHC's own fusion turns a boxed-to-boxed 'VG.convert' into a
-- 'clone', not a no-op, so this has to be done explicitly). The same trick
-- can't reach 'toJSON'\/'parseJSON': as class methods, a RULE only sees the
-- opaque 'ToJSON'\/'FromJSON' dictionary at the call site, with no way to
-- recover the underlying @'VG.Vector' v a@ from it.
--
-- Keep it this way: growing a @'VG.Vector' v a@ constraint on any function
-- here, or switching to the generic
-- @splitVectorBySize@, brings the regression
-- straight back.
module Data.Grid.Sized.Internal.Grid.Nest
  ( CollapseGrid
  , nestByShape
  , flattenByShape
  , nestedToJSON
  , nestedParseJSON
  ) where

import           Data.Grid.Sized.Coord (AllSizedKnown (..), MaxCoordSize,
                                        SizeProof (..))

import           Data.Aeson
import           Data.Aeson.Types      (Parser)
import           Data.Proxy            (Proxy (..))
import qualified Data.Vector           as V
import qualified GHC.TypeLits          as GHC

-- | Given a grid type, give back a series of nested lists repesenting the grid. The lists will have a number of layers equal to the dimensionality.
type family CollapseGrid cs a where
  CollapseGrid '[] a = a
  CollapseGrid (c ': cs) a = [CollapseGrid cs a]

-- | @splitVectorBySize@ at a boxed vector, for the recursions below.
--
-- Written out rather than calling the exported generic one, and this is the
-- single change that mattered: inside a function that is itself
-- polymorphically recursive, the generic version is reached through a
-- @'VG.Vector' V.Vector a@ dictionary and its 'VG.take' and 'VG.drop' never
-- reduce to the O(1) slice they are. Here they do. Restoring this one helper
-- took @collapseGrid@ from 83% above baseline back to level.
splitBoxedBySize :: Int -> V.Vector a -> [V.Vector a]
splitBoxedBySize n v
  | n <= 0    = error $ "splitBoxedBySize: chunk size must be positive, got " ++ show n
  | otherwise = [ V.slice i (min n (len - i)) v | i <- [0, n .. len - 1] ]
  where
    len = V.length v

-- | The axis-list recursion of @collapseGrid@, at a concrete boxed vector.
nestByShape :: forall cs a. AllSizedKnown cs => V.Vector a -> CollapseGrid cs a
nestByShape v =
  case sizeProof @cs of
    SizeNil -> v V.! 0
    SizeCons @_ @_ @rest ->
      map (nestByShape @rest) $
      splitBoxedBySize (fromIntegral $ GHC.natVal (Proxy @(MaxCoordSize rest))) v

-- | The axis-list recursion of @gridFromList@, flattening to row-major order
-- and checking the length at every dimension on the way.
flattenByShape ::
     forall cs a. AllSizedKnown cs
  => CollapseGrid cs a
  -> Maybe (V.Vector a)
flattenByShape cg =
  case sizeProof @cs of
    SizeNil -> Just $ V.singleton cg
    SizeCons @_ @n @rest ->
      if length cg == fromIntegral (GHC.natVal (Proxy @n))
        then V.concat <$> traverse (flattenByShape @rest) cg
        else Nothing

-- | 'toJSON' for a grid, at a concrete boxed vector. Separate from the instance
-- for the reason given under \"Recursing down the axis list\": the recursion
-- must not carry the vector parameter.
nestedToJSON ::
     forall cs a. (AllSizedKnown cs, ToJSON a)
  => V.Vector a
  -> Value
-- The last axis is a case of its own, and that is sized-grid-adr.12: aeson's
-- own @'ToJSON' ('V.Vector' a)@ turns the innermost row into an 'Array'
-- directly. Without it the general branch reaches this row too, and splits it
-- into one single-element slice per cell only for @SizeNil@ to read the cell
-- back out of it and @'toJSON' :: ['Value'] -> 'Value'@ to rebuild a vector
-- from the list of results -- three intermediates per cell to produce what the
-- vector instance produces in one pass. Measured on @toJSON 100x100@:
-- 652 us and 3.7 MB became 288 us and 1.3 MB.
nestedToJSON v =
  case sizeProof @cs of
    SizeNil -> toJSON (v V.! 0)
    SizeCons @_ @_ @rest ->
      case sizeProof @rest of
        SizeNil -> toJSON v
        SizeCons{} ->
          toJSON $
          map (nestedToJSON @rest) $
          splitBoxedBySize (fromIntegral $ GHC.natVal (Proxy @(MaxCoordSize rest))) v

-- | 'parseJSON' for a grid, producing the flat row-major vector. Separate from
-- the instance so the recursion does not carry the vector parameter; see
-- \"Recursing down the axis list\".
nestedParseJSON ::
     forall cs a. (AllSizedKnown cs, FromJSON a)
  => Value
  -> Parser (V.Vector a)
-- The last axis is a case of its own, for the reason given on 'nestedToJSON'
-- and with the same measurement behind it (sized-grid-adr.12). At the
-- innermost row aeson's own @'FromJSON' [a]@ parses the elements in one pass;
-- the general branch would parse the row to a @['Value']@ first and then hand
-- each 'Value' to @SizeNil@, which wraps every cell in a one-element vector
-- for 'V.concat' to copy straight back out. The explicit value traversal also
-- avoids Aeson's special @[Char]@ instance, which expects a JSON string rather
-- than the array of singleton strings emitted for a character row. Measured on
-- the isolated parse of
-- a 100x100 grid: 1.91 ms and 8.1 MB became 957 us and 5.1 MB, which is
-- within 1.5x of what aeson charges to parse the same 'Value' as a plain
-- @[[Int]]@ -- the floor this recursion can reach without replacing aeson's
-- element parser.
--
-- The order of the two failures differs at that row and only there: the
-- element parses now happen before the length check, so a row that is both
-- ragged /and/ badly typed reports the element rather than the length. Both
-- are still rejected, which is what
-- "Test.Invariant"'s @Malformed JSON must be rejected@ pins.
nestedParseJSON val =
  case sizeProof @cs of
    SizeNil -> V.singleton <$> parseJSON val
    SizeCons @_ @n @rest ->
      case sizeProof @rest of
        SizeNil -> do
          vals :: [Value] <- parseJSON val
          checkLength @n (length vals)
          V.fromList <$> traverse parseJSON vals
        SizeCons{} -> do
          vals :: [Value] <- parseJSON val
          checkLength @n (length vals)
          V.concat <$> traverse (nestedParseJSON @rest) vals

-- | The per-dimension length check both branches of 'nestedParseJSON' share:
-- @n@ elements at this axis, or a parse failure naming both counts.
checkLength :: forall n. GHC.KnownNat n => Int -> Parser ()
checkLength got
  | got == expected = pure ()
  | otherwise =
      fail $ "Grid: expected " ++ show expected ++ " elements, got " ++ show got
  where
    expected = fromIntegral (GHC.natVal (Proxy @n)) :: Int
