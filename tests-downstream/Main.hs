{-# LANGUAGE OverloadedStrings #-}

-- | What a consumer has to write, compiled the way a consumer compiles.
--
-- This module is built without @ghc-typelits-natnormalise@ and
-- @ghc-typelits-knownnat@ -- see the @downstream@ stanza in @grid-sized.cabal@
-- -- because @-fplugin@ is not transitive and so no consumer has them. Every
-- signature below is therefore a claim about what the library asks of code
-- outside it, and the claim is checked by the fact that this module compiles at
-- all. The assertions in 'main' are almost beside the point; the type checker
-- is the test.
--
-- sized-grid-k6n. Until 'AllGridSizeKnown' became a class it was a type family,
-- which does no solving: the @KnownNat (MaxCoordSize cs)@ in its expansion
-- landed unsolved in the caller's context, and at @'[Clamped n, Clamped n]@
-- that reads @KnownNat (n * (n * 1))@. GHC cannot derive that from @KnownNat n@
-- unaided, so five signatures in the one downstream package spelled it out:
--
-- > parse :: (KnownNat n, KnownNat (n * n)) => String -> Maybe (Grid '[Clamped n, Clamped n] Cell)
--
-- If any signature here ever needs a @KnownNat@ over a product again, the leak
-- is back.
module Main
  ( main
  ) where

import           Data.Grid.Sized

import           Data.Aeson           (decode, encode)
import           Data.ByteString.Lazy (ByteString)
import           Data.Maybe           (fromJust)
import           GHC.TypeLits         (KnownNat)
import           System.Exit          (exitFailure)

-- | ../aoc/src/2025/04.hs and four siblings, with the constraint they used to
-- carry deleted. @KnownNat n@ is the whole context.
parseSquare ::
     forall n. KnownNat n
  => String
  -> Maybe (Grid '[ Clamped n, Clamped n] Char)
parseSquare = gridFromList . lines

-- | The other direction over the same axis list.
renderSquare ::
     forall n. KnownNat n
  => Grid '[ Clamped n, Clamped n] Char
  -> [String]
renderSquare = collapseGrid

-- | Both JSON instances were constrained by the same type family, so both
-- leaked the same product. Neither does now.
roundTripJSON ::
     forall n. KnownNat n
  => Grid '[ Clamped n, Clamped n] Char
  -> Maybe (Grid '[ Clamped n, Clamped n] Char)
roundTripJSON = decode . encode

-- | sized-grid-e6h. 'mapAxis' and 'scanAxis' resolve their axis-position
-- class purely through concrete-literal instance selection -- no @KnownNat@
-- product, so no plugin should be needed here either.
columnSums :: Grid '[ Clamped 3, Clamped 3] Int -> Grid '[ Clamped 3, Clamped 3] Int
columnSums = scanAxis 0 (+)

-- | A ragged decode must still be rejected: dropping the constraint from the
-- consumer must not have dropped the length check that constraint pays for.
rejectsRagged :: Bool
rejectsRagged =
  case decode ragged :: Maybe (Grid '[ Clamped 3, Clamped 3] Int) of
    Nothing -> True
    Just _  -> False
  where
    ragged = "[[1,2,3],[4,5]]" :: ByteString

main :: IO ()
main = do
  let rows = ["abc", "def", "ghi"]
      grid = fromJust (parseSquare (unlines rows)) :: Grid '[ Clamped 3, Clamped 3] Char
      numbers = fromJust (gridFromList [[1, 2, 3], [4, 5, 6], [7, 8, 9]]) ::
                  Grid '[ Clamped 3, Clamped 3] Int
      checks =
        [ ("collapseGrid . gridFromList", renderSquare grid == rows)
        , ("decode . encode", fmap renderSquare (roundTripJSON grid) == Just rows)
        , ("ragged JSON rejected", rejectsRagged)
        , ( "scanAxis 0 scans down each column"
          , collapseGrid (columnSums numbers) == [[1, 2, 3], [5, 7, 9], [12, 15, 18]])
        ]
  case [name | (name, ok) <- checks, not ok] of
    [] -> putStrLn ("downstream: " ++ show (length checks) ++ " checks passed")
    bad -> do
      mapM_ (putStrLn . ("downstream FAILED: " ++)) bad
      exitFailure
