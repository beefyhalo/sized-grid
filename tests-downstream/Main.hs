{-# LANGUAGE OverloadedStrings #-}

-- | What a consumer has to write, compiled the way a consumer compiles: no
-- @ghc-typelits-natnormalise@\/@ghc-typelits-knownnat@ plugins (@-fplugin@
-- is not transitive), so every signature here is a claim about what the
-- library asks of code outside it, checked by the fact that this module
-- compiles at all. The assertions in 'main' are almost beside the point;
-- the type checker is the test.
module Main
  ( main
  ) where

import           Data.Grid.Sized

import           Control.Exception    (SomeException, evaluate, try)
import           Data.Aeson           (decode, encode)
import           Data.ByteString.Lazy (ByteString)
import           Data.Maybe           (fromJust)
import           GHC.TypeLits         (KnownNat)
import           System.Exit          (exitFailure)

parseSquare ::
     forall n. KnownNat n
  => String
  -> Maybe (Grid '[ Clamped n, Clamped n] Char)
parseSquare = gridFromList . lines

renderSquare ::
     forall n. KnownNat n
  => Grid '[ Clamped n, Clamped n] Char
  -> [String]
renderSquare = collapseGrid

roundTripJSON ::
     forall n. KnownNat n
  => Grid '[ Clamped n, Clamped n] Char
  -> Maybe (Grid '[ Clamped n, Clamped n] Char)
roundTripJSON = decode . encode

columnSums :: Grid '[ Clamped 3, Clamped 3] Int -> Grid '[ Clamped 3, Clamped 3] Int
columnSums = scanAxis 0 (+)

rejectsRagged :: Bool
rejectsRagged =
  case decode ragged :: Maybe (Grid '[ Clamped 3, Clamped 3] Int) of
    Nothing -> True
    Just _  -> False
  where
    ragged = "[[1,2,3],[4,5]]" :: ByteString

-- | sized-grid-sxy. 'unsafeOrdinal' states its precondition with an assert
-- that the library keeps alive for every consumer by compiling itself with
-- @-fno-ignore-asserts@ (see the note in the .cabal). The check that reaches
-- here is the one baked into the interface file, so this is the only place it
-- can be tested from: a flag on this component cannot put it back.
--
-- It is easy to lose. Any optimisation flag written after
-- @-fno-ignore-asserts@ -- @-O2@ on the library stanza, or an @-O2@ appended
-- by a @package grid-sized@ stanza in someone's cabal.project -- re-enables
-- @-fignore-asserts@, and out-of-range Ordinals start indexing vectors out of
-- bounds in silence. Measured, not hypothetical.
assertSurvivesIntoConsumers :: IO Bool
assertSurvivesIntoConsumers = do
  r <- try (evaluate (ordinalToInt (unsafeOrdinal 99 :: Ordinal 5)))
  pure $
    case r :: Either SomeException Int of
      Left _  -> True
      Right _ -> False

main :: IO ()
main = do
  assertLives <- assertSurvivesIntoConsumers
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
        , ("unsafeOrdinal's assert survives into a consumer", assertLives)
        ]
  case [name | (name, ok) <- checks, not ok] of
    [] -> putStrLn ("downstream: " ++ show (length checks) ++ " checks passed")
    bad -> do
      mapM_ (putStrLn . ("downstream FAILED: " ++)) bad
      exitFailure
