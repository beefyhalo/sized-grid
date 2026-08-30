{-# LANGUAGE OverloadedStrings #-}

-- | What a consumer has to write, compiled the way a consumer compiles: no
-- @ghc-typelits-natnormalise@\/@ghc-typelits-knownnat@ plugins (@-fplugin@
-- is not transitive), so every signature here is a claim about what the
-- library asks of code outside it, checked by the fact that this module
-- compiles at all. The assertions in 'main' are almost beside the point;
-- the type checker is the test.
module Main
  ( main,
  )
where

import Control.Exception (SomeException, evaluate, try)
import Data.Aeson (decode, encode)
import Data.ByteString.Lazy (ByteString)
import Data.Grid.Sized
import Data.Maybe (fromJust)
import GHC.TypeLits (KnownNat)
import System.Exit (exitFailure)

parseSquare ::
  forall n.
  (KnownNat n) =>
  String ->
  Maybe (Grid '[Clamped n, Clamped n] Char)
parseSquare = gridFromList . lines

renderSquare ::
  forall n.
  (KnownNat n) =>
  Grid '[Clamped n, Clamped n] Char ->
  [String]
renderSquare = collapseGrid

roundTripJSON ::
  forall n.
  (KnownNat n) =>
  Grid '[Clamped n, Clamped n] Char ->
  Maybe (Grid '[Clamped n, Clamped n] Char)
roundTripJSON = decode . encode

columnSums :: Grid '[Clamped 3, Clamped 3] Int -> Grid '[Clamped 3, Clamped 3] Int
columnSums = scanAxis 0 (+)

rejectsRagged :: Bool
rejectsRagged =
  case decode ragged :: Maybe (Grid '[Clamped 3, Clamped 3] Int) of
    Nothing -> True
    Just _ -> False
  where
    ragged = "[[1,2,3],[4,5]]" :: ByteString

-- | sized-grid-sxy, sized-grid-adr.14. An out-of-range 'unsafeOrdinal' must
-- fail in a consumer, not index a vector out of bounds in silence. The check
-- that reaches here is the one baked into the library's interface file, so
-- this is the only place it can be tested from: a flag on this component
-- cannot put back a check the library compiled away.
--
-- It used to be easy to lose. The check was an assert kept alive by
-- @-fno-ignore-asserts@ on the library, and any optimisation flag written
-- after that one -- @-O2@ on the library stanza, or an @-O2@ appended by a
-- @package grid-sized@ stanza in someone's cabal.project -- re-enabled
-- @-fignore-asserts@ and took it away. Measured, not hypothetical.
-- sized-grid-adr.14 made it an ordinary guard instead, which no flag can
-- strip, and which is also the faster of the two. This check stays because it
-- tests the property rather than the mechanism.
checkSurvivesIntoConsumers :: IO Bool
checkSurvivesIntoConsumers = do
  r <- try (evaluate (ordinalToInt (unsafeOrdinal 99 :: Ordinal 5)))
  pure $
    case r :: Either SomeException Int of
      Left _ -> True
      Right _ -> False

main :: IO ()
main = do
  checkLives <- checkSurvivesIntoConsumers
  let rows = ["abc", "def", "ghi"]
      grid = fromJust (parseSquare (unlines rows)) :: Grid '[Clamped 3, Clamped 3] Char
      numbers =
        fromJust (gridFromList [[1, 2, 3], [4, 5, 6], [7, 8, 9]]) ::
          Grid '[Clamped 3, Clamped 3] Int
      checks =
        [ ("collapseGrid . gridFromList", renderSquare grid == rows),
          ("decode . encode", fmap renderSquare (roundTripJSON grid) == Just rows),
          ("ragged JSON rejected", rejectsRagged),
          ( "scanAxis 0 scans down each column",
            collapseGrid (columnSums numbers) == [[1, 2, 3], [5, 7, 9], [12, 15, 18]]
          ),
          ("unsafeOrdinal's range check survives into a consumer", checkLives)
        ]
  case [name | (name, ok) <- checks, not ok] of
    [] -> putStrLn ("downstream: " ++ show (length checks) ++ " checks passed")
    bad -> do
      mapM_ (putStrLn . ("downstream FAILED: " ++)) bad
      exitFailure
