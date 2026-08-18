-- | Compile-fail harness: each case under @tests\/compile-fail@ is a
-- deliberately ill-typed use of the API, and 'assertCompileFails' shells out
-- to GHC to check that it is rejected with the expected diagnostic.
module Test.CompileFail
  ( compileFailTests
  ) where

import           Data.List             (isInfixOf)
import           System.Exit           (ExitCode (..))
import           System.Process        (readProcessWithExitCode)

import           Test.Tasty
import           Test.Tasty.HUnit

-- | @-isrc@ compiles straight from source, bypassing cabal's own flag
-- plumbing, so the extensions and plugins are spelled out explicitly here.
ghcFlags :: [String]
ghcFlags =
  [ "-fno-code"
  , "-isrc"
  , "-fplugin=GHC.TypeLits.Normalise"
  , "-fplugin=GHC.TypeLits.KnownNat.Solver"
  , "-XGHC2024"
  , "-XDefaultSignatures"
  , "-XFunctionalDependencies"
  , "-XPatternSynonyms"
  , "-XRequiredTypeArguments"
  , "-XTypeAbstractions"
  , "-XTypeFamilies"
  , "-XUndecidableInstances"
  , "-XUndecidableSuperClasses"
  , "-XViewPatterns"
  ]

-- | Requires @expectedSubstring@ in the diagnostic, not just any failure, so a
-- typo that breaks the snippet in an unrelated way cannot pass for the
-- precondition actually firing.
assertCompileFails :: FilePath -> String -> Assertion
assertCompileFails file expectedSubstring = do
  (code, _out, err) <-
    readProcessWithExitCode "ghc" (ghcFlags ++ ["tests/compile-fail/" ++ file]) ""
  case code of
    ExitSuccess ->
      assertFailure (file ++ " was expected to fail to compile, but it compiled cleanly")
    ExitFailure _ ->
      assertBool
        (file ++ ": compiler error did not mention " ++ show expectedSubstring ++ ":\n" ++ err)
        (expectedSubstring `isInfixOf` err)

compileFailTests :: TestTree
compileFailTests =
  testGroup
    "Misuse the compiler now rejects"
    [ testCase "takeGrid past the source length" $
      assertCompileFails "TakeGridTooBig.hs" "Cannot satisfy: 9 <= 3"
    , testCase "dropGrid past the source length" $
      assertCompileFails "DropGridTooBig.hs" "Cannot satisfy: 9 <= 3"
    , testCase "splitHigherDim remainder annotated as anything but x - y" $
      assertCompileFails "SplitHigherDimWrongRemainder.hs" "Couldn't match"
    , testCase "shrinkGrid with a window that does not fit" $
      assertCompileFails "ShrinkGridWindowTooBig.hs" "Cannot satisfy: 6 <= 4"
    ]
