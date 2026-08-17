-- | sized-grid-cti. Wave 02 (sized-grid-2h0) tightened 'Data.Grid.Sized.takeGrid',
-- 'Data.Grid.Sized.dropGrid' and 'Data.Grid.Sized.splitHigherDim' so that
-- misuse is a type error rather than a silently wrong 'Data.Grid.Sized.Grid'.
-- That is the right outcome, but it means the regression coverage for it
-- cannot live in the rest of this suite as an ordinary 'Test.Tasty.HUnit'
-- assertion: the misuse does not compile, so there is no value left at
-- runtime to assert against.
--
-- This module is the compile-fail harness instead. Each case under
-- @tests\/compile-fail@ is one deliberately ill-typed use of the tightened
-- API; 'assertCompileFails' shells out to GHC to compile it on its own and
-- checks that GHC rejects it with the diagnostic its precondition promises.
-- Compiling with @-isrc@ rather than against the built package avoids
-- depending on 'grid-sized' having been registered into a package database
-- first, so this runs the same way regardless of how the suite is invoked.
--
-- '-fdefer-type-errors' was tried and rejected before this: turning the
-- misuse into a runtime 'Control.Exception.TypeError' sounds like it would
-- let these live as ordinary assertions after all, but in practice GHC does
-- not scope the deferred error to the ill-typed subexpression. It was
-- observed here to swallow the entire enclosing top-level binding -- a
-- @main@ built this way ran none of its statements, not even the ones before
-- the bad one -- which is exactly the brittleness sized-grid-cti flagged
-- 'should-not-typecheck' for. Asking GHC to fail outright, and reading what
-- it says, is the more direct check of the two.
module Test.CompileFail
  ( compileFailTests
  ) where

import           Data.List             (isInfixOf)
import           System.Exit           (ExitCode (..))
import           System.Process        (readProcessWithExitCode)

import           Test.Tasty
import           Test.Tasty.HUnit

-- | The extensions and plugins the library itself is built with (the @lang@
-- common stanza in grid-sized.cabal), spelled out explicitly here: @-isrc@
-- compiles straight from source, bypassing cabal's own flag plumbing, so
-- nothing else supplies them.
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

-- | Compile a snippet under @tests\/compile-fail@ and assert that GHC rejects
-- it, with @expectedSubstring@ somewhere in the diagnostic -- not just that it
-- fails for any reason, so a typo that breaks the snippet in an unrelated way
-- cannot pass for the precondition actually firing.
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
