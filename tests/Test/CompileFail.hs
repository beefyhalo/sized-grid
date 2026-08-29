-- | Compile-fail harness: each case under @tests\/compile-fail@ is a
-- deliberately ill-typed use of the API, and 'assertCompileFails' shells out
-- to GHC to check that it is rejected with the expected diagnostic.
module Test.CompileFail
  ( compileFailTests
  ) where

import           Control.Exception     (IOException, try)
import           Data.List             (isInfixOf)
import           System.Exit           (ExitCode (..))
import           System.Process        (readProcessWithExitCode)

import           Test.Tasty
import           Test.Tasty.HUnit

-- | @-isrc@ compiles straight from source, bypassing cabal's own flag
-- plumbing, so the extensions and plugins are spelled out explicitly here.
--
-- The invocation goes through @cabal exec ghc --@ rather than a bare @ghc@:
-- a bare @ghc@ only sees the ambient package environment (whatever's on
-- PATH), which does not necessarily include every one of the library's
-- build-depends -- 'groups' (sized-grid-yyq) was on the library's
-- build-depends but missing from the ambient env, so every snippet failed
-- with a "Could not find module" error instead of the diagnostic under
-- test. @cabal exec@ resolves the same package set cabal itself gives the
-- library, so this can't drift from build-depends again.
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

-- | @cabal exec@ re-solves rather than reading back the plan that built this
-- test binary, so it is told to solve for the same one: without
-- @--enable-tests --enable-benchmarks@ the test-suite and benchmark stanzas
-- are absent from the solve, and the solver is free to pick a dependency
-- version the plan that built us did not (sized-grid-1zkr). Whatever it picks
-- that way is not in the store, so `cabal exec` leaves it out of the
-- environment file it writes and GHC reports the installed one as a /hidden/
-- package -- on CI, every snippet failed on @Could not load module
-- \'Data.Aeson\'. It is a member of the hidden package \'aeson-2.2.5.0\'@
-- rather than on the diagnostic under test. A machine whose store happens to
-- hold both versions never sees this, which is why it showed up on CI first.
cabalFlags :: [String]
cabalFlags = ["--enable-tests", "--enable-benchmarks"]

-- | Requires @expectedSubstring@ in the diagnostic, not just any failure, so a
-- typo that breaks the snippet in an unrelated way cannot pass for the
-- precondition actually firing.
--
-- Tries @cabal exec ghc --@ first, which resolves the same package set cabal
-- gives the library. Nix's sandboxed check build has no @cabal@ on PATH at
-- all (sized-grid-jz3), so on a spawn failure this falls back to a bare
-- @ghc@ -- inside that sandbox GHC_PACKAGE_PATH is already pinned to the
-- derivation's exact package set, so it sees the same packages either way.
assertCompileFails :: FilePath -> String -> Assertion
assertCompileFails file expectedSubstring = do
  let args =
        ["exec"] ++ cabalFlags ++ ["ghc", "--"] ++ ghcFlags
          ++ ["tests/compile-fail/" ++ file]
  cabalResult <- try (readProcessWithExitCode "cabal" args "")
  (code, _out, err) <- case cabalResult of
    Right result -> pure result
    Left (_ :: IOException) ->
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
      -- sized-grid-mbh0: a restriction destroys the boundary policy, so a
      -- window is Ordinal-axed whatever the source's axis type was. The
      -- substring names both halves of the mismatch, because a regression
      -- that made the window's axis free again would let the snippet compile
      -- rather than fail differently.
    , testCase "a window annotated with the source's boundary policy" $
      assertCompileFails
        "WindowKeepsSourcePolicy.hs"
        "Couldn't match type: Ordinal 3"
      -- sized-grid-pnws: the same rule over the narrowing half of the shape
      -- algebra. takeGrid stands in for dropGrid, sliceGrid and
      -- splitHigherDim, which share its result type by construction.
    , testCase "takeGrid annotated with the source's boundary policy" $
      assertCompileFails
        "RestrictionKeepsSourcePolicy.hs"
        "Couldn't match type: Ordinal 3"
    , testCase "walkPathTotal on a coord with a walled axis" $
      assertCompileFails
        "WalkPathTotalNotBoundaryless.hs"
        "No instance for \8216Boundaryless (Clamped 5)\8217"
      -- sized-grid-adr.16: a 'Coord' is now a bare 'Int', so nothing in its
      -- representation mentions @cs@ and the role annotation is the only thing
      -- keeping @coerce@ from forging an out-of-range coordinate. The
      -- substring is deliberately the unquoted half of the diagnostic: the
      -- snippet has exactly one way to fail, and a role that regressed to
      -- phantom would compile cleanly rather than fail differently.
    , testCase "coerce a Coord between axis sizes (the nominal role)" $
      assertCompileFails "CoordCoerceAcrossSizes.hs" "Couldn't match type"
    ]
