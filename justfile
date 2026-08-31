# sized-grid task runner. `just` = named recipes, run from any subdir.
# Toolchain (ghc, cabal, ormolu, hlint) comes from the flake devShell via direnv.

# list recipes
default:
    @just --list

# build every package
build *ARGS:
    cabal build all {{ARGS}}

# run every test suite
test *ARGS:
    cabal test all {{ARGS}}

# run benchmarks
bench *ARGS:
    cabal bench all {{ARGS}}

# GHCi for one package (default: the root library)
repl pkg="grid-sized":
    cabal repl {{pkg}}

# format all tracked Haskell sources in place (same ormolu as the pre-commit hook)
fmt:
    fd -e hs -X ormolu --mode inplace

# lint (reads .hlint.yaml at the repo root)
lint:
    hlint .

# the full pre-commit gate: hlint + ormolu + build
check:
    nix flake check

# re-run the tests on every Haskell change
watch *ARGS:
    watchexec -e hs -- just test {{ARGS}}

# drop cabal build artifacts
clean:
    cabal clean

# pack the tree into repomix-output.xml for external review tools
repomix:
    repomix
