#!/bin/sh
# The four runs the checked-in CSVs come from. Wall clock is not optional --
# see the README and bench/README.md in the library.
set -e
BIN=./dist-newstyle/build/aarch64-osx/ghc-9.12.3/utxm-paraxis-0/b/compare/build/compare/compare
for n in 1 2 4 8; do
  echo "=== -N$n ==="
  "$BIN" --time-mode wall --stdev 5 \
    --csv "results-ghc9.12.3-aarch64-darwin-N$n.csv" +RTS "-N$n"
done
