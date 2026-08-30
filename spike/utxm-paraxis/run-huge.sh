#!/bin/sh
# The out-of-cache shape, measured on its own. Its groups are self-contained,
# so every ratio quoted from them is still between two arms in one process --
# which is the only kind this project quotes. See the README.
set -e
BIN=./dist-newstyle/build/aarch64-osx/ghc-9.12.3/utxm-paraxis-0/b/compare/build/compare/compare
for n in 1 2 4 8; do
  echo "=== -N$n ==="
  "$BIN" --time-mode wall --stdev 5 -p '/Huge/' \
    --csv "results-huge-ghc9.12.3-aarch64-darwin-N$n.csv" +RTS "-N$n"
done
