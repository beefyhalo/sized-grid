# 7qbi: folding one axis away

Seven candidate implementations of an axis-removing fold, measured against
each other and against the two things a caller can write with the published
API today. It exists to answer `sized-grid-7qbi` -- what shape the loop should
be, what the result type should be, and what the empty axis means -- and it is
deliberately **not** a port of `Data.Grid.Sized.Internal.Grid.Axis` and not a
candidate to become one. One of the seven is the recommendation; the other six
exist so that the recommendation is a measurement rather than a preference.

Frozen at the state that produced the numbers in that issue. It is not in the
project's `cabal.project`, so `cabal build all` never sees it and CI never runs
it; it carries its own `cabal.project` naming `../..` instead. Nothing else in
the tree depends on it, so it can be deleted the day 7qbi's follow-ups land.

## Running it

```
cd spike/7qbi-foldaxis
cabal run 7qbi-foldaxis:bench:compare -- --csv results.csv
```

The binary checks all seven candidates against a coordinate-indexed reference
before it measures anything -- every axis of a 3x5 grid and a 2x3x4 cube,
boxed and unboxed, over `\acc x -> 2 * acc - x`, which is neither associative
nor commutative and so cannot be satisfied by a fold that runs the wrong way
along the axis or groups it differently -- and exits non-zero on the first
disagreement, so a candidate that is fast because it folds the wrong cells
fails rather than reports. The references are written with `indexGrid` and
`tabulateGrid` and share no code with any candidate, the way
`tests/Test/Axis.hs` does it.

`results-ghc9.12.3-aarch64-darwin-O1.csv` is a plain run.
`results-ghc9.12.3-aarch64-darwin-O2.csv` is the same run with

```
printf 'package 7qbi-foldaxis\n  ghc-options: -O2\n' > cabal.project.local
```

in place, which is what sized-grid-y99h says to do when an -O1/-O2 split is
suspected. Both were taken on an otherwise quiet machine. **Read both.** Three
of the candidates are 3-5x apart between the two levels and two are not, and
which candidate wins changes with the level -- so a single-level measurement
here would have picked the wrong loop.

The benchmark component carries no `-O2` of its own, on purpose: `grid-sized`'s
own `benchmark` stanza carries none either, so this component is compiled the
way a consumer's is, and the candidates are judged on the unfoldings the
library component actually exports. The candidate library, in contrast, carries
exactly the `ghc-options` of `grid-sized`'s `library` stanza.

## The candidates

All seven have the same type: a strict left fold along axis `n` returning a
grid with that axis removed, `GridOf v cs x -> GridOf v (DropAxis n cs) y`.

| name | shape |
| --- | --- |
| `foldAxisGenerate` | output-driven: `VG.generate`, one strided inner fold per output cell |
| `foldAxisWrite` | the same walk with `VG.generate` replaced by an explicit mutable write, to separate the walk's cost from the combinator's |
| `foldAxisTotal` | `foldAxisWrite` with the output length read off the result type instead of divided out -- the variant that survives an empty axis |
| `foldAxisNonEmpty` | `foldAxisWrite` with the empty axis excluded by an `IsCoordLifted c` constraint instead. **The recommendation.** |
| `foldAxisSweep` | input-driven: one pass up the input, partial results held in the mutable output |
| `foldAxisFibres` | `map (foldlGrid' f z) . axisFibres n`, repacked. What a caller writes today |
| `foldAxisSlices` | the innermost axis only: each fibre is a slice, so each fold is `VG.foldl'` over it |
| `foldAxisSplit` | axis 0 only: `foldl (zipWithGrid f)` over `splitGrid`. The other thing a caller writes today |

## What it found

Full numbers in the two CSVs; `sized-grid-7qbi` and
`docs/superpowers/specs/2026-08-29-axis-fold-design.md` carry the argument.
In one paragraph: the output-driven loop is the right one, it allocates
nothing per cell where the fibre-gathering form allocates 5.9 MB on a
90,000-cell grid, and on the contiguous axis it costs what the whole-grid
`foldlGrid'` costs -- which is the floor. It loses in exactly two places, both
recorded in the issue: the unboxed strided axis at -O1 only, where the
input-driven sweep is 2.3x faster and the gap closes entirely at -O2; and a
product-typed accumulator over an unboxed grid, where the sweep is 9.7x faster
because its accumulators live unboxed in the output vector instead of being
re-allocated per cell.
