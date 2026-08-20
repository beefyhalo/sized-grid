# adr.8: `Coord` as a flat `Int`

A throwaway grid whose coordinate *is* its row-major position, measured
against the real one. It exists to answer `sized-grid-adr.8` -- what would the
other end of the design space buy? -- and it is deliberately **not** a port of
this library and not a candidate to become one.

Frozen at the state that produced the numbers in that issue. It is not in the
project's `cabal.project`, so `cabal build all` never sees it and CI never runs
it; it carries its own `cabal.project` naming `../..` instead. Nothing else in
the tree depends on it, so it can be deleted the day adr.8 is decided.

## Running it

```
cd spike/adr8-intcoord
cabal bench adr8-intcoord:bench:compare
```

The binary checks the two implementations agree on fourteen properties before
it measures anything, so a change that makes the spike faster by making it
wrong fails rather than reports.

`results-ghc9.12.3-aarch64-darwin-run{1,2}.csv` are the two back-to-back runs
the issue's numbers come from.

## What is in it, and what is deliberately matched

`src/IntCoord.hs` is the experiment: `newtype Coord cs = Coord Int`, per-axis
operations by `quotRem`, and a `Shape` class standing in for `IsCoordList`.
Several things about it are copied rather than invented, so that the
comparison is of representations and not of two people's arithmetic:

* the axis policies are transliterated from `Clamped`'s and `Periodic`'s own
  instances -- clamp by comparison rather than add-then-clamp, reduce the
  displacement before adding, distance the short way round on a torus;
* `axisStepsI` is `axisSteps` with its dedup filter intact;
* the fold over the axis list is a class method, for the reason recorded on
  `IsCoordList`: a self-recursive fold cannot unroll;
* every axis value the spike constructs is range-checked, mirroring
  `unsafeOrdinal`'s guard (sized-grid-adr.14), because otherwise the spike
  would be measuring a cheaper invariant as well as a different
  representation. It costs real time -- see the issue.

`adr8-intcoord.cabal` puts the experiment in its own component with the same
`ghc-options` as `grid-sized`'s `library` stanza, so it reaches the benchmark
through a `.hi` file exactly as the control does. Compiling it into
`bench/Main.hs` would let GHC see the whole thing at once and flatter it.

`bench/Main.hs` holds both sides in adjacent pairs -- bodies copied from
`bench/Main.hs` in the library -- so that machine noise, which follows the
clock rather than the code, hits both halves of every ratio. It also keeps
three raw-vector controls that have nothing to do with coordinates; they are
what identified the harness artifact recorded in the issue, where the shape of
a `nf` argument, not the representation, moved a benchmark by 1.6x.
