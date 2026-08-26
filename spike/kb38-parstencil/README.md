# kb38: building a stencil table on more than one core

Seven candidate build paths for a `Stencil` table, measured against the
library's own `stencilFor`. It exists to answer `sized-grid-kb38` -- is the
table build worth parallelising, and above what size -- and it is deliberately
**not** a port of `Data.Grid.Sized.Stencil` and not a candidate to become one.

Frozen at the state that produced the numbers in that issue. It is not in the
project's `cabal.project`, so `cabal build all` never sees it and CI never runs
it; it carries its own `cabal.project` naming `../..` instead. Nothing else in
the tree depends on it, so it can be deleted the day kb38's follow-ups land.

## Running it

```
cd spike/kb38-parstencil
cabal bench kb38-parstencil:bench:compare \
  --benchmark-options="--time-mode wall" -- +RTS -N4
```

`--time-mode wall` is not optional. **tasty-bench measures CPU time by
default**, which its own Haddock notes is "total elapsed CPU time across all
cores" for a multithreaded algorithm -- so it cannot see a parallel speedup by
construction. The first pass at this spike ran the whole matrix in the default
mode and concluded that parallelism did nothing: every parallel variant came
back within noise of its sequential twin at `-N4`, and *worse* at `-N8`. What
gave it away was `+RTS -s` on the same run reporting 3.0s of mutator time
against 1.25s elapsed. The work was on four cores the whole time; the harness
was reporting the sum of it. Anything measuring parallelism in this project
has to pass this flag or use another clock.

The binary checks all seven variants against the library's `stencilFor` before
it measures anything -- eleven combinations of shape and neighbourhood, over
`Clamped`, `Periodic` and `Reflective` axes, including an asymmetric kernel and
the empty neighbourhood -- and exits non-zero on the first disagreement, so a
variant that is fast because it is wrong fails rather than reports.

`results-ghc9.12.3-aarch64-darwin-N{1,2,4,8}.csv` are the four runs the issue's
numbers come from, in that order, on an otherwise quiet 10-core machine.

## The two axes of the experiment

They are separable, and it turned out to matter a great deal that they are.

**How many passes.** The library asks the neighbourhood function twice per
cell: once for a length, to discover the width, and once for the row. That is
deliberate (`sized-grid-adr.15`) -- the one-pass alternative it replaced held
every row live across a width pass. But a caller who can *bound* the width can
lay the table out at the bound in one pass, record the width it actually saw,
and compact if the bound was loose. Nothing is retained, and the neighbourhood
function runs once. `mooreStencil` and `vonNeumannStencil` can supply such a
bound -- `(2r+1)^d - 1` -- though the general `stencilFor` cannot.

**How many cores.** Every cell's row is independent of every other's, so both
the width pass and the fill split by index range with no communication at all.
The write target is one mutable vector whose chunks are disjoint.

`tableSeqBounded` is the first without the second, `tableParBoth` is the second
without the first, `tableParBounded` is both, and `tableParWidth` /
`tableParFill` parallelise one pass each so the credit can be assigned.
`tableParChunked` is a one-pass variant needing *no* bound -- each chunk lays
out at its own width and a second pass copies the chunks into the widest of
them -- which is the general-purpose answer, and loses everywhere.

## What the numbers said

Best wall-clock time at any `-N`, against the library at `-N1`:

| variant | 400 cells | 2,500 | 90,000 | 90,000, moore 3 |
|---|---|---|---|---|
| `seqBounded` | 1.93x | 1.90x | 1.89x | 1.94x |
| `parWidth` | 1.32x | 1.41x | 1.45x | 1.53x |
| `parFill` | 1.48x | 1.63x | 1.58x | 1.75x |
| `parBoth` | 2.37x | 2.84x | 2.88x | 3.11x |
| `parBounded` | 4.14x | 4.96x | 4.89x | 5.42x |
| `parChunked` | 1.53x | 1.24x | 0.97x | 1.06x |

Four things are worth taking away from that table, and only one of them is
about parallelism.

**The one-pass bounded build is a free 1.9x, on one core.** It is the largest
single win here that does not cost a thread, an `unsafePerformIO`, or a
threshold, and it halves allocation as well -- 535 MB to 260 MB on the 90,000
cell Moore build. `sized-grid-adr.15` chose the two-pass build against a
one-pass build that *retained*; it did not consider one that allocates at a
bound, and the note it left says the width is discovered rather than derived
because a derived bound would over-pad every row. That is true of a derived
*final* width and not of a derived *allocation* size, which is what this is.

**Both passes have to be parallel or neither is worth much.** Half of a
two-pass build is still a full serial pass: `parWidth` and `parFill` cap out
at 1.4x and 1.6x, which is roughly what Amdahl predicts, and they only reach
2.9x together.

**Past four cores the returns stop.** `parBounded` on the 90,000-cell Moore
build runs 29.9 / 16.2 / 11.7 / 14.6 ms at `-N1/2/4/8`: near-linear to two,
2.6x at four, and *slower* at eight. The build is allocation-bound -- it is
still consing a list per cell out of the neighbourhood function -- so past a
few cores it is competing for memory bandwidth and GC rather than for
arithmetic.

**Turning `-N` on costs the sequential path.** The library's own `stencilFor`
measures 57.0 ms at `-N1`, 61.2 ms at `-N4` and 78.9 ms at `-N8` on the same
machine, unchanged code: parallel GC on a single-threaded mutator. Any
threshold that falls back to the sequential path on a consumer who has asked
for `-N` is falling back to a path that `-N` has already made slower.

## Where the crossover is

The parallel dispatch costs about 20-30 μs, all of it forking and joining, and
it does not vary with the chunk count -- one chunk per capability and eight
per capability measure the same to within 2% on the 90,000-cell build, so
there is no load-balancing win to chase from a static split.

That overhead sets the threshold, and the threshold is low:

| cells | `seqBounded` at `-N1` | `parBounded` at `-N4` |
|---|---|---|
| 12 | 3.7 μs | 11.8 μs |
| 100 | 29.7 μs | 21.3 μs |
| 400 | 123.0 μs | 57.5 μs |
| 1,024 | 319.9 μs | 131.5 μs |

Break-even is around 100 cells and the win is unambiguous by 400. A threshold
of 1,024 cells has an order of magnitude of margin on both sides and is a
`>= 1024` on a number the type already knows.

## The memory tradeoff

Allocation per build is the CSVs' own column; peak residency needs a run of
one benchmark on its own, because `+RTS -s` reports the high-water mark of the
whole process and the correctness checks build tables of their own:

```
KB38_SKIP_CHECKS=1 ./compare --time-mode wall \
  -p '/Big), vonNeumann 2.tableSeqBounded/' +RTS -N4 -s
```

On the 90,000-cell grid:

| | allocated per build | peak residency |
|---|---|---|
| library, Moore r=1 | 535 MB | 6.02 MB |
| bounded, Moore r=1 | 260 MB | 6.02 MB |
| library, von Neumann r=2 | 833 MB | 8.90 MB |
| bounded, von Neumann r=2 | 426 MB | 17.5 MB |

The bounded variants halve allocation in every case, because they run the
neighbourhood function once instead of twice. What they trade for it is peak
residency, and only when the bound is loose: Moore's bound is exact, so the
table is laid out at the size it ends up and residency is unchanged to three
digits, while von Neumann at radius 2 has a bound of 24 against a width of 12
and so briefly holds a table of twice the final size. The worst case is
`n * ub` machine words, and it is bounded, predictable from the type and the
radius, and freed as soon as the compacting copy is done.

Parallelism on its own changes neither column: `parBoth` allocates what the
two-pass build allocates less the padded list it no longer conses -- 441 MB
against 535 MB -- and its residency matches the library's to three digits. It
is the *bound*, not the threads, that both halves allocation and puts a
ceiling on residency.

`parChunked`, the variant that needs no bound, is the one with a memory
problem: every chunk's table is live when the final one is allocated, and it
copies 122 MB per build where nothing else copies more than 600 KB. It is
slower than the library it replaces on the three largest shapes. Recorded here
so that nobody reaches for it again.

## What is deliberately matched

`src/ParStencil.hs` is in its own component with the same `ghc-options` as
`grid-sized`'s `library` stanza, so the experiments reach the benchmark through
a `.hi` file exactly as the control does. Compiling them into `bench/Main.hs`
would let GHC see the whole thing at once and flatter them.

The variants produce a `Table` -- a strict pair of a width and a vector --
rather than a `Stencil`, because `Stencil` is abstract and has no constructor.
Its two accessors are what the comparison runs against, which is the whole of
its observable content.

`tableSeq` is a transliteration of the library's build and is *not* the
control; the control is `stencilFor` itself. It is there so that the parallel
variants differ from something in the same module by exactly the parallelism,
and so that the benchmark can show it and the real one measuring the same --
which it does, to within 1% at every size.
