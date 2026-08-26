# 49hi: running a stencil on more than one core

Five candidate execution paths for each of the library's two stencil kernels,
measured against the library's own `stencilGrid` and `stencilFoldGrid`. It
exists to answer `sized-grid-49hi` -- is applying a stencil worth
parallelising, and above what size -- and it is deliberately **not** a port of
`Data.Grid.Sized.Stencil` and not a candidate to become one.

Building the table is the sibling question `sized-grid-kb38`, answered in
`spike/kb38-parstencil`. Nothing here builds a table inside a benchmark: every
`Stencil` is a top-level CAF, which is what a consumer does and what makes
these numbers about the fill alone.

Frozen at the state that produced the numbers in that issue. It is not in the
project's `cabal.project`, so `cabal build all` never sees it and CI never runs
it; it carries its own `cabal.project` naming `../..` instead. Nothing else in
the tree depends on it, so it can be deleted the day 49hi's follow-ups land.

## Running it

```
cd spike/49hi-parexec
cabal bench 49hi-parexec:bench:compare \
  --benchmark-options="--time-mode wall" -- +RTS -N4
```

`sh run.sh` is the four runs the checked-in CSVs come from, in order.

`--time-mode wall` is not optional, for the reason `spike/kb38-parstencil`
records at length and `bench/README.md` now warns about: **tasty-bench
measures CPU time by default**, which its own Haddock notes is "total elapsed
CPU time across all cores" for a multithreaded algorithm, so it cannot see a
parallel speedup by construction.

The binary checks all ten variants against the library before it measures
anything -- both vector representations, both boundary policies, sizes from
fewer cells than there are cores to more cells than there are chunks, and an
order-sensitive rule as well as a neighbour sum -- and exits non-zero on the
first disagreement, so a variant that is fast because it scrambled a row fails
rather than reports. The order-sensitive rule is the one that earns its place:
a neighbour sum is commutative and associative and so cannot tell a chunked
fill that reordered a row from one that did not.

`results-ghc9.12.3-aarch64-darwin-N{1,2,4,8}.csv` are the four runs the
issue's numbers come from, in that order, on an otherwise quiet machine.

## The three axes of the experiment

kb38 had two axes and found it mattered that they were separable. This has
three, and the third -- which has no parallelism in it at all -- turned out to
be worth more than the other two together on one of the two kernels.

**How it is inlined.** The library marks both kernels `INLINABLE`. That
exposes an unfolding for GHC to *specialise on the types*; it does not oblige
GHC to *inline the body*, and for a body with fork/join scaffolding in it GHC
declines. When it declines, the rule stays a lambda-bound variable inside the
fill, so `stencilFoldGrid`'s accumulator is a boxed `Int` that `step` re-boxes
once per neighbour, and the neighbour it folds in is boxed on the way out of
the unboxed vector as well. The tell is in the Core: the inner loop reads

```
$wgo3 ww11 acc = case acc of acc1 { I# ipv7 -> ... step acc1 (I# ds8) ... }
```

where a specialised one reads `jump $wgo1 (+# ww8 1#) (+# ww9 ds5)`. `INLINE`
makes the rule known where the loop is built, and the whole row folds in
registers.

**How strict it is.** The library fills the result with
`Data.Vector.Generic.generate`, which on a boxed vector writes a thunk per
cell. On an unboxed grid that is already the same as forcing; on a boxed one
it defers every rule application to whoever forces the grid, and an `iterate`
loop defers them across generations. Writing `$!` into a mutable vector forces
each cell where it is computed. No threads are involved in that either.

**How many cores.** Cell `i` of the result reads the old grid and the table
and writes nothing but slot `i`, so the fill splits by index range with no
communication at all: disjoint slices of one mutable vector, joined before it
is frozen.

Crossing them gives the five arms each kernel is measured at. Read left to
right, each adds exactly one thing to the one before it: `Seq` is the library
restated, `SeqInline` changes only the pragma, `SeqStrict` adds the strict
fill, `Par` adds the cores, and `ParSpark` trades the `unsafePerformIO` for a
copy.

`ParSpark` is there because the API decision has to see the price of purity.
The `Par` variants are pure functions with `unsafePerformIO` inside, on the
licence kb38's build variants took -- workers write disjoint slices of a
vector nothing else can see, join before it is frozen, and re-raise the first
exception any of them raised, so the result is a function of the arguments
alone. `ParSpark` expresses the same split with `par` over per-chunk vectors
and needs no such licence, paying an extra `n`-element allocation and copy for
it.

## How to read the tables

**Every ratio quoted below is between two arms measured in the same process.**
Each `-N` column is a separate run of the binary, and absolute times drift
between runs on this machine by more than any effect measured here: running
the whole matrix twice, half an hour apart, moved the peak parallel speedup on
90,000 unboxed cells from 2.59x to 1.88x. The crossovers agreed to within one
size step across both runs and the sequential findings agreed to within a few
percent; the peak speedups did not. So read the crossovers as findings and the
peak numbers as one machine on one morning.

Thermal state moves absolute times on an Apple laptop far more than a pragma
does. Within one process every arm is measured back to back under the same
conditions, so a column is trustworthy and a row is not.

## What the numbers said

### The largest win is sequential, and it is a pragma

90,000 unboxed cells, `-N1`, one process:

| arm | time | allocated |
|---|---|---|
| `stencilFoldGrid` (library) | 780 μs | 13 MB |
| `foldSeq` (the same body, `INLINABLE`) | 783 μs | 13 MB |
| `foldSeqInline` (the same body, `INLINE`) | 448 μs | 703 KB |
| `foldSeqStrict` (and a strict fill) | 444 μs | 703 KB |

`foldSeq` is a transliteration of the library and measures as one, which is
what licenses reading the next row as the pragma and nothing else: **1.74x
faster and 19x less allocated, on one core, for a one-word diff.**

That is not a claim about a spike. Changing `stencilGrid` and
`stencilFoldGrid` in the library itself from `INLINABLE` to `INLINE`, and
rebuilding nothing else, moves the library's own arm to exactly where the
spike's arm is:

| arm | before | after |
|---|---|---|
| `stencilFoldGrid` (library) | 1.03 ms / 13 MB | 430 μs / 703 KB |
| `stencilGrid` (library) | 6.28 ms / 92 MB | 4.47 ms / 82 MB |

with `foldSeqInline` alongside at 431 μs / 703 KB in the same process. (Those
two rows are from a separate pair of runs, so compare them with each other and
not with the table above.) The library edit was reverted; it belongs to a
follow-up, not to a spike.

The gather kernel gains much less from the pragma than the fold kernel does,
and for a legible reason: `stencilGrid` builds a `[a]` per cell, so a boxed
accumulator is a small part of what it was already allocating.
`stencilFoldGrid` exists precisely so as not to build that list, and boxing was
most of what it had left.

### Strictness pays on boxed grids, and only there

90,000 cells, `-N1`, one process per column:

| arm | boxed | unboxed |
|---|---|---|
| `stencilFoldGrid` (library) | 4.74 ms | 780 μs |
| `foldSeqInline` | 3.16 ms | 448 μs |
| `foldSeqStrict` | 1.47 ms | 444 μs |

On an unboxed grid `VG.generate` already forces, so the strict fill is worth
about 1%. On a boxed one it is worth another 2.1x on top of the pragma,
because the result vector stops being a vector of thunks.

The `iterate` loop is where that compounds, and it is the shape an automaton
actually runs. 2,500 boxed cells, 100 generations, `-N1`:

| arm | time | allocated |
|---|---|---|
| `stencilFoldGrid` (library) | 18.1 ms | 21 MB |
| `foldSeqInline` | 18.6 ms | 21 MB |
| `foldSeqStrict` | 3.58 ms | 5.7 MB |
| `stencilGrid` (library) | 34.4 ms | 237 MB |
| `gridSeqInline` | 32.7 ms | 210 MB |
| `gridSeqStrict` | 15.0 ms | 195 MB |

**5.1x on the fold kernel and 2.3x on the gather kernel, on one core, from
forcing each cell where it is written.** Note that the pragma does nothing at
all here on its own -- `foldSeqInline` is level with the library -- because
across 100 generations the thunk chain is the whole cost and only strictness
touches it. The two sequential axes do not overlap: one is worth having on a
single pass, the other on a loop.

### The two kernels have crossovers an order of magnitude apart

The parallel arm against the best sequential arm, both measured in the same
process, at `-N4` -- the setting that suits this machine:

| cells | gather (`gridPar`) | fold (`foldPar`) |
|---|---|---|
| 12 | 0.05x | 0.01x |
| 400 | 0.79x | 0.37x |
| 1,024 | 0.99x | 0.51x |
| 2,500 boxed | 1.11x | 0.58x |
| 2,500 unboxed | 1.55x | 0.71x |
| 10,000 boxed | 1.37x | 0.58x |
| 10,000 unboxed | 1.77x | **1.48x** |
| 40,000 unboxed | 1.67x | 1.89x |
| 90,000 boxed | 1.21x | 0.75x |
| 90,000 unboxed | 1.72x | 1.88x |

**The gather kernel starts paying at about 1,000 cells. The fold kernel does
not start paying until somewhere between 2,500 and 10,000, and only on an
unboxed grid; on a boxed one it never does** -- every boxed row above is a
loss, and its best showing anywhere is 1.08x at `-N1`, which is not a parallel
win at all.

The two facts have one cause, and it is the same cause as the pragma result.
The fold kernel is about ten times cheaper per cell than the gather kernel --
that is what it is for -- so the same fixed dispatch cost needs about ten times
as many cells to amortise. Making the sequential path faster moves the
crossover up, which is the trap in measuring a threshold before doing the free
optimisations: against the library as it stands, the parallel fold looks like
it pays from about 2,500 cells; against a fixed one it does not.

Boxed grids fall further behind still because they are allocation-bound, which
is kb38's finding transferred intact: past two cores a parallel fold over a
boxed grid is competing for memory bandwidth and GC rather than for arithmetic.

kb38's other transferable finding shows up plainly in these runs as well:
**turning `-N` on costs the sequential path.** Unchanged library code,
90,000 boxed cells, `stencilGrid`: 8.6 ms at `-N1` against 10.4 ms at `-N2`
and 10.9 ms at `-N8`. A threshold that falls back to the sequential path on a
consumer who asked for `-N` is falling back to a path `-N` has already made
slower.

### What the dispatch costs

From 12 cells, where the work is nothing and the measurement is the fork/join:

| | `-N1` | `-N2` | `-N4` | `-N8` |
|---|---|---|---|---|
| `foldPar` | 399 ns | 5.70 μs | 9.52 μs | 26.7 μs |
| `gridPar` | 753 ns | 5.20 μs | 10.5 μs | 25.5 μs |

At two chunks per capability that is 4, 8 and 16 forked workers, so **roughly
1.3--1.7 μs per chunk**, scaling with the chunk count rather than being a
fixed cost per call.

That does *not* contradict kb38's "static chunk count did not matter": at
90,000 cells, one, two and eight chunks per capability measure within 3% of
each other, exactly as kb38 found for the build.

| chunks/capability | `gridPar` `-N4` | `foldPar` `-N4` |
|---|---|---|
| 1 | 3.18 ms | 222 μs |
| 2 | 3.12 ms | 220 μs |
| 8 | 3.15 ms | 226 μs |

Both are true: the chunk count sets the dispatch cost, and the dispatch cost
stops mattering once there is enough work to hide it. It is only visible below
the crossover, where the answer is not to tune the chunk count but not to fork.

### Sparks are not competitive, except once

`ParSpark` loses to `Par` almost everywhere, and the exception says why. On
90,000 *boxed* cells at `-N4` the gather kernel measures 5.38 ms sparked
against 6.18 ms forked -- the one place sparks win -- because that is the case
where per-cell work is largest relative to the extra `n`-element copy that
sparking costs. On the fold kernel, ten times cheaper per cell, the copy is
most of the budget and `foldParSpark` never comes close: 414 μs against 244 μs
on 90,000 unboxed cells at `-N4`.

So purity is buyable, but only for the expensive kernel on the representation
that suits it least. It is not a general substitute for the `unsafePerformIO`
split.

## A caveat about this machine

The machine has 10 cores, but not ten of the same core: `hw.perflevel0` is 4
performance cores and `hw.perflevel1` is 6 efficiency cores. So `-N4` is
roughly "the fast cores", and `-N8` necessarily puts half of a static equal
split on cores several times slower, where the join then waits for the
slowest. `-N8` is worse than `-N4` in nearly every row above.

Any reading of "returns stop past four cores" here has to carry that: it is at
least partly a fact about this laptop and not only about the algorithm. kb38
measured the same knee on the same machine and attributed it to allocation
bandwidth. Both are probably true, and neither is separable from the other
without a machine whose cores are all alike.

## What this says to do

In order of how much they are worth against how little they cost:

1. **Change the two pragmas.** `INLINE` rather than `INLINABLE` on
   `stencilGrid` and `stencilFoldGrid`: 1.74x and 19x less allocation on the
   fold kernel, no API change, no threads, no threshold, verified in the
   library itself and not only in the spike. Weigh it against the code-size
   cost at the call sites, which this spike did not measure.
2. **Offer a strict fill.** 5.1x on a hundred-generation boxed automaton loop,
   still with no threads. It cannot simply replace the lazy fill: forcing
   every cell is a real semantic change for a boxed grid, where the current
   kernels let a consumer hold a grid some of whose cells are bottom or merely
   expensive and never look at them. `stencilFoldGrid` is the more defensible
   of the two to change in place -- a caller who reaches for a fold kernel
   rather than `stencilGrid` has already said they intend to fold every
   neighbour of every cell -- but that is a judgement about the API's promises
   and not something these numbers can settle.
3. **Then, and only then, consider parallelism**, opt-in and never by silent
   dispatch. A library that forks behind the caller's back needs `-threaded`,
   changes strictness, and -- per the table above -- would be wrong to do it
   below about 1,000 cells for the gather kernel, below something between
   2,500 and 10,000 for the fold kernel on an unboxed grid, and wrong to do it
   at all for the fold kernel on a boxed one. Note that those thresholds are
   measured against sequential arms that items 1 and 2 have not yet made
   faster, so doing this work first would move them up again and shrink what
   is left to win.

## Two harness traps, in case they recur

**The first arm of every group was paying for the input.** A boxed `Grid`
built by `tabulateGrid` is a vector of thunks until something forces it, and
the first benchmark to `nf` a result over it is what does. The library control
is listed first in every group, so it was charged for materialising the grid:
33.6 μs against 22.4 μs for its own transliteration on 400 boxed cells, at
identical allocation, purely for being at the top of the list. `warm` in
`bench/Main.hs` forces every grid and table before `defaultMain`.

**A spark has to be the thing the consumer then demands.** Sparking `force c`
and handing on `c` leaves the spark unreachable the moment the consumer takes
the other route, and the RTS collects it: `+RTS -s` reported 6,185 of 6,384
sparks GC'd and no speedup whatever. Sparking the deep-forcing thunk *itself*,
and putting that same thunk in the list, took it to 10,835 converted of
12,528. `-s` is the only way either of these announces itself.
