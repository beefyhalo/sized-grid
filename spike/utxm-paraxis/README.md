# utxm: walking one axis of a grid on more than one core

Candidate walks for `mapAxis` and `scanAxis`, measured against the library's
own. It exists to answer `sized-grid-utxm` -- are the axis operations worth
parallelising, and above what size -- and it is deliberately **not** a port of
`Data.Grid.Sized.Internal.Grid.Axis` and not a candidate to become one.

Building a stencil table is `sized-grid-kb38` and running one is
`sized-grid-49hi`; both are answered in their own directories. This is the
third of the three, and it is the one where the answer is least about threads.

Frozen at the state that produced the numbers below. It is not in the
project's `cabal.project`, so `cabal build all` never sees it and CI never runs
it; it carries its own `cabal.project` naming `../..` instead. Nothing else in
the tree depends on it, so it can be deleted the day utxm's follow-ups land.

## Running it

```
cd spike/utxm-paraxis
cabal bench utxm-paraxis:bench:compare \
  --benchmark-options="--time-mode wall" -- +RTS -N4
```

`sh run-all.sh` is the eight runs the checked-in CSVs come from:
`run.sh` produces `results-*.csv` and `run-huge.sh` produces
`results-huge-*.csv`, at `-N1/2/4/8` each.

`--time-mode wall` is not optional, for the reason `spike/kb38-parstencil`
records at length and `bench/README.md` now warns about: **tasty-bench measures
CPU time by default**, which its own Haddock notes is "total elapsed CPU time
across all cores" for a multithreaded algorithm, so it cannot see a parallel
speedup by construction.

The binary checks every variant against the library before it measures
anything -- both vector representations, both axes of a 2-D grid and all three
of a 3-D one, four chunk multipliers each, a length-preserving reversal and a
Horner scan that are both order-sensitive -- and exits non-zero on the first
disagreement, so a variant that is fast because it scrambled a fibre fails
rather than reports.

That gate was tested rather than trusted. Dropping the last row of each strip
in `scanParStrip` made it fail immediately with an uninitialised-element error;
swapping the operator's arguments in `scanFibres` produced 80 mismatches, all
of them on the Horner scan and none on the commutative one -- which is exactly
why an order-sensitive operator is in the set. Both breaks were reverted and
the gate re-run clean.

## What is different about this question

kb38 and 49hi both split a per-cell fill. The number of independent pieces was
the number of cells, and the only question was whether there were enough of
them.

Here the independent pieces are **fibres**, and there are `len / axisSize` of
them however many cells there are. That is the whole shape of this problem, and
it has two consequences the other two spikes never met:

- A large grid can have nothing to split. `Thin` is 4 x 22,500: 90,000 cells,
  and **four** fibres along its inner axis.
- The chunk count is bounded by the fibre count, so the granularity knob that
  did not matter in kb38 or 49hi matters here, and it matters in the wrong
  direction.

## The axes of the experiment

**Which axis.** `stride == 1` is the innermost, whose fibres are already
contiguous; the library skips both the gather and the mutable scatter for it.
Any other axis is gather-apply-scatter over a strided walk. These are two
different programs in the library and they do not parallelise alike, so every
arm is measured at both.

**How the chunk walks memory.** `scanAxisStrided` deliberately does *not* walk
fibres: it makes one in-order pass up the vector, reading the element one
stride back out of the output it already wrote. A split that hands each worker
a set of fibres reinstates the walk the library rejected. A split that hands
each worker a contiguous *range of offsets* and sweeps it row by row does not.
`Fibre` and `Strip` are those two.

**How many cores**, and at what chunk granularity.

**Whether the grid fits in cache.** This one was not planned. It turned out to
decide the answer, so it got its own shape late; see immediately below.

## How to read the tables

**Every ratio quoted below is between two arms measured in the same process**,
except the crossover table, which says where it is not and why. Each `-N`
column is a separate run of the binary, and absolute times drift between runs
on this machine by more than several of the effects measured here -- across the
runs taken for this spike, one whole `-N2` column arrived 50% high, and the
final `-N8` column of the out-of-cache sweep arrived about 90% high on every
arm of every group at once. Where a column is visibly disturbed it is called
out rather than quoted.

So read a column, never a row, and treat the crossovers as findings and the
peak numbers as one machine on one morning. This is 49hi's warning and it
earned its place again here.

## What the numbers said

### Everything fitted in cache, and that was hiding the answer

This machine's performance cluster has a 16 MB L2 shared by its four cores. The
largest shape in the first pass, `Big`, is 300 x 300 `Int` = 720 KB. It is
resident, it never misses, and so a strided walk and an in-order walk over it
cost the same.

90,000 cells (`Big`), axis 0, unboxed, `-N1`, one process:

| arm | time |
|---|---|
| `scanSeq` (in-order pass, the library restated) | 75.8 μs |
| `scanSeqFibre` (fibre-at-a-time walk) | 75.7 μs |

The library's own Haddock on `scanAxisStrided` records the fibre walk losing to
the in-order pass by 254 μs against 68 μs on this exact shape. **That did not
reproduce**: level, to 0.1%. Which looked like a contradiction, and was
instead the measurement running entirely inside L2, where the access pattern is
nearly free.

Adding a 2000 x 2000 shape -- 4,000,000 cells, 32 MB, twice the L2 -- settles
it. Same arms, same code, `-N1`:

| arm | 90,000 cells (in cache) | 4,000,000 cells (out of cache) |
|---|---|---|
| `scanSeq` (in-order pass) | 75.8 μs | 3.05 ms |
| `scanSeqFibre` (fibre walk) | 75.7 μs | 23.14 ms |
| ratio | 1.00x | **7.6x** |

**The library's choice of an in-order pass over a fibre walk is worth nothing
in cache and 7.6x out of it.** The Haddock was right and the first pass here
was measuring the wrong regime. Any future claim in this project about stride
and locality has to be made on a grid larger than 16 MB, or it is measuring
nothing.

### Parallelising the wrong walk never recovers what the walk lost

4,000,000 cells, axis 0 (strided), unboxed -- the fibre-split arm against the
sequential in-order pass it would replace:

| arm | `-N1` | `-N2` | `-N4` |
|---|---|---|---|
| `scanSeq` (in-order, one core) | 3.05 ms | 3.01 ms | 3.02 ms |
| `scanParFibre` | 23.23 ms | 12.95 ms | 7.19 ms |
| `scanParStrip` | 3.39 ms | 2.34 ms | **1.87 ms** |

(The `-N8` column of this sweep is omitted: it arrived with every arm of every
group, the untouched library included, about 90% high, so it says something
about the machine that morning and nothing about the arms.)

`scanParFibre` scales -- 23.2 to 7.2 ms is 3.2x on four cores -- and **at four
cores it is still 2.4x slower than the single-core sequential arm it set out to
beat**. Four cores do not buy back a 7.6x locality loss.

`scanParStrip` keeps the in-order walk inside each chunk and is the only scan
arm that wins on this axis: 1.61x over the sequential pass at `-N4`. That is
the design point of this spike, and it is worth stating as a rule rather than a
number: **split the iteration space, not the algorithm.** The strip split is
the same walk over a narrower window; the fibre split is a different walk.

In cache the ordering reverses -- on `Big` axis 0 at `-N4`, `scanParFibre` is
34.6 μs against `scanParStrip`'s 40.9 μs -- because with no misses to avoid,
the strip's extra loop nesting is pure overhead. Both are true, and which one
holds is a property of the consumer's grid, not of the library.

### The largest wins are sequential, again

This is the third spike in a row whose biggest number has no threads in it.

90,000 cells (`Big`), axis 1 (the contiguous axis), unboxed, one process,
`-N1`:

| arm | time | allocated |
|---|---|---|
| `scanAxis` (library) | 82.3 μs | 1.53 MB |
| `scanSeq` (transliteration) | 73.4 μs | 1.49 MB |
| `scanSeqMut` (fills directly, no `concat`) | 48.4 μs | 720 KB |
| `scanSeqStrip` (chunked, **not** forked) | 37.1 μs | 721 KB |

and the same four arms on 4,000,000 cells: 4.05 / 4.01 / 2.02 / 1.63 ms.

The library's `stride == 1` fast path is
`VG.concat (map (VG.scanl1' f) (splitVectorBySize axisSize v))`. That allocates
a vector per fibre, then allocates the result and copies every element into it
-- `2n` words where a direct fill needs `n`. **Filling directly is 1.52x in
cache and 1.99x out of it, and halves allocation**, with no threads, no
`unsafePerformIO`, and no API change. It is the single most actionable thing in
this spike.

Note what that does to the parallel numbers. The best parallel arm on this
shape is `scanParStrip` at 26.3 μs (`-N4`), which is 3.13x the library. But
2.22x of that is available with no threads at all. **Four cores add 1.41x on
top of the free fix.** Measuring a threshold before doing the free
optimisations would have credited all of it to the threads -- 49hi's trap, met
again.

### A 1.2-1.3x this spike could not explain

`scanSeqMut` (48.4 μs) and `scanSeqStrip` (37.1 μs) above run line-for-line the
same inner loop on the contiguous axis, allocate within 1 KB of each other, and
neither forks anything. The gap is real -- it reproduces on every stride-1
shape and in both cache regimes (2.02 against 1.63 ms out of cache) -- and
three candidate explanations were tested and eliminated:

- **Chunk count.** `scanSeqStrip` at one chunk is a single sequential loop and
  measures the same as at 32. Not the split.
- **Forking.** `scanSeqStrip` is `scanParStrip`'s code with `sequence_` in
  place of the fork/join. It is the *fastest* arm at `-N1`. Not the threads.
- **How the result vector is built.** `scanSeqFreeze` holds the loop fixed and
  replaces `VG.create` with `runST` over an explicit `unsafeNew`/`unsafeFreeze`.
  It measures 48.3 μs against `scanSeqMut`'s 48.4. Not `VG.create`.

What is left between them is the monad (`ST` under `VG.create` against `IO`
under `unsafePerformIO`) and the loop being reached through a list of chunk
ranges rather than called directly. Neither is a satisfying explanation and
neither was confirmed, so it is recorded as an open question rather than
guessed at. It is worth chasing: it is comparable to what four cores contribute
to the same operation, and it costs no threads to have.

### Where parallelism pays, in fibres

`mapAxis` is the clean case -- `f` consumes a whole fibre, so the sequential
path already walks fibres and a split changes nothing but which core runs
which. Axis 0, `mapPar` at `-N4` against the library at `-N1`:

| cells | fibres | library `-N1` | `mapPar` `-N4` | ratio |
|---|---|---|---|---|
| 1,024 (boxed) | 32 | 10.4 μs | 25.6 μs | 0.41x |
| 2,500 | 50 | 4.4 μs | 12.2 μs | 0.36x |
| 10,000 | 100 | 17.7 μs | 16.7 μs | 1.06x |
| 40,000 | 200 | 67.5 μs | 34.2 μs | 1.97x |
| 90,000 | 300 | 159.7 μs | 64.8 μs | 2.46x |
| 4,000,000 | 2,000 | 28.17 ms | 8.84 ms | 3.19x |

These two columns come from different runs, which everywhere else in this file
is the comparison to distrust. It is the right one here for kb38's reason: the
library at `-N1` is what a consumer who has not asked for `-threaded` actually
gets, so it is the baseline the decision is against. The crossover is what to
read, and it agreed to within one size step across every run taken.

**The crossover is at about 100 fibres and the win is unambiguous by 200.**
Stated in cells it would be "about 10,000", which is true only of a square grid
and is the wrong way to say it -- as the next table shows.

### The fibre count is the threshold, not the cell count

`Thin` is 4 x 22,500. Same 90,000 cells as `Big`, unboxed, and its two axes
disagree by nearly four orders of magnitude about how many pieces there are:

| axis | fibres | fibre length | library `-N1` | `mapPar` `-N4` | ratio |
|---|---|---|---|---|---|
| 0 | 22,500 | 4 | 226.7 μs | 94.9 μs | 2.39x |
| 1 | 4 | 22,500 | 58.3 μs | 47.0 μs | 1.24x |

Along axis 1 there are four units of work. A static split cannot use more than
four workers whatever `-N` says, and the scan arm on that axis confirms it:
`scanParStrip` goes 35.3 / 29.5 / 25.7 / 37.9 μs at `-N1/2/4/8` -- it stops
improving at four and is worse at eight, on a 90,000-cell grid.

**A threshold on the cell count would fork eight workers for four fibres.** Any
dispatch rule the library adopts has to be on `fibreCount`, which is
`len / axisSize` and which the type already knows.

### The chunk multiplier matters here, and kb38 and 49hi said it would not

Both earlier spikes found the static chunk count irrelevant once there was
enough work -- 1 and 8 chunks per capability within 2-3%. That does not
transfer, and the reason is the fibre count again. 90,000 cells (`Big`), axis
0, unboxed:

| chunks/capability | `-N1` | `-N2` | `-N4` | `-N8` |
|---|---|---|---|---|
| `scanParStrip` mult=1 | 70.1 μs | 50.3 μs | **38.8 μs** | 65.7 μs |
| `scanParStrip` mult=2 | 73.8 μs | 53.2 μs | 43.1 μs | 67.9 μs |
| `scanParStrip` mult=8 | 76.5 μs | 54.7 μs | 53.4 μs | 154.6 μs |
| `scanParStrip` mult=32 | 90.9 μs | 74.1 μs | 111.9 μs | **547.9 μs** |

One chunk per capability is best everywhere, and 32 is a catastrophe -- 8.3x
worse than mult=1 at `-N8`, where 256 chunks are being cut out of 300 fibres.

And it is not only the dispatch. The sequential `scanSeqStrip` degrades the
same way with no threads at all -- 70.0 μs at mult=1 against 87.4 μs at mult=32
at `-N1`, a 25% loss from nothing but narrower strips. A strip `(o1 - o0)`
words wide stops being a sequential access once it is comparable to a cache
line, and 300 fibres cut 256 ways is one word.

**So the main groups in this spike, which run at mult=2, understate every
parallel arm.** They were left that way because mult=2 is what the sibling
spikes used, and comparability across the three was worth more than a few
percent; `chunkGroup` is where the granularity question is actually answered.

### Sparks are not competitive, except where the layout is the point

`ParSpark` trades the `unsafePerformIO` for `par` over per-chunk vectors. On
the contiguous axis this was expected to be *free* -- the sequential path is
already a `concat`, so sparking the thunks it is about to demand should cost no
extra allocation -- and it is not: 102.1 μs against `scanSeq`'s 73.4 μs on
`Big` axis 1. The prediction was wrong because the arm sparks *chunks* rather
than individual fibres, and a chunk's result has to be built before it can be
concatenated.

Worse where there are many fibres: on `Thin` axis 0 (22,500 fibres),
`mapParSpark` is 751.2 μs against `mapSeq`'s 227.2 μs, and allocates 7.85 MB
against 2.88 MB.

The one exception is instructive. Out of cache on axis 0, `mapParSpark` at
`-N1` is 11.16 ms against `mapSeq`'s 27.97 ms -- **2.5x faster on one core** --
because laying each chunk out contiguously and scattering once has better
locality than the library's per-fibre gather-and-scatter when the grid does not
fit in cache. That is not a fact about sparks; it is another sequential
finding wearing a spark's clothes, and it belongs with the `scanSeqMut` result
as something to chase.

### Turning `-N` on costs the sequential path

kb38's finding, transferred intact. Unchanged library code, 90,000 cells,
unboxed:

| | `-N1` | `-N4` | `-N8` |
|---|---|---|---|
| `scanAxis`, axis 0 | 69.2 μs | 72.8 μs | 79.6 μs |
| `mapAxis`, axis 1 | 75.5 μs | 82.3 μs | 94.4 μs |

15% and 25% slower for asking for cores it does not use -- parallel GC on a
single-threaded mutator. A threshold that falls back to the sequential path on
a consumer who has asked for `-N` is falling back to a path `-N` has already
made slower.

## A caveat about this machine

Ten cores, but not ten alike: `hw.perflevel0` is 4 performance cores and
`hw.perflevel1` is 6 efficiency cores, and the 16 MB L2 belongs to the
performance cluster. So `-N4` is roughly "the fast cores", and `-N8`
necessarily puts half of a static equal split on cores several times slower,
where the join waits for the slowest. `-N8` is worse than `-N4` in nearly every
row above.

Any reading of "returns stop past four cores" here has to carry that. kb38 and
49hi measured the same knee on the same machine.

## What this says to do

In order of how much they are worth against how little they cost:

1. **Fill the contiguous scan directly instead of concatenating per-fibre
   vectors.** 1.52x in cache, 1.99x out of it, half the allocation, no threads,
   no `unsafePerformIO`, no threshold, no API change. `scanAxisStrided`'s
   `stride == 1` branch only. This is the whole of what this spike recommends
   doing unconditionally.
2. **Chase the two sequential effects it could not explain**: the 1.2-1.3x
   between `scanSeqMut` and `scanSeqStrip`, and the 2.5x between `mapSeq` and
   the chunk-then-scatter layout out of cache. Both are sequential, both are
   comparable to what four cores contribute, and neither is understood. A spike
   that ends with "and we do not know why" has named the next piece of work.
3. **Then, and only then, consider parallelism**, opt-in and never by silent
   dispatch. A library that forks behind the caller's back needs `-threaded`
   and changes strictness. If it is offered:
   - the rule is on `fibreCount >= ~200`, in fibres, never in cells;
   - one chunk per capability, not more;
   - the strip split, not the fibre split, for the scan -- and only the strip
     split is safe to offer at all, since the fibre split is slower than
     sequential code out of cache even on four cores;
   - `mapAxis` is the better candidate of the two: its split is uncontroversial
     (the sequential path already walks fibres) and it reaches 2.5x in cache
     and 3.2x out of it.
4. **Do not offer `ParSpark`.** It loses on every shape measured here except
   one, and in that one the win belongs to the memory layout rather than to the
   sparks.

Items 1 and 2 move the sequential baseline that item 3's thresholds are
measured against, so doing them first would raise the crossover again and
shrink what is left for the threads to win.

## What is deliberately matched

`src/ParAxis.hs` is in its own component with the same `ghc-options` as
`grid-sized`'s `library` stanza, so the experiments reach the benchmark through
a `.hi` file exactly as the control does. Compiling them into `bench/Main.hs`
would let GHC see the whole thing at once and flatter them.

The kernels take `axisSize` and `stride` as `Int`s, which is exactly how the
library's own `mapAxisStrided` and `scanAxisStrided` take them. The benchmark
does not write those literals out: it asks the library for them through
`axisSizeAndStride`, so an arm cannot be measured at a geometry the library
would never hand it.

`mapSeq` and `scanSeq` are transliterations of the library's two engines and
are *not* the controls; the controls are `mapAxis` and `scanAxis` themselves.
They are there so the parallel variants differ from something in the same
module by exactly the parallelism, and so the benchmark can show the
transliteration and the real one measuring the same -- which they do, to within
a few percent at every size.

`fibreAt` and `splitBySize` are copied from the library rather than imported,
because `Data.Grid.Sized.Internal.Grid.Axis` is an `other-module`. They are
byte-for-byte its own, so the only thing differing between a variant here and
the control is the walk around them.

`scanFibres` is shared by `scanSeqFibre` and `scanParFibre`, and `stripBy` by
`scanSeqStrip` and `scanParStrip`, so that in each pair the *only* difference
is whether the chunks are forked. Without those two sequential twins there is
no way to say what share of a parallel number is really parallel, and in this
spike the answer turned out to be "most of it is not".

`warm` forces every grid before `defaultMain`, including the 32 MB one, so no
benchmark pays to materialise its input and every group runs under the same
live-heap conditions. That is why the main CSVs were re-measured after the
out-of-cache shape was added rather than kept: a 32 MB live grid changes GC
pressure for every other group, and a checked-in CSV a re-run cannot reproduce
is worse than no CSV.
