# Folding one axis away

Design for `sized-grid-7qbi`. Written 2026-08-29. Measurements from
`spike/7qbi-foldaxis`, GHC 9.12.3, aarch64-darwin.

## Question

The library reaches one named axis for three operations — `mapAxis` (transform
each fibre in place), `scanAxis` (prefix-scan along it), `axisFibres` /
`axisFold` (read the fibres out) — and folds the whole grid with `foldlGrid'`.
It has no operation that *removes* an axis: row sums, column maxima, marginals,
the reduce half of a tensor contraction, the "collapse the time axis" step of a
dynamic program.

Answer, in one line: **implement it, as a strict left fold whose result is a
`GridOf` over the axis list with that axis deleted; the loop should be
output-driven, and the empty axis should be excluded by a constraint rather
than handled.**

## 1. What the library already has

Everything that reaches an axis by position goes through one class:

```haskell
class MapAxis (n :: Nat) (cs :: [Type]) (c :: Type) | n cs -> c where
  axisSizeAndStride :: (Int, Int)
```

At a concrete axis list that pair is two literals. Row-major layout means a
position in the flat vector decomposes as

```
p = hi * (axisSize * stride) + i * stride + lo
```

where `i` indexes axis `n`, `lo < stride` is the combined index of the axes
*below* it, and `hi` is the combined index of the axes *above*. Every existing
axis operation is a loop over that decomposition: `mapAxisStrided` gathers each
fibre, applies `f`, scatters it back; `scanAxisStrided` walks the vector in
order reading one element a stride back; `axisFibres` gathers lazily.

| API | what it does to the axis | result |
| --- | --- | --- |
| `mapAxis n` / `axis n` (`Setter`) | transforms each fibre, length-preserving | same shape |
| `scanAxis n` | prefix-scans along it | same shape |
| `axisFibres n` / `axisFold n` (`Fold`) | reads the fibres out | `[GridOf v '[c] a]` |
| `splitGrid` / `lowerDim` | peels axis 0 into an outer grid of sub-grids | `Grid '[c] (GridOf v cs a)` |
| `mapLowerDim` / `zipLowerDim` | maps under axis 0 | axis 0 kept |
| `takeGrid`, `dropGrid`, `sliceGrid`, `splitHigherDim` | narrow an axis | narrowed axis becomes `Ordinal` |
| `foldlGrid'`, `Foldable`, `Foldable1` | fold the whole grid | one value |
| `scanl1Grid` | scan the whole grid in row-major order | same shape |

The gap is exactly one row: nothing consumes an axis. The nearest thing a
caller can write today is `map (foldlGrid' f z) . axisFibres n`, repacked by
hand, or — for axis 0 only — `foldl (zipWithGrid f) … . splitGrid`. Both are
measured below as controls.

## 2. The result's type

```haskell
type family DropAxis (n :: Nat) (cs :: [Type]) :: [Type] where
  DropAxis 0 (c ': cs) = cs
  DropAxis n (c ': cs) = c ': DropAxis (n - 1) cs
```

A closed family, deliberately *not* an associated type of `MapAxis`. The long
note on that class records why an associated type does not work there — GHC
checks the two overlapping instances' equations for confluence and the abstract
recursive case does not pass — and the same argument would apply to any family
defined instance by instance. A closed family matches top to bottom in one
place and reduces the moment `n` is a literal, which it is at every call site
`RequiredTypeArguments` produces.

Three things fall out of the address arithmetic, and they are what make this
cheap:

**The output needs no permutation.** Deleting axis `n` deletes the middle term
of the decomposition above, so the fold that consumes every `i` at fixed
`(hi, lo)` belongs at output position `o = hi * stride + lo` — which is exactly
that cell's row-major position in `DropAxis n cs`. The fibres already come out
in that order, so filling the output in order is correct.

**The output needs no size evidence.** Its length is `length v / axisSize`,
both of which are already in hand. No `AllSizedKnown`, no `IsCoordList`, no
`KnownNat` on the result: the constraint set is `MapAxis n cs c` plus the two
`VG.Vector` obligations, the same set `mapAxis` carries.

**Every surviving axis keeps its boundary policy.** The library's rule is that
*a restriction destroys the boundary policy, a pointing preserves it*, which is
why `takeGrid` and `gridWindows` hand back `Ordinal`. A fold restricts nothing:
the axes that survive are at full width and untouched, and the axis that does
not survive has no cells left to have a policy about. So
`foldAxis' 0 :: Grid '[Periodic 9, Clamped 4] a -> Grid '[Clamped 4] b`, with
`Clamped 4` intact. This is `splitGrid`'s situation, not `splitHigherDim`'s.

*A lower-dimensional grid, not a family of summaries.* The "family of
summaries" reading is what `toListOf (axisFold n)` already gives, and it is
strictly worse: it loses the shape, so the result cannot be fed back into any
grid operation, and it allocates a cons cell and a materialised fibre per
summary (measured: 5.9 MB against 7 KB on a 90,000-cell grid).

## 3. Semantics

### The empty axis

`IsCoordLifted` demands `1 <= CoordNat x` for every axis, but **`AllSizedKnown`
does not, and neither does `MapAxis`** — so a zero-sized axis is constructible
without ever meeting that constraint:

```haskell
>>> let g = fromJust (gridFromVector V.empty) :: Grid '[Clamped 3, Clamped 0] Int
>>> -- folding axis 0 away: 0 cells in, 0 cells out, fine
>>> -- folding axis 1 away: should give three copies of the seed
```

The naive implementation divides by the axis's size to find the output's
length, and dies with `divide by zero` on the second one. Verified in the
spike, not reasoned about.

Two ways out, both measured:

1. **Take the length from the result type** —
   `KnownNat (MaxCoordSize (DropAxis n cs))` and one `natVal` — which makes the
   operation total and gives the right answer, `MaxCoordSize (DropAxis n cs)`
   copies of the seed.
2. **Exclude the empty axis by constraint**, `IsCoordLifted c` on the
   signature, which is the per-axis obligation `IsCoordList` already imposes
   and which every axis of every grid built through the safe API already
   satisfies. Then the division cannot divide by zero.

**Take (2).** Option (1) costs 5.7x at -O1 (§4), and it buys totality on a
grid that the rest of the library cannot do much with anyway: `Grid '[Clamped
3, Clamped 0] Int` has no coordinates, so `tabulateGrid`, `indexGrid` and
every stencil are already out of reach for it. The constraint is not a new
demand on callers; it is the demand the library already makes everywhere else,
stated where the division needs it.

The same constraint has a second use: it is exactly what makes a *seedless*
reduce total. `1 <= CoordNat c` means every fibre has a first element, so
`reduceAxis :: (a -> a -> a) -> …` needs no `Maybe` and no `Monoid` identity —
the `Foldable1` situation, and the reason `Foldable1 (Grid cs)` carries
`IsCoordList` while `Foldable` cannot.

### Order, and non-associative folds

The fold runs **strictly, left to right, in increasing index order along the
axis**, with the accumulator as the left argument. That is `foldlGrid'`'s
convention, `scanl1Grid`'s, and `scanAxis`'s, and it is the convention the
tests pin with `(-)` and `\acc x -> 2 * acc - x` — operators that are neither
associative nor commutative, so a fold that runs the wrong way or groups
differently cannot pass.

Two consequences worth writing on the function:

- **No associative variant.** An associative reduction may regroup, which buys
  nothing sequentially — the same number of applications — and costs the
  specified order, which is the thing a non-associative caller relies on.
  Regrouping only pays when it buys parallelism, and that is
  `sized-grid-utxm`'s question, not this one. If it lands, it wants a
  *separate* name whose contract is "associativity is the caller's promise,
  order is unspecified" — not a re-spelling of this one.
- **On a periodic axis, "index order" is the representation's choice, not the
  topology's.** A `Periodic 9` axis has no distinguished first cell, but its
  storage does, so a non-associative fold along it folds a particular rotation
  of the cycle. `foldlGrid'` and `scanl1Grid` have the same property and do not
  say so; this should.

## 4. Implementation, measured

Seven candidates, all with the same type, in `spike/7qbi-foldaxis`. Each was
checked against a coordinate-indexed reference — every axis of a 3x5 and a
2x3x4, boxed and unboxed, under a non-associative operator — before being
measured. Times in µs on 90,000-cell grids; **allocation is the -O1 column**
and is the same at -O2 to within a percent.

### The strided axis (axis 0 of 300x300, stride 300)

| candidate | -O1 boxed | -O2 boxed | -O1 unboxed | -O2 unboxed | allocated |
| --- | --- | --- | --- | --- | --- |
| output-driven (`nonempty`) | 347 | **274** | 301 | **63** | 7.3 KB / 2.5 KB |
| input-driven (`sweep`) | 550 | 546 | **133** | 133 | 1.4 MB / 2.5 KB |
| `axisFibres` + `foldlGrid'` | 1035 | 1070 | 197 | 194 | 5.9 MB / 819 KB |
| `splitGrid` + `zipWithGrid` | 4167 | 2590 | 742 | 191 | 9.4 MB / 6.6 MB |

### The contiguous axis (axis 1 of 300x300, stride 1)

| candidate | -O1 boxed | -O2 boxed | -O1 unboxed | -O2 unboxed |
| --- | --- | --- | --- | --- |
| output-driven (`nonempty`) | 314 | 296 | **92** | **91** |
| slice-per-fibre fast path | 311 | 295 | 91 | 90 |
| input-driven (`sweep`) | 703 | 704 | 288 | 288 |
| `axisFibres` + `foldlGrid'` | 893 | 878 | 195 | 195 |
| *for scale:* `foldlGrid'` over the whole grid | 311 | 294 | 87 | 89 |

### The middle axis of a cube (axis 1 of 60x50x30)

| candidate | -O1 boxed | -O2 boxed | -O1 unboxed | -O2 unboxed |
| --- | --- | --- | --- | --- |
| output-driven (`nonempty`) | 368 | **260** | 308 | **60** |
| input-driven (`sweep`) | 568 | 558 | **147** | 141 |
| `axisFibres` + `foldlGrid'` | 1128 | 1126 | 243 | 238 |

### What the numbers say

**Strided vector kernels, no materialised fibres — yes.** The output-driven
loop allocates 7.3 KB boxed and 2.5 KB unboxed for a whole 90,000-cell fold:
the result and nothing else. `axisFibres` allocates 5.9 MB for the same answer,
because a fibre is a copy and the list is a cons cell per output cell. That
ratio, not the times, is the reason not to build this out of the existing
pieces.

**It costs what the whole-grid fold costs.** On the contiguous axis, unboxed,
92 µs against `foldlGrid'`'s 87 µs on the same 90,000 cells. That is the floor,
and the operation is at it.

**No `stride == 1` fast path is needed.** `mapAxisStrided` and
`scanAxisStrided` both special-case the innermost axis, whose fibres are
contiguous. A fold does not need to: the general loop at `stride == 1` is
already a contiguous walk, and the hand-written slice-per-fibre variant matches
it to within noise (91 vs 92 unboxed, 311 vs 314 boxed). Two branches saved.

**`VG.generate` is not the cost.** Replacing it with an explicit mutable write
loop changes nothing but the allocation of the boxed result — 7.3 KB against
16.9 KB, because forcing before the write stores a value rather than a thunk.
The strict write is the right one anyway: a boxed vector of unforced
fibre-folds is a thunk chain per output cell, the hazard `scanl1Grid` is strict
for.

**The loops split by axis, and -O1 hides it.** The input-driven sweep reads the
input sequentially and keeps its partial results in the output, which is why it
wins on the strided axis unboxed. But it writes every output cell `axisSize`
times, and on a *boxed* vector each of those writes allocates a box: 1.4 MB per
fold, one word per input cell, which is what sinks it boxed everywhere. At -O2
the output-driven loop overtakes it on the strided axis too (63 vs 133), so at
-O2 there is no case among the scalar folds where the sweep wins. At -O1 there
are two, both unboxed, both about 2.3x. This is `sized-grid-y99h`'s pattern
exactly — a loop this library depends on being 3-5x apart between the two
optimisation levels — and it is why the recommendation is not "measure once".

**One case is not close: a product-typed accumulator on an unboxed grid.**
Folding `Int` into `(Int, Int)` (a min/max pair) over 300x300:

| candidate | -O1 boxed | -O1 unboxed | unboxed allocated |
| --- | --- | --- | --- |
| output-driven | 1855 | 1716 | 12.2 MB |
| input-driven (`sweep`) | 2280 | **177** | 4.9 KB |

The output-driven loop holds the accumulator in an argument, and a pair
accumulator is re-allocated every step whatever the vector is — 136 bytes per
input cell. The sweep holds its accumulators *in the unboxed output vector*,
where a pair is two machine words and updating one allocates nothing, so it is
9.7x faster and 2,500x smaller. Nothing at -O2 changes this (1572 vs 177).

This is a real hole in the recommendation, and it is the one to watch: min/max,
mean-and-count, and running (sum, sumOfSquares) are exactly the row/column
statistics the issue names as motivation. It is filed as a follow-up rather
than designed here, because the fix is not obvious — the choice depends on
whether `v y` is unboxed, which is not observable from `VG.Vector v y` — and
because the workaround is available today: fold the two components in two
passes, each with a scalar accumulator.

## 5. API sketch

```haskell
-- Data.Grid.Sized.Internal.Grid.Axis, exported through Data.Grid.Sized

type family DropAxis (n :: Nat) (cs :: [Type]) :: [Type]

-- | Strict left fold along one named axis, removing it.
--
-- > foldAxis' 1 (+) 0 g   -- rather than foldAxis' (Proxy @1) (+) 0 g
foldAxis' ::
     forall v cs x y c. forall n -> ( MapAxis n cs c
                                    , IsCoordLifted c
                                    , VG.Vector v x
                                    , VG.Vector v y)
  => (y -> x -> y) -> y -> GridOf v cs x -> GridOf v (DropAxis n cs) y
```

Primed, as `foldlGrid'` is, because it is strict. Named `foldAxis'` rather than
`reduceAxis` because it is a fold with a seed and a type change, not an
associative reduction; leaving `reduceAxis` unclaimed keeps that name free for
the parallel one if `sized-grid-utxm` ever wants it.

Not in the first cut, deliberately:

- **`reduceAxis :: (a -> a -> a) -> …`**, the seedless form. Total under the
  same `IsCoordLifted c` (§3), a natural companion to `scanAxis`, which is also
  seedless — and the only way to write `min`/`max` along an axis without
  inventing an identity. It is a second entry point to a slightly different
  loop (seed from the fibre's first element, start at `i = 1`), not a wrapper,
  so it is a follow-up rather than free. Worth having.
- **`sum`/`product`/`maximum` wrappers.** The issue says to avoid them until the
  core settles, and that is right.
- **An optic.** `axisFold n` already reads the fibres; a fold that changes the
  shape is not an optic over the source.

### Test matrix

`tests/Test/Axis.hs` is the home; its references and shapes carry over
unchanged.

| what | how |
| --- | --- |
| agrees with a coordinate reference, every axis | `refFlat0/1`, `refCube0/1/2` extended with a seed, on `'[Ordinal 3, Ordinal 5]` and `'[Ordinal 2, Ordinal 3, Ordinal 4]` — distinct sizes throughout, and a middle axis no `transposeGrid` reaches |
| order and grouping are pinned | every property run over `(-)` and `\acc x -> 2*acc - x`, neither associative nor commutative |
| agrees with the published-API spelling | `foldAxis' n f z g === (fromJust . gridFromVector . VG.fromList . map (foldlGrid' f z) . axisFibres n) g` — two implementations that share only the fibre decomposition |
| degenerates correctly | on a one-axis grid, `foldAxis' 0 f z === foldlGrid' f z` (up to the `'[]` grid's single cell) |
| the axes commute | for associative-commutative `f`, folding axes 0 then 1 equals folding 1 then 0 (indices shift: `foldAxis' 0 . foldAxis' 1` vs `foldAxis' 1 . foldAxis' 0` on a cube) |
| boxed and unboxed agree | the same properties at `UGrid`, as `tests/Test/Unboxed.hs` does |
| the empty axis is rejected | a `tests/compile-fail/` case: `foldAxis' 1` on `Grid '[Clamped 3, Clamped 0] Int` must not compile |
| the policy survives | a type annotation is the test: `foldAxis' 0 f z (g :: Grid '[Periodic 9, Clamped 4] Int) :: Grid '[Clamped 4] Int` must typecheck |

Benchmarks: `foldAxis' 0` and `foldAxis' 1` on 300x300, boxed and unboxed, in
the "boxed vs unboxed" group next to `mapAxis 0` and `axisFold 0`, which are
what they should be read against.

## 6. Recommendation

**Implement**, with the output-driven strict-write loop, `DropAxis n cs` as the
result shape, and `IsCoordLifted c` excluding the empty axis.

It closes a real gap — nothing in the library consumes an axis — at a cost that
is at the floor for the operation, with a constraint set no larger than
`mapAxis`'s, and it needs no new type-level machinery beyond one closed family.
The two things a caller writes today are 3.0x and 12x slower boxed, and
allocate 5.9 MB and 9.4 MB where this allocates 7 KB.

Performance risks, in order:

1. **The product-accumulator case** (§4), 9.7x on unboxed grids. Filed as a
   follow-up; the two-pass workaround exists.
2. **The -O1/-O2 split.** The recommended loop is 4.5x faster at -O2 on the
   strided axis unboxed, and at -O1 it loses to the input-driven sweep there by
   2.3x. Consumers get -O1 by default. This is `sized-grid-y99h`'s problem and
   should be tracked there rather than solved by picking the loop that is
   uniformly mediocre.
3. **Nothing else.** No fast path is owed, no fibre is materialised, and the
   result carries no size evidence that could go stale.
