# Module map

Outcome of `sized-grid-6kor`. Written 2026-08-26 after `6kor.1`, `.2` and
`.3` landed, and completed the same day when `.6` to `.9` closed the epic.
Also published as a page:
<https://claude.ai/code/artifact/771a3ec7-1092-4c91-b64f-f77c92db4678>

## Problem

Three modules carried the library: `Data.Grid.Sized.Coord` (1015 lines),
`Data.Grid.Sized.Internal.Grid` (1299) and `Data.Grid.Sized.Optics` (336).
2650 lines, half the source tree, and each of them a stack of unrelated
domains behind a single export list — `Coord` alone held the representation,
neighbourhoods, rays, paths, two metrics, boundaries, reflection frames,
centred and punctured coordinates, and the weaken/strengthen classes.

The cost was navigational, not functional. Nothing was wrong with the code;
it was just that finding the two lines that compute a Manhattan distance meant
scrolling past four hundred lines of instance declarations, and the import
lists at the top of each module told a reader nothing about which part of the
module needed what.

## What the split did

Each of the three became a facade with a **byte-identical export list**, so no
downstream import changed. `Internal.Grid`'s list lost one thing: a `-- $bulk`
doc anchor, which now lives in `Internal.Grid.Core` with the named chunk it
refers to.

```
Data.Grid.Sized.Coord                 1015 →  137   (pure re-export)
Data.Grid.Sized.Internal.Grid         1299 →   69   (pure re-export)
Data.Grid.Sized.Optics                 336 →   55   (pure re-export)
```

### `Coord`

| module | holds | exposed |
|---|---|---|
| `Coord.Internal` | the `Coord` newtype, its instances, `AffineCoordList`, `AllSizedKnown` | no |
| `Coord.Torus` | `TorusCoord` and the finite displacement group | yes |
| `Coord.Neighbourhood` | `offsetCoord`, `stepsWithin`, Moore, von Neumann | yes |
| `Coord.Path` | `OffGrid`, rays, `Path`, the two walks | yes |
| `Coord.Distance` | Chebyshev and Manhattan | yes |
| `Coord.Boundary` | edges, corners, interior | yes |
| `Coord.Transform` | reflection frames, transport, weaken/strengthen | yes |
| `Coord.Centre` | centred and punctured coordinates | yes |

`Coord.Internal` is hidden for the same reason `Internal.Grid` is: it exports
the raw `Coord` constructor, which every sibling needs because they all do
arithmetic on the flat position. That constructor is no stronger than the
public `unsafeCoordFromPosition`/`coordPosition` pair — a coordinate *is* its
position since `adr.16` — so hiding it closes no hole. What it buys is that
the range invariant keeps one documented public entrance instead of two.

The submodule is `Coord.Neighbourhood`, not `Coord.Neighborhood` as the issue
proposed, because it exports `neighbours`, `mooreNeighbours` and
`vonNeumannNeighbours`.

### `Internal.Grid`

| module | holds |
|---|---|
| `Internal.Grid.Core` | the `GridOf` newtype, every instance, the bulk operations |
| `Internal.Grid.Shape` | `permuteGrid` … `zipLowerDim`: the whole-axis algebra |
| `Internal.Grid.Axis` | `MapAxis` and the fibre machinery |
| `Internal.Grid.Windows` | `shrinkGrid`, tiles, sliding windows |
| `Internal.Grid.Nest` | the axis-list recursion, at a boxed vector |

All five are hidden, as the aggregator already was.

`Nest` is the one cut that is not along the export list. The four axis-list
recursions — `nestByShape`, `flattenByShape`, `nestedToJSON`,
`nestedParseJSON` — must not see the vector parameter, or the dictionary they
carry per level stops specialising and every `take`/`drop`/`concat` becomes an
indirect call, measured at 60–300%. None of the four mentions `GridOf`, so they
move out whole, and `Core` keeps the two wrappers, the two `RULES` and the two
JSON instances that do. The module boundary now *states* the rule that a
`$recursion` doc chunk previously only asked for — and that chunk was
unreferenced, so it had never rendered anywhere.

`Shape` is a concern the issue did not name but the file had: the shape algebra
is arithmetic on the flat vector, defines no instances, touches no fibres, and
`Windows` is built on top of it.

### `Optics`

`Optics.Coordinate` (coordinates, displacements, ordinals, and the
`Field1`…`Field5` orphan instances), `Optics.Grid`, `Optics.FocusedGrid`. All
exposed. The split is along the export sections rather than the line order,
because the file interleaved two domains: `permuted` and `_Transposed`, both
grid optics, sat between `_EmptyCoord` and `_Position`.

`-Wno-orphans` moves to `Optics.Coordinate` with the instances it covers. Those
instances attach `lens`'s classes to this library's types, so neither package
can own both sides; the facade re-exports the module, which is what keeps a
consumer of the optics getting the instances.

### `Coord.Class`

Not one of the three the epic was filed for. It became the largest module in
the tree once the three were split -- 558 lines the epic had never touched --
and it held two things that separate cleanly:

| module | holds | exposed |
|---|---|---|
| `Coord.Class.Axis` | what one axis's boundary policy means: `IsCoord`, `IsCoordLifted`, `Boundaryless`, `Extremum`, `Even`/`Odd`/`OddC`, the index conversions | yes |
| `Coord.Class.List` | the row-major fold over the axis list: `IsCoordList` and its two instances, `IsCoordListF`, `MapDiff`, `AllDiffSame` | yes |

```
Data.Grid.Sized.Coord.Class            558 →   36   (pure re-export)
```

Every `IsCoordList` method calls `offsetIsCoord`, `axisBoundaryIsCoord`,
`axisDistanceIsCoord`, `toAxisIndex` or `unsafeFromAxisIndex`, and nothing goes
the other way, so the split is one edge with no cycle. Both children are
exposed, as `Optics`'s are, because the facade is exposed and each half is a
public concept.

Nothing moved but the module boundary: the export list is byte-identical, the
33 `INLINE` pragmas are the same 33 lines, and the two halves' code text
matches the original body exactly once blank lines and order are set aside.
All three were checked against the parent commit rather than assumed.

## The resulting layering

No cycles, and each layer imports only downward:

```
  Internal.Error   Internal.Type   Coord.Delta        (no internal imports)
        ↓                ↓
     Ordinal   →   Coord.Class.Axis → Coord.Class.List
                        ↓
                  Coord.Class                          ← facade
                        ↓
                  Coord.Internal
                        ↓
   Coord.{Torus,Neighbourhood,Distance,Boundary,Transform,Centre}
                        ↓  (Path ← Neighbourhood)
                      Coord                           ← facade
                        ↓
   Internal.Grid.Nest → Internal.Grid.Core → Shape → Windows
                                          ↘ Axis
                        ↓
                  Internal.Grid                       ← aggregator
                        ↓
      Focused → Class → Optics.{Coordinate,Grid,FocusedGrid}
                        ↓
                     Optics                           ← facade
                        ↓
                  Data.Grid.Sized
```

## What must stay stable across any further work here

These are the load-bearing facts a later refactor can break silently. Every one
of them has a measurement in the source next to it.

1. **`INLINE` on `(:|)`.** A pattern synonym with a required context compiles to
   a matcher that takes dictionaries. Without the pragma, `tabulate 300x300`
   with a rule that destructures its coordinate goes from 1.13 ms / 4.8 MB to
   9.20 ms / 36 MB.
2. **`INLINE` on `permuteGrid`.** `INLINABLE` leaves the axis list opaque to the
   module that calls it; `transposeGrid` at 300×300 measured 22–29 ms / 85–92 MB
   against 873 µs / 704 KB.
3. **`INLINE`, not `INLINABLE`, on `mapAxisStrided` and `scanAxisStrided`.**
   Worth 2.1× boxed and 8.7× unboxed. Being `INLINE` also means their bodies
   are optimised in the *caller's* context, which is what the last section of
   this page is about.
4. **The library's `-O2 -fexpose-all-unfoldings -fspecialise-aggressively`**, and
   `-fno-ignore-asserts` staying *last* on that line.
5. **`IsCoordList`'s folds being class methods.** A self-recursive fold over the
   axis list is polymorphic recursion GHC cannot unroll: 27 MB against 34 bytes
   on `index x90000`.
6. **The two `RULES`** on `collapseGrid` and `gridFromList`, and their phase
   control (`[1]` on the function, `[~1]` on the rule). Unphased, the function
   inlines before the rule can match.

The split moved none of these: every pragma travelled with its definition, and
`-fexpose-all-unfoldings` was already on, so a cross-module call site sees what
an intra-module one saw. Confirmed by running the benchmark suite against a
same-session baseline recorded from the parent commit in a clean worktree, once
for each of `.1`, `.2`, `.3`, `.6`, `.7`, `.8` and `.9`. For `.9`, the one that
touches `IsCoordList` itself, the decisive figure is allocation rather than
time: all 70 benchmarks that allocate more than a kilobyte came within 0.5% of
the control, and it is allocation — 27 MB against 34 bytes — that moves when
that fold stops unrolling.

**Do not benchmark against `bench/baseline-ghc9.12.3-aarch64-darwin.csv` to
judge a refactor on a busy machine.** Doing so during `6kor.2` reported five or
six failures at 25–52% slower, with a different set each run and every
allocation figure at or below baseline. The suite was taking 230 s where it had
taken 160 s that morning. Record a control from the parent commit and compare
against that.

## What was found duplicated, and what was done about it

Three true duplications, all now closed against this epic:

- **`6kor.6`** — `Ixed.ix` and `Optics.cell` had byte-identical bodies, and
  both bounds-checked a write that `IsGrid.gridIndex` documented could not
  fail. One `cellLens` in `Internal.Grid.Core` now, reached by all three, with
  `unsafeUpd` on every path. `gridIndex` losing its old getter took
  `AllSizedKnown cs` and `IsCoordList cs` off both `IsGrid` instances with it:
  neither was needed once a coordinate *was* its position.
- **`6kor.7`** — `axisFibres` duplicated `mapAxisStrided`'s fibre gather. One
  `fibreAt`. Only the read: see below.
- **`6kor.8`** — twelve repetitions of the pointwise-lift idiom across
  `Coord`'s instances became `pointwise0`/`pointwise1`/`pointwise2`, taking
  the axis class as a type argument; four `TorusCoord` instances became
  `deriving newtype`. The other eight `TorusCoord` instances stay
  hand-written, each for a stated reason.

Seven further near-duplications were examined and found intentional. They are
catalogued in bd memory `duplication-in-grid-sized-that-is-deliberate-and` so
they are not re-litigated; the short version is `splitVectorBySize` vs
`splitBoxedBySize`, `mapAxisStrided` vs `scanAxisStrided`, the two nesting
recursions vs the two JSON ones, `posAdd` vs `posTransport`, `WeakenCoord` vs
`StrengthenCoord`, `gridTiles` vs `gridWindows`, and `IsCoordList`'s twelve
methods sharing a `quotRem stride` skeleton. Each has a measurement behind it.

## The eighth: shared code whose cost depends on the caller's `-O` level

`6kor.7` had an optional second half — sharing the *walk* that produces the
fibre start positions, not just the read. It was written and benchmarked, and
the result is the most useful thing this epic turned up:

| | `-O1` | `-O2` |
|---|---|---|
| explicit walk | 170 µs | 145 µs |
| shared `[Int]` producer | 374 µs | 149 µs |

`mapAxisStrided` is `INLINE`, so its body is optimised in the *caller's*
context, and the benchmark component is built at cabal's default `-O1` while
the library is `-O2`. The list fuses away at `-O2` and does not at `-O1`. The
walk stays duplicated, because `-O1` is what a consumer's executable gets by
default and two saved lines are not worth doubling their inner loop.

Two rules follow, and they apply to any later change to an `INLINE` kernel
here:

1. **A benchmark judging such a change must say which `-O` level the caller
   was built at.** The same diff reads as a 120% regression or as free.
2. **`stencilFoldGrid'` is not the only entry point with an `-O1`/`-O2` gap.**
   Measured the same way, `scanAxis 1` over an unboxed 300×300 grid is
   224 µs / 4.41 MB at `-O1` and 75 µs / 1.53 MB at `-O2` — a 3.0× split,
   sharper than the stencil's. `scanAxis 0` is unmoved, because stride 1 takes
   the `VG.concat` arm and never enters the `ST` loop, which localises the
   problem to the strided arm. Recorded on `sized-grid-y99h`, which now has to
   decide for `scanAxisStrided` as well as for the stencil.
