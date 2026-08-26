# Module map

Outcome of `sized-grid-6kor`. Written 2026-08-26, after `6kor.1`, `.2` and `.3`
landed. Also published as a page:
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

## The resulting layering

No cycles, and each layer imports only downward:

```
  Internal.Error   Internal.Type   Coord.Delta        (no internal imports)
        ↓                ↓
     Ordinal   →   Coord.Class
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
   Worth 2.1× boxed and 8.7× unboxed.
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
same-session baseline recorded from the parent commit in a clean worktree — all
74 pass for each of the three splits.

**Do not benchmark against `bench/baseline-ghc9.12.3-aarch64-darwin.csv` to
judge a refactor on a busy machine.** Doing so during `6kor.2` reported five or
six failures at 25–52% slower, with a different set each run and every
allocation figure at or below baseline. The suite was taking 230 s where it had
taken 160 s that morning. Record a control from the parent commit and compare
against that.

## Still open

Three true duplications, filed against this epic:

- **`6kor.6`** (P2) — `Ixed.ix` and `Optics.cell` have byte-identical bodies,
  and both bounds-check a write that `IsGrid.gridIndex` documents cannot fail.
  One `cellLens` in `Internal.Grid.Core`, reached by all three.
- **`6kor.7`** (P3) — `axisFibres` duplicates `mapAxisStrided`'s fibre gather.
- **`6kor.8`** (P3) — twelve repetitions of the pointwise-lift idiom across
  `Coord`'s instances, plus four `TorusCoord` instances that
  `deriving newtype` gives.

And one further split, filed as **`6kor.9`** (P3): `Coord.Class` is now the
largest module in the tree at 558 lines, and it was never touched. It holds two
things that can separate cleanly — `IsCoord`/`IsCoordLifted`, the per-axis
boundary policy, and `IsCoordList`, the per-axis-list fold — with the second
depending on the first and nothing going the other way.

Seven further near-duplications were examined and found intentional. They are
catalogued in bd memory `duplication-in-grid-sized-that-is-deliberate-and` so
they are not re-litigated; the short version is `splitVectorBySize` vs
`splitBoxedBySize`, `mapAxisStrided` vs `scanAxisStrided`, the two nesting
recursions vs the two JSON ones, `posAdd` vs `posTransport`, `WeakenCoord` vs
`StrengthenCoord`, `gridTiles` vs `gridWindows`, and `IsCoordList`'s twelve
methods sharing a `quotRem stride` skeleton. Each has a measurement behind it.
