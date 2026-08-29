# The grid-view family

Design for `sized-grid-d9o9`. Written 2026-08-26.

**Status, 2026-08-27.** Pieces 1 and 2 of the recommendation landed as
`sized-grid-mbh0`: the rule is stated in the README's design thesis, in
`Internal.Grid.Windows`'s module header, on `permuteGrid` and on the
"Windows and tiles" export section of `Data.Grid.Sized`, and `shrinkGrid`,
`gridWindows` and `gridTiles` now return `Ordinal`-axed grids with
`Ordinal` offsets. The tables below describe the state *before* that, and
are kept as the record of what was found. Piece 3 is `sized-grid-xmxm` and
piece 4 is `sized-grid-3ybi`; the rest of the shape algebra, which this
document's table lists as "same as source" without flagging it, landed as
`sized-grid-pnws` on 2026-08-29 -- `takeGrid`, `dropGrid`, `sliceGrid`,
`splitHigherDim` and the `slice`/`prefix`/`suffix` lenses are
`Ordinal`-axed too, and `combineHigherDim` was left alone as a
*construction*, where the policy is the caller's to declare.

## Question

The issue asks whether the library is missing a general "view over a grid" —
a `Window`, `Subgrid` or `View` type that `FocusedGrid`, `shrinkGrid`,
`gridWindows`, `gridTiles`, `slice` and `axisFibres` are all special cases of.

Answer, in one line: **the general abstraction already exists and is called
`permuteGrid`; what is missing is the boundary-policy rule that every member of
the family should obey, and today two of them get it demonstrably wrong.**

## What the library has

| API | Narrows the index space? | Keeps the source position? | Optic | Result's axis policy |
| --- | --- | --- | --- | --- |
| `FocusedGrid` | no | yes, the focus | `focus` / `unfocused` lenses | same as source |
| `Walker` | no | yes, focus + heading | none | same as source |
| `shrinkGrid` | yes, all axes at once | no | none | **forced** same as source |
| `gridWindows` / `windows` | yes, outermost axis | no | `Fold` | **free**, caller picks |
| `gridTiles` / `tiles` | yes, outermost axis | no | `Traversal` | **free**, caller picks |
| `sliceGrid` / `slice` / `prefix` / `suffix` | yes, 1-D only | offset is in the type | `Lens` | same as source |
| `splitGrid` / `lowerDim` | yes, drops outermost axis | index of the sub-grid | `Iso` / `Traversal` | same as source |
| `axisFibres` / `axisFold` / `axis` | yes, down to one axis | no | `Fold` / `Setter` | same as source |
| `partitionFocus` / `PuncturedCoord` | no | the centre | none | same as source |
| `Stencil` | positions, not sub-grids | implicit in the row | none | resolved at build time |

Two things fall straight out of the table.

**It is two families, not one.** `FocusedGrid` and `Walker` *point*: they add a
distinguished position and keep the full extent. Everything else *restricts*:
it narrows the extent and keeps no position. They do not share laws.
`FocusedGrid`'s entire value is its `Comonad`, and that comonad is over the
whole grid — `extend f` runs `f` at every cell of the source. A restriction's
whole value is that it is *not* the whole grid. Forcing both under one
`View cs ds` carrying an injection `Coord ds -> Coord cs` would make
`FocusedGrid` the case `ds ~ '[]`, which throws away the comonad and with it
every consumer call site (`../aoc` uses `extend`, `experiment`, `seek` and
nothing else). So: do not unify them. The pointing family is
`sized-grid-ylhl`'s; this document is about the restriction family only.

**The restriction family disagrees with itself about boundary policy.** Three
spellings, three answers: `shrinkGrid` forces the window's coordinate
constructor to equal the source's, `gridWindows` and `gridTiles` leave it
entirely free, and nothing anywhere says which is right.

## The bug that settles it

The library thesis is that the coordinate type *is* the boundary policy, and
that nothing clamps or wraps silently. A window that keeps its source's policy
breaks that, and not subtly. Source `[1..9]`, window of 3 at offset 1, one step
to the left of the window's first cell:

```
              Periodic 9 -> Periodic 3        Clamped 9 -> Clamped 3
source                    [1,2,3,4,5,6,7,8,9]  [1,2,3,4,5,6,7,8,9]
window at offset 1        [2,3,4]              [2,3,4]
window cell 0, step left  4                    2
the same step in source   1                    1
```

The window invents a seam that does not exist in the space it is a view of.
Periodicity is a property of a whole axis; a proper sub-window of a periodic
axis is not periodic. "Clamped" means stepping off the edge stays at the edge;
the window's edge is not the source's edge, so clamping there is a statement
about a wall that isn't there. Both answers are wrong for the same reason, and
both are wrong *silently* — the one failure mode this library organises its
types against.

`shrinkGrid` cannot avoid it: its instance head is
`ShrinkableGrid (c x ': cs) (c y ': as) (c z ': bs)`, so the window's policy is
the source's by construction. `gridWindows` merely permits it — `small` and
`big` are independent types related only by `CoordNat small <= CoordNat big`,
so `gridWindows @(Clamped 3)` over a `Grid '[Periodic 9]` compiles and is what
produced the right-hand column above.

## The rule

> **A restriction destroys the boundary policy. A pointing preserves it.**

Concretely: every proper sub-grid lands on `Ordinal`, the policy-free axis the
library already has. `Ordinal` is exactly right for this — it is an `IsCoord`
with no walls and no wrap, so an offset that leaves it returns `Nothing`
instead of inventing an answer. A window is a bag of cells with a shape and no
topology of its own, and `Ordinal` is how this library spells that.

The same rule read from the other side fixes the offsets. `shrinkGrid`'s offset
into a `Periodic 9` source windowed to 3 has type `Coord '[Periodic 7]`: the 7
is the number of valid offsets, and `Periodic` is meaningless there — worse,
`<>` on that coordinate wraps offset 6 plus offset 3 round to offset 2, which is
arithmetic on a position in no space the caller has. Offsets are indices, so
they are `Ordinal`s too.

The rule costs the full-width case a little honesty for a lot of simplicity: a
window whose extent equals its source's *does* preserve the policy, but making
the type say so needs a type-level conditional on every axis, which is not worth
it. Restrictions return `Ordinal`; a caller who wants the policy back re-states
it, which is a place where they have to think, which is the point.

## The general combinator already exists

`permuteGrid` is documented as a permutation and named as one, and its type is
strictly more general:

```haskell
permuteGrid :: (Coord cs -> Coord ds) -> GridOf v ds a -> GridOf v cs a
```

Nothing requires that function to be bijective, injective, or even
non-constant. It is `Grid` being contravariant in its index — the general
"view by reindexing" — and every member of the restriction family is an
instance of it. Verified, both of these compile and run:

```haskell
-- a window of 3 at offset 1, as a coordinate map into the source
permuteGrid (\c -> unsafeOrdinal (ordinalToInt (headOf c) + 1) :| EmptyCoord) src
  == [2,3,4]

-- not a permutation at all: every cell reads source cell 0
permuteGrid (const (unsafeOrdinal 0 :| EmptyCoord)) src
  == [1,1,1,1]
```

So the answer to "should the shared abstraction be a typeclass, a wrapper type,
or a documented convention" is: **it is an existing function whose Haddock does
not say what it is.** The specialised members stay, because they are fast where
`permuteGrid` is not — `gridWindows` is one `VG.slice`, where `permuteGrid`
builds a `coordSpaceSize @cs`-long index table and a `Coord` per cell — but the
concept has a name and a home already.

## What not to build

- **No `Window` / `Subgrid` wrapper holding its source.** It retains the whole
  source grid behind a small view, and it re-poses rather than answers the
  policy question, since the wrapped grid still has to have *some* axis type.
- **No `GridView` typeclass.** The two families share no law worth abstracting
  over, and within the restriction family the only shared operation is
  "reindex", which is `permuteGrid`.
- **No `View cs ds` carrying an injection.** It is `permuteGrid` with the
  function stored instead of applied, and storing it forfeits the flat
  row-major representation that every fast path in `Internal.Grid.Shape` and
  `Internal.Grid.Windows` depends on.

## Recommendation

**API addition plus one breaking correction**, not a new type and not a class.
Four pieces, in dependency order:

1. **State the rule** — in `Internal.Grid.Windows`'s module header, on
   `permuteGrid`, and in the README's boundary-policy section. Restrictions are
   policy-destroying; pointings are policy-preserving. This is the docs-only
   half and it is worth doing even if nothing else lands.

2. **Make the types say it** (breaking): `shrinkGrid`, `gridWindows` and
   `gridTiles` return `Ordinal`-axed grids, and `shrinkGrid`'s offset becomes
   an `Ordinal` coordinate. Pre-1.0, and the status quo is a silent wrong
   answer, so the break is the cheap part.

3. **Give the offset back** — `windows` becomes an `IndexedFold` and `tiles` an
   `IndexedTraversal`, keyed by the offset; likewise `lowerDim` and `axisFold`.
   Every real windowing consumer (convolution, template matching, "which 3x3
   sums highest") needs to know *where* a window came from, and today recovers
   it by `zip [0..]` and re-deriving the arithmetic. `lens` already has the
   vocabulary, so this is the offset-carrying view the issue was reaching for
   at the cost of no new type.

4. **Restrict along any axis, not just the outermost** — `gridWindows` and
   `gridTiles` do the outer axis, `shrinkGrid` does all of them at once,
   `slice` does 1-D only, and the documented way to tile the second axis is
   `zipLowerDim gridTiles`, which does not generalise to the third. `MapAxis n
   cs c` already solves exactly this for `mapAxis` / `scanAxis` / `axisFibres`
   by reaching an axis by position and reading off its size and stride. Windows
   and tiles should be restated on it.

`permuteGrid` is not renamed. `reindexGrid` would describe it better, but the
name is published and the Haddock can carry the correction.

## Not covered here

The pointing family — `FocusedGrid`, `Walker`, `traceOffset`, `tracePath`,
`stepWalker` — is `sized-grid-ylhl`, answered on 2026-08-29 in
[the pointing family](2026-08-29-pointing-family-design.md): its model is
already complete at the `Coord` layer and only three of its operations were
lifted, so that is an API addition too, not a new type and not a class. The
"do not unify them" conclusion above survived the second look. The direct
sliding-window transforms that
avoid materialising each window are `sized-grid-psk4`, and piece 3 above is
their read-only precursor rather than a substitute for them.
