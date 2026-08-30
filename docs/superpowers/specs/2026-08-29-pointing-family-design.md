# The pointing family

Design for `sized-grid-ylhl`. Written 2026-08-29.

Companion to [the grid-view family](2026-08-26-grid-view-design.md), which
split the library's views into two families and took the *restriction* half.
This is the other half: `FocusedGrid`, `Walker`, `traceOffset`, `tracePath`,
`walkEverywhere`, `stepWalker`.

## Question

The issue asks whether the walker/path semantics are a missing conceptual
layer — "a path/walker abstraction that composes focus, heading, and movement
under a single boundary-policy-aware model" — and whether it should be a
typeclass, a data type, or nothing.

Answer, in one line: **the conceptual model already exists and is complete at
the `Coord` layer; what is missing is that only three of its operations were
lifted through the pointing layer, and the three that were lifted throw away
the position.** No new type, no new class. One new `Coord`-layer primitive,
five liftings, and one field whose index is wrong.

## The model that already exists

Movement in this library is a 2×2×2, and the `Coord` layer fills every cell.

| | one step | a route |
| --- | --- | --- |
| **checked** — `Maybe`, needs only `IsCoordList` | `offsetCoord` | `walkPath` |
| **total** — licensed by the axis type | `(.+^)` (`AffineSpace`) | `walkPathTotal` (`All Boundaryless`) |

with two more that are checked-step iterated (`coordRay`, `offsetCoordUpTo`)
and one that is a total step carrying a frame (`transportCoord`).

That *is* the "boundary-policy-aware movement model" the issue is reaching
for, and it is already coherent: whether an operation may be total is decided
by whether the axis type licenses it, and everything else returns `Maybe`.
Nothing about a focus or a heading changes that. The README states the rule
already ("If a new function has to clamp, wrap, or truncate to stay total, it
either takes the coordinate type that licenses it or it returns `Maybe`").

## What the pointing layer does with it

Every function in `Data.Grid.Sized.Focused` is a one-liner over the table
above:

```haskell
traceOffset d (FocusedGrid g p) = index g <$> offsetCoord p d
tracePath   p (FocusedGrid g q) = index g <$> walkPath q p
walkEverywhere p                = extend (tracePath p)
stepWalker (Walker (FocusedGrid g p) h) = ... transportCoord p h
```

So the layer adds no semantics. It only re-exposes the `Coord` layer at two
richer points: a position **plus a payload** (`FocusedGrid`), and a position
plus a payload **plus a frame** (`Walker`). Which makes the gap countable:

| `Coord` operation | on `FocusedGrid` | on `Walker` |
| --- | --- | --- |
| `offsetCoord` (checked step) | `traceOffset` — **returns the value, not the grid** | **missing** |
| `(.+^)` (total step) | `seeks (.+^ d)`, hand-rolled | `stepWalker` ✓ |
| `walkPath` (checked route) | `tracePath` — **returns the value, not the grid** | **missing** |
| `walkPathTotal` (total route) | missing | missing |
| `coordRay` | missing | missing |
| `offsetCoordUpTo` | missing | missing |
| `transportCoord` (total step + frame) | n/a | `stepWalker` ✓ |
| *checked step + frame* | n/a | **does not exist at any layer** |

Two shapes fall out of that table, and they are the whole finding.

### 1. Nothing moves a focus and reports failure

`traceOffset` and `tracePath` are the only checked operations in the pointing
family, and both discard the position they just computed. There is no

```haskell
offsetFocus :: Delta … -> FocusedGrid cs a -> Maybe (FocusedGrid cs a)
```

anywhere, and no optic that is one either — `focus` is a plain `Lens'` into
the `Coord`, so composing it with movement gets you the *total* step or
nothing at all.

Three independent consumers rebuild it by hand, each differently:

- **`maze/src/Maze/Model.hs`** writes `traceTo p fg = (\c -> (c, indexGrid
  (focusedGrid fg) c)) <$> walkPath (pos fg) p`, with the comment "`tracePath`
  gives the second half and `walkPath` the first; this is the pair, from one
  walk, because every caller here wants both." That comment is the issue,
  stated by a consumer.
- **`../aoc/src/2016/02.hs`** steps with `seeks (.+^ delta c)` and then rejects
  the step by looking at the *cell contents* — `if isNothing (extract next)
  then fg else next`, commented `-- It's a "wall", stay put`. Here the total
  step is the right one (`Clamped` is chosen deliberately, and staying put at
  the keypad's rectangular edge is the intended behaviour), so this is the
  weaker of the three. What it shows is that the call site needs *two*
  rejection mechanisms — the axis policy for the rectangle, a payload check for
  the missing keys — and the library hands it a `FocusedGrid` that can express
  neither as a step that failed.
- **`../aoc/src/2017/19.hs`** is a `Walker` in all but name — `type State n =
  (FocusedGrid '[Clamped n, Clamped n] Cell, Dir)` plus a step that turns and
  then moves — and never touches `Walker`. Its step is `seeks (shift d)`, i.e.
  `.+^` on a `Clamped` axis. Its termination condition is walking onto an
  `Empty` cell; at the true grid edge the clamp puts it back on the cell it was
  already on, which is not `Empty`, so the `unfoldr` never terminates. The
  solution is correct only because the input is padded.

That last one is the library's own thesis failing at a call site: the silent
clamp is the failure mode the coordinate types exist to prevent, and the
pointing layer offers no way to avoid it.

### 2. A `Walker` cannot exist on an `Ordinal` axis

```haskell
data Walker cs a = Walker
    { walkerGrid    :: FocusedGrid cs a
    , walkerHeading :: Diff (Coord cs) }
```

and `Diff (Coord cs) = Delta (MapDiff cs)`. `Ordinal` deliberately has no
`AffineSpace` instance, so `MapDiff` is stuck on it. Confirmed:

```
ghci> :kind! Diff (Coord '[Ordinal 5, Ordinal 5])
= Delta [Diff (Ordinal 5), Diff (Ordinal 5)]
```

That type has no values, so the heading field cannot be filled. This is not
"a walker cannot move in a window" — it is that a walker cannot be *written
down* in a window. Every restriction lands on `Ordinal` by the rule
`sized-grid-mbh0` established, so no `gridWindows`, `gridTiles`, `shrinkGrid`,
`takeGrid`, `sliceGrid` or `splitHigherDim` result can hold a walker at all.

`sized-grid-i0ob.2` diagnosed the same mistake for `offsetCoord` — checked
movement is indexed by `Diff`, an affine notion, when what it needs is one
signed step count per axis — and deferred the `Walker` half here. It is the
same fix: index the heading by `i0ob.2`'s `MapStep cs` (every axis to `Int`)
rather than `MapDiff cs`. Under `AllDiffSame Int cs`, which every call site
that compiles today already carries, the two are the *same type*, so this is
source-compatible; `Automata.Ant`'s `MapDiff cs ~ '[Int, Int]` constraint
becomes unnecessary rather than broken.

## The missing primitive: a checked step that carries a frame

`transportCoord` is total. Its checked counterpart does not exist, and it is
the operation a walker in a window needs. It is buildable from `IsCoord`
methods alone — `offsetIsCoord` and `axisFrameFlipsIsCoord`, neither of which
mentions `Diff` — so unlike `transportCoord` it works on `Ordinal`:

```haskell
transportCoordMaybe ::
     CheckedTransportList cs
  => Coord cs -> Delta (MapStep cs) -> Maybe (Coord cs, Delta (MapStep cs))
```

Spiked against the real library — `spike/ylhl-pointing/Spike.hs`, which has
the instructions for reproducing everything below. One walker, started at row
1 column 0 with heading `(0, 1)`, run to exhaustion on a 5×5 board of each
policy — positions and the heading's second component:

```
Ordinal   : [((1,0),1),((1,1),1),((1,2),1),((1,3),1),((1,4),1)]
Clamped   : [((1,0),1),((1,1),1),((1,2),1),((1,3),1),((1,4),1)]
Periodic  : [((1,0),1),((1,1),1),((1,2),1),((1,3),1),((1,4),1),((1,0),1), …
Reflective: [((1,0),1),((1,1),1),((1,2),1),((1,3),1),((1,4),1)]
Reflect101: [((1,0),1),((1,1),1),((1,2),1),((1,3),1),((1,4),-1),((1,3),-1), …
```

`Ordinal` stops at the window's own edge and never wraps to the source's
policy; `Clamped` reports the wall instead of clamping onto it; `Periodic`
wraps forever. All three are right, and all three are unreachable today.

### The thing the spike found

`Reflective` stops at the wall and `Reflect101` turns around. Two policies
that are the same idea modulo where the mirror sits disagree about what a
*checked* step means.

The cause is a documented tie-break. `Reflect101`'s mirror is the edge cell
itself, and `mirrorAt` resolves the ambiguous fixed point `r == m` as
reflected — `-- @r == m@ is the far mirror's fixed point, genuinely ambiguous
in parity; @>=@ (not @>@) resolves it as reflected`. So on a 5-cell axis:

| | from 3, step +1 | from 4, step +1 |
| --- | --- | --- |
| `Reflective` total | 4, no flip | 4 (in place), flip |
| `Reflective` checked | `Just 4` | `Nothing` |
| `Reflect101` total | 4, **flip** | 3, flip |
| `Reflect101` checked | `Just 4` | `Nothing` |

`Reflect101` is the only axis in the library where a step the bounds check
*accepts* also reports a frame flip — 5 cases on a 5-cell axis, all of them
landing exactly on the mirror cell. The total walker cannot see it (from cell
4 both headings lead to 3), so nothing has ever depended on it. A checked
walker can, and it turns around one cell early.

The right reading is the simpler one, and it is a law worth stating on
`IsCoord`:

> **A checked step that succeeds has not hit a wall, so the frame does not
> turn.** `offsetIsCoord c d == Just c'` implies `not (axisFrameFlipsIsCoord
> c d)`.

Every axis in the library satisfies it except `Reflect101` at its fixed point.
With the law in force, `transportCoordMaybe` collapses to `offsetCoord` with
the heading passed through unchanged — no new fold, no new class, and
`Reflective` and `Reflect101` agree again. The `Reflect101` tie-break is
filed separately, because it is a defect in an existing function rather than
part of this design.

**Resolved, `sized-grid-c0s9`.** `mirrorAt` now resolves both fixed points as
*not* reflected (`>` rather than `>=`), which leaves `(.+^)` bit-for-bit
unchanged -- at `r == m` the two branches already agreed, since
`period - m == m` -- and makes the flag mean "mirrors crossed" rather than
"mirrors reached". The law above is stated on `axisFrameFlipsIsCoord` and
tested exhaustively for every axis type, and the spike's last row now matches
`Reflective`. So `transportCoordMaybe` is `offsetCoord` with the heading
passed through, as recommended below.

## What not to build

- **No `Path`-driven `Walker`.** A walker's route is generated by its own
  heading; a `Path` is a route given in advance. Composing them means
  overriding the heading each step, which is `walkPath` on the position with
  the walker along for the ride — nothing a walker contributes.
- **No heading-polymorphic `Walker cs h a`.** `../aoc/src/2017/19.hs` carries
  a `Dir` enum, which looks like an argument for it. But the heading has to
  *be* a displacement for transport to have anything to say: `transportCoord`
  hands back a reversed heading, and it can only do that if the heading is the
  thing it reversed. A domain enum is a view of a displacement, not a
  replacement for one — `Automata.Ant` writes `turnRight`/`turnLeft` directly
  on `Delta '[Int, Int]` and loses nothing.
- **No `Comonad (Walker cs)`.** `extend` would have to produce a heading per
  cell, and there is only one heading. Its absence is correct.
- **No unification with `FocusedGrid`.** `Walker` is `FocusedGrid` plus a
  frame, and the frame is exactly what `transportCoord` acts on. Two types,
  one of which contains the other, is already the right shape.
- **No typeclass over the family.** The two members differ by one field, and
  every operation on them is a lifting of a `Coord` operation. There is no law
  they share that `Coord` does not already state.

## Recommendation

**API addition, small and mechanical.** Not a new abstraction — the model is
`Coord`'s and it is finished. Four pieces:

1. **State the model** (docs-only, unblocked, worth doing alone). The README's
   design thesis says a movement operation is checked or licensed-total, and
   the corollary "a restriction destroys the boundary policy, a pointing
   preserves it" says what pointing does to the *policy*. Neither says what
   the pointing family *offers*: the same movement vocabulary, at a position
   with a payload and optionally a frame. Say it, and put the table above in
   `Data.Grid.Sized.Focused`'s (currently absent) module header.

2. **Unstick the heading** — `walkerHeading :: Delta (MapStep cs)`. Depends on
   `sized-grid-i0ob.2` landing `MapStep`. Source-compatible by the same
   argument `i0ob.2` makes for `offsetCoord`, and it is what lets a walker
   exist in a window.

   **`MapStep` has landed, `sized-grid-i0ob.2`.** It is exported from
   `Data.Grid.Sized.Coord`, and `offsetCoord`, `coordRay`, `offsetCoordUpTo`,
   `Path`, `walkPath`, `traceOffset`, `tracePath` and `walkEverywhere` are all
   indexed by it and all work on an `Ordinal` axis. Source-compatible as
   predicted: no call site in this repo or in `../aoc` changed. The heading
   itself is still `Diff (Coord cs)` — that is this piece, and it is now
   unblocked.

3. **State the checked-step law on `IsCoord`** and fix `Reflect101`'s
   fixed-point tie-break so it holds. Filed separately; blocks piece 4 only
   in the sense that piece 4's semantics are undefined until it is settled.

4. **Lift the rest of the vocabulary**, position-preserving:

   ```haskell
   offsetFocus  :: Delta (MapStep cs) -> FocusedGrid cs a -> Maybe (FocusedGrid cs a)
   walkFocus    :: Path cs           -> FocusedGrid cs a -> Maybe (FocusedGrid cs a)
   focusRay     :: Delta (MapStep cs) -> FocusedGrid cs a -> [FocusedGrid cs a]
   stepWalkerWithin :: Walker cs a -> Maybe (Walker cs a)
   walkerTrail      :: Walker cs a -> [Walker cs a]
   ```

   `traceOffset` and `tracePath` stay, and become `fmap extract . offsetFocus`
   and `fmap extract . walkFocus` — that they fall out as one-liners over the
   new pair is the check that the layering is right, and it keeps every
   existing call site.

Pieces 1 and 3 are worth landing whatever happens to 2 and 4. Piece 4 without
piece 2 still helps every `Clamped`/`Periodic` consumer above; it just leaves
windows out.

## Not covered here

The restriction family is `sized-grid-d9o9`, settled. The `Ordinal` checked
step at the `Coord` layer is `sized-grid-i0ob.2` and this document assumes its
`MapStep` diagnosis rather than re-deriving it.
