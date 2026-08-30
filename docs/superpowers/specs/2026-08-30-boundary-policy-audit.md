# Boundary-policy audit of the public API

Research for `sized-grid-lf3g`. Written 2026-08-30.

## What was asked

`sized-grid-lf3g` was filed on 2026-08-23 to ask whether the library is missing
any core concept relative to its own thesis, and named one candidate outright:
a checked, `Maybe`-returning movement primitive. It also asked for a broader
sweep — "safe edge traversal, explicit radius-checked neighbourhoods, and
whether the public API should expose a boundary-aware movement primitive rather
than requiring callers to reconstruct it".

## The rule being audited

From `README.md:9-29`:

> **The coordinate type is the boundary policy.**
>
> Any operation that can leave the space is either total because the type says
> how to come back, or it returns `Maybe`. **Nothing clamps silently.**

An operation violates the rule if it can leave the space, is total, and no axis
type in its signature licenses the way it comes back.

## The named candidate is closed

It was a real gap when filed and it is not one now.
`docs/superpowers/specs/2026-08-11-safe-neighbour-offset-design.md` designed it,
`sized-grid-7gs` landed it, and `sized-grid-i0ob.2` and `sized-grid-c0s9`
finished it. The concept is first-class across three layers:

| Concept | Where |
|---|---|
| checked step, per axis | `offsetIsCoord`, `src/Data/Grid/Sized/Coord/Class/Axis.hs:87` |
| checked step, per coord | `offsetCoord`, `src/Data/Grid/Sized/Coord/Neighbourhood.hs:31` |
| radius-checked neighbourhood | `mooreNeighbours`, `vonNeumannNeighbours`, `neighbours`, `stepsWithin`, `axisSteps` |
| safe edge traversal | `coordRay`, `offsetCoordUpTo`, `OffGrid`, `Path` / `walkPath`, in `Coord/Path.hs` |
| edge predicates | `axisBoundary`, `onBoundary`, `isCorner`, `interiorCoords`, in `Coord/Boundary.hs` |
| checked step at the grid layer | `traceOffset`, `tracePath`, `walkEverywhere`, in `Focused.hs` |

All three sub-questions lf3g listed are answered by code that exists. Checked
movement is indexed by `MapStep cs` rather than the affine `MapDiff cs`, so it
asks an axis only for a bounds check and therefore reaches `Ordinal` — which
means it works inside a window, where every restriction lands
(`sized-grid-mbh0`).

## Sweep of the rest of the surface

Every exposed module was read for the violating shape. The result is mostly
clean, and the clean cases are recorded because "we looked" is the finding:

| Operation | Can leave the space? | Verdict |
|---|---|---|
| `gridFromVector`, `gridFromList` | yes, wrong length | `Maybe`. Clean. |
| `coordFromPosition` / `_Position` | yes, out of range | `Maybe` / `Prism'`. Clean. |
| `weakenIsCoord` / `_Weakened`, `strengthenIsCoord` | yes, narrowing | `Maybe` one way, total the other. Clean. |
| `translated` (`Optics/Coordinate.hs:147`) | yes — an `Iso` that clamps is not an `Iso` | gated on `All Boundaryless cs`. Clean, and the sharpest instance of the rule anywhere in the library. |
| `(.+^)` on `Clamped` | yes | total, and `Clamped` is the licence. Clean by name. |
| `(.+^)` on `Reflective` / `Reflect101` | yes | total, and the bounce is the licence. Clean. |
| windows, tiles, `slice`, `prefix`, `suffix`, `lowerDim` | restrict | narrowed axis returns as `Ordinal`. Clean. |
| any axis type | — | none has a `Num` instance, so no `fromInteger` truncates silently. Clean. |
| `unsafeCoordFromPosition`, `Data.Grid.Sized.Unsafe` | yes | named. Clean. |
| `toEnum` on `Clamped` / `Reflective` / `Reflect101` | yes | **throws.** Already filed as `sized-grid-tgmj`. |
| `stepWalker` heading after a bounce | yes | already filed as `sized-grid-pc93`. |
| moving a focus and reporting failure | yes | already filed as `sized-grid-qbal`. |

Three things the sweep found that were not filed anywhere; each is now an issue.

### 1. The checked step has no lifted wrapper

`IsCoord` is kinded `Nat -> Type`, so each of its methods gets a wrapper at
kind `Type` for callers holding an axis value:

```haskell
axisBoundary   = axisBoundaryIsCoord   @(CoordContainer x) @(CoordNat x)
axisDistance   = axisDistanceIsCoord   @(CoordContainer x) @(CoordNat x)
axisFrameFlips = axisFrameFlipsIsCoord @(CoordContainer x) @(CoordNat x)
```

`offsetIsCoord` is the one method with a `Type`-kinded use and no wrapper.
`axisSteps` covers the radius form; the single step is not covered, so a caller
writes the two type applications by hand. Filed as `sized-grid-8hgl`.

### 2. `interiorCoords` has no complement

`Coord/Boundary.hs:47` enumerates the interior. The boundary — the set the
predicates `onBoundary` and `isCorner` describe — has no enumerator, so the
one half of the partition the module is named for is the half you write
yourself. Filed as `sized-grid-gfbc`.

### 3. One consumer never migrated

The 7gs design table lists four `../aoc` call sites that the new API replaces.
Three were migrated. `../aoc/src/2018/18.hs:70-80` still hand-rolls
`neighborsOf` with `nr >= 0 && nr < nVal && nc >= 0 && nc < nVal` — the exact
11 lines the design promised would become `neighbours` — and rebuilds each
result through `toEnum` on a `Clamped` axis, which is the throwing path in
`sized-grid-tgmj`. 7gs's close reason says "all five in-repo consumers
migrated", and it is right; `../aoc` is out-of-repo and was left behind.
Filed as `sized-grid-eca4`.

## Answer to the issue's acceptance criteria

3. **API omission, intentional omission, or a decision to formalize?** The
   omission lf3g named was a real API omission, and it has been filled. What
   remains at the coordinate layer is not omission but decision, and both
   decisions are already written down where a reader meets them:

   - `Clamped`'s `offsetIsCoord` refuses at the edge rather than clamping.
     Recorded in the 7gs design note: a bounded space's checked reading of "off
     the edge" is `Nothing`.
   - `Reflective` and `Reflect101` also refuse, even though their types do say
     how to come back. This looks like an inconsistency with `Periodic` and is
     not: the law at `Coord/Class/Axis.hs:119-124` fixes `offsetCoord` as "move
     without hitting a wall" and `transportCoord` as the total bouncing
     counterpart, so a checked step can carry a heading with no fold of its
     own. Settled by `sized-grid-c0s9`.

4. **Next step.** No new library concept. Close lf3g; the three findings above
   carry the remaining work, and the pointing layer's share of it is already
   `sized-grid-qbal`.
