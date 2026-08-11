# Safe neighbour offset

Design for `sized-grid-7gs`. Written 2026-08-11.

## Problem

The most-wanted operation on a bounded grid is "move by a delta, or fail if that
leaves the grid". The library has no such function, so every observed consumer
builds it out of parts, and each one builds it differently.

`../aoc/src/2024/04.hs:63-80` widens the coordinate type to `n + 1`, offsets
there, then narrows back:

```haskell
i, j, k :: Coord '[Periodic (n + 1), Periodic (n + 1)]
i = strengthenCoord c .+^ d
j = i .+^ d
k = j .+^ d
... mapMaybe (traverse weakenCoord) ...
    \\ plusNat @n @1 \\ leTrans @1 @n @(n + 1) \\ plusMonotone2 @n @0 @1
```

Three lines of type-level lemmas to express "neighbour, or `Nothing`". It is
also not a bounds check: it works only because `Periodic`'s wrap-around at
`n + 1` happens not to collide for small deltas.

`../aoc/src/2015/18.hs:64-66` and `../aoc/src/2018/18.hs:78-80` give up on the
library and hand-roll the whole thing with explicit `nr >= 0 && nr < nVal`
tests.

`../aoc/src/2025/04.hs:62` uses the library function and then repairs the
result:

```haskell
experiment (filter (/= coord) . nubOrd . moorePoints 1) g
```

Both repairs are necessary. `moorePoints` includes the centre, so the `filter`
is real; and it is built on `(.+^)`, which clamps on `HardWrap`, so every
off-grid offset folds back onto an edge cell and the `nubOrd` is real too.
Measured on `HardWrap 5 x HardWrap 5`:

```
moorePoints 1 (0,0) -> 9 results, 4 distinct   <- 5 duplicates
moorePoints 1 (2,2) -> 9 results, 9 distinct
```

Dedup cannot be the caller's job when the clamp is what created the duplicates.

## Thesis

This library's organising idea, and the thing that distinguishes it from every
other grid library, is that **the coordinate type is the boundary policy**.
`Periodic` is a torus, `HardWrap` is a bounded space.

The code does not currently commit to that idea. This design states the rule it
should follow:

> Any operation that can leave the space is either total because the type says
> how to come back, or it returns `Maybe`. Nothing clamps silently.

`(.+^)` is the total form. This design adds the checked form.

## Design

### The class method

`IsCoord` gains one method, named for the existing `weakenIsCoord` /
`strengthenIsCoord` pattern: the `…IsCoord` suffix marks the per-axis
counterpart of a `Coord`-level function.

```haskell
class IsCoord c where
  ...
  -- | Offset by a signed displacement, or 'Nothing' if that leaves the space.
  offsetIsCoord :: (KnownNat n, 1 <= n) => c n -> Integer -> Maybe (c n)

  default offsetIsCoord :: (KnownNat n, 1 <= n) => c n -> Integer -> Maybe (c n)
  offsetIsCoord c d =
    review asOrdinal <$>
      numToOrdinal (toInteger (ordinalToInt (c ^. asOrdinal)) + d)
```

The default is the bounds check. It is a one-liner because `numToOrdinal`
already returns `Maybe` and already goes via `Integer`, so a displacement wider
than `Int` cannot wrap into range.

| Instance | `offsetIsCoord` | Rationale |
|---|---|---|
| `Ordinal` | default | The raw index. Off-grid does not exist. |
| `HardWrap` | default | A bounded space. `Nothing` at the edge is the checked reading of "there is nothing past the edge". |
| `Periodic` | `\c d -> Just (c .+^ d)` | A torus. Total, and reuses the tested `AffineSpace` instance. |

The method carries `1 <= n` because `AffineSpace (Periodic n)` requires it.
This costs nothing: `IsCoordLifted` already guarantees `1 <= CoordNat x` for
every axis, so every consumer has the evidence.

`(.+^)` is not changed. `HardWrap` keeps clamping there, because `AffineSpace`
requires a total operation and clamping is its honest total approximation.
`offsetIsCoord` is the checked counterpart, not a replacement.

### The `Coord` layer

```haskell
offsetCoord ::
     ( All IsCoordLifted cs
     , AllDiffSame Integer cs
     , IsProductType (CoordDiff cs) (MapDiff cs)
     )
  => Coord cs -> Diff (Coord cs) -> Maybe (Coord cs)

mooreNeighbours      :: All IsCoordLifted cs => Int -> Coord cs -> [Coord cs]
vonNeumannNeighbours :: All IsCoordLifted cs => Int -> Coord cs -> [Coord cs]

neighbours :: All IsCoordLifted cs => Coord cs -> [Coord cs]
neighbours = mooreNeighbours 1
```

`offsetCoord` takes `Diff (Coord cs)` so that it is a drop-in for `c .+^ d`.
`../aoc/src/2024/04.hs` already builds `d = (dy, dx)` tuples of exactly that
type.

`neighbours` exists as the radius-1 Moore alias because that is the
overwhelmingly common case — all four observed call sites — and ergonomics is
what this issue is about. There is deliberately no `neighbours4`: von Neumann
radius 1 gives four neighbours in 2D but six in 3D, so a count in the name
bakes in an assumption the type does not make. `vonNeumannNeighbours 1` is
explicit and correct in any dimension.

### Constraints

The neighbourhood functions ask for `All IsCoordLifted cs` and nothing else.
`vonNeumanPoints` currently asks for eight constraints:

```haskell
( Enum a, Num a, Ord a
, All Integral (MapDiff cs), AllDiffSame a cs, All AffineSpace cs
, Ord (CoordDiff cs), IsProductType (CoordDiff cs) (MapDiff cs) )
```

Building on `offsetIsCoord` rather than `(.+^)` deletes all of them. It also
makes the neighbourhood functions work on `Ordinal` axes, which `moorePoints`
never could: `Ordinal` has no `AffineSpace` instance, so `All AffineSpace cs`
was unsatisfiable for any coord containing one.

`offsetCoord` keeps `AllDiffSame Integer cs` and `IsProductType`, because it
takes a `Diff` and has to take it apart. The asymmetry is accepted: `Ordinal`
has no `Diff` at all, so the alternative is inventing one, which is out of
scope here.

### Generating a neighbourhood

Per axis, from centre value `c` and radius `r`:

1. Build `[(d, v) | d <- [-r .. r], Just v <- [offsetIsCoord c d]]`, in that
   ascending order of `d`.
2. Deduplicate on `v`, keeping the entry with the smallest `abs d`. Ties — two
   deltas of equal magnitude reaching the same value, which happens on a
   `Periodic n` axis when `2 * r >= n` — are broken toward the earlier, more
   negative `d`.

Values are compared as `ordinalToInt (v ^. asOrdinal)`, an `Int`, so the dedup
needs no `Eq` constraint on the axis type.

This yields, per axis, a list of `(dist, value)` pairs in which every `value` is
distinct and `dist` is the true minimal number of steps to reach it. On a torus
that is the *wrapped* distance, which is what makes `vonNeumannNeighbours`
correct there rather than accidentally right.

The centre is always present in each axis list, at `dist == 0`, because
`offsetIsCoord c 0 == Just c` under both policies.

Take the cartesian product across axes. Then:

- `mooreNeighbours r` keeps every combination. Each axis already moved at most
  `r`, which is the Chebyshev condition.
- `vonNeumannNeighbours r` keeps combinations whose `sum dists <= r`.
- Both drop the single combination in which every axis chose `dist == 0`, which
  is exactly the centre.

The product is taken in row-major order — first axis most significant, last axis
varying fastest — matching the layout `coordPosition` already documents. Result
order is therefore fully determined, which the tests rely on.

Two properties follow structurally rather than by repair:

- **No duplicates.** A cartesian product of per-axis-distinct sets is distinct.
  No `nubOrd`, and no `Ord (Coord cs)` constraint.
- **Centre excluded.** Identified by its distances, not by an equality test, so
  no `Eq (Coord cs)` constraint.

### Deliberate behaviour: von Neumann on a small torus

On a `Periodic n` axis with `2 * r >= n`, some values are reachable by a
shorter wrapped path than `abs d` suggests, so their recorded `dist` is the
wrapped one. `vonNeumannNeighbours r` therefore returns a *smaller* set than a
reading of "all deltas summing to at most r" implies.

This is correct — it is the true metric on a torus — and it is chosen, not
incidental. It is called out here because it is the one place where the design's
behaviour could surprise a reader, and it must be documented on the function.

### What is deleted

`moorePoints` and `vonNeumanPoints` are removed, not deprecated.

The rename is the safety mechanism. Their replacement changes behaviour
(the centre is excluded, off-grid is dropped, duplicates are gone) under what
would otherwise be a compatible signature, so keeping the names would let
`../aoc/src/2025/04.hs:62` keep compiling while its `filter` and `nubOrd`
silently became no-ops. Deleting the names turns every call site into a compile
error, which is the outcome we want.

Deprecation was considered and rejected: the package is 0.3.0.0 with one known
consumer, which we own, so a deprecation cycle is ceremony for an audience of
one, and it leaves the wrong functions callable in the meantime.

Removing `vonNeumanPoints` also retires its misspelling — von Neumann has two
n's.

## Consumers after the change

| Call site | Before | After |
|---|---|---|
| `../aoc/src/2015/18.hs:64` | 11-line hand-rolled `neighborsOf` | `neighbours` |
| `../aoc/src/2018/18.hs:78` | 11-line hand-rolled `neighborsOf` | `neighbours` |
| `../aoc/src/2025/04.hs:62` | `filter (/= coord) . nubOrd . moorePoints 1` | `neighbours` |
| `../aoc/src/2024/04.hs:63-80` | widen to `n + 1`, offset, narrow, 3 type-level lemmas | `offsetCoord`, three times |

`2024/04` additionally has to change its grid from `Periodic n` to the clamping
coord type. It is not a torus; it used `Periodic` only to make the `n + 1`
trick work. Under this design the coordinate type states the boundary policy, so
a puzzle grid with real edges must say so. That migration belongs to
`sized-grid-bb5`, not here.

## Testing

Property tests over `Periodic` and `HardWrap`, in two and three dimensions.

| Property | Scope |
|---|---|
| `offsetCoord c zeroV == Just c` | both |
| `offsetCoord c d >>= flip offsetCoord (negateV d) == Just c` | `Periodic` always; `HardWrap` whenever the first offset succeeds |
| `neighbours c` contains no duplicates | both |
| `neighbours c` does not contain `c` | both |
| `c' ∈ neighbours c ⟺ c ∈ neighbours c'` | both |
| `vonNeumannNeighbours r c ⊆ mooreNeighbours r c` | both |
| `length (neighbours c) == 8` on `Periodic n`, `n >= 3` | `Periodic` |
| `length (neighbours c)` is 3 at a corner, 5 on an edge, 8 in the interior, on `HardWrap n`, `n >= 3` | `HardWrap` |
| every `c' ∈ mooreNeighbours r c` satisfies `offsetIsCoord`-reachability on each axis within `r` | both |

One regression test pinned to the measurement recorded on `sized-grid-7gs`: on
`HardWrap 5 x HardWrap 5`, `neighbours (0,0)` is exactly three distinct
coordinates, where `moorePoints 1 (0,0)` returned nine with four distinct.

## Out of scope

- Renaming `HardWrap`, which clamps and does not wrap. Filed as
  `sized-grid-7d1`.
- Renaming the package and re-owning the metadata. Filed as `sized-grid-85n`.
- Migrating `../aoc` to the new API. Belongs to `sized-grid-bb5`.
- Changing `(.+^)` on `HardWrap`. It must stay total for `AffineSpace`.
- A grid-level neighbour accessor. The `Coord -> [Coord]` shape already composes
  with `experiment` on a `FocusedGrid`, which is how all four call sites use it.
