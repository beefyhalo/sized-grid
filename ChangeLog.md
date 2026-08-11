# Revision history for sized-grid

## 0.3.0.0 -- NOT PUBLISHED

Correctness release. Every change below is breaking, and each one turns a
silently-wrong result into either a rejected value or a type error.

* `Ordinal` is now a newtype over `Int` rather than a GADT carrying the value
  as a type-level `Nat`. The old representation put a `Proxy` and two
  `KnownNat` dictionaries in every value and called `someNatVal` on every
  construction, so a single coordinate addition allocated a type-level natural.
  Coordinate arithmetic is now roughly twice as fast and allocates half as
  much; `toEnum`/`fromEnum` allocates 60% less.

  **Migration:** the `Ordinal` constructor is no longer exported — the range
  invariant is now maintained by this module rather than by the type checker,
  so it cannot be. Build with `numToOrdinal` (checked) or `unsafeOrdinal`
  (unchecked, `assert`-guarded, precondition documented), and read with
  `ordinalToInt` or `ordinalToNum`. `reifyOrdinal` recovers the value as a
  type-level `Nat` for the one case that needs it.

  Three signatures gained constraints the GADT used to smuggle in inside its
  values: `asSizeProxy` requires `KnownNat n`, `maxCoord` requires `1 <= n`
  (there is no maximum of a `c 0`; it was `fromJust` on `Nothing`), and the
  `ShrinkableGrid (c x ': cs)` instance requires `KnownNat x`. `Show (Ordinal m)`
  requires `KnownNat m`, as it must to print the size.

* `Periodic`'s `Enum` instance wrapped modulo `n - 1` instead of `n`, so
  `toEnum 2 :: Periodic 3` was `0` and `fromEnum . toEnum` was not the identity
  on the type's own range.

* `[minBound ..]` and `[minBound, x ..]` on an `Ordinal`, and so on a
  `HardWrap`, no longer throw `Maybe.fromJust: Nothing`. The derived `enumFrom`
  counted past `maxBound`.

* `HardWrap`'s `(.-.)` now returns a true signed displacement instead of
  clamping to `[0, n-1]`. The clamp remains in `(.+^)`. This restores the
  `AffineSpace` law `b .+^ (a .-. b) == a`, which `HardWrap` previously
  violated for every pair with `a < b`.

  **Migration:** code that relied on the clamp to compute an absolute value —
  the idiom `(a .-. b) + (b .-. a)` — now gets `0`. Use `abs` on the
  difference instead.

* `FromJSON (Grid cs a)` now validates the length at every dimension and fails
  on a mismatch. Previously a short, long or ragged array decoded to a `Grid`
  whose vector disagreed with its type, which made `index` throw and `(<*>)`
  silently truncate. Its constraints are now `(AllGridSizeKnown cs, SListI cs)`,
  matching `ToJSON`.

* `Grid` is now abstract: the `Grid` constructor and the `unGrid` field are no
  longer exported. Anyone could previously build a `Grid` whose vector length
  disagreed with `MaxCoordSize cs`, which is the one invariant this library
  exists to enforce — the `FromJSON` and `takeGrid` bugs above were both
  instances of it. Every module now has an explicit export list, and
  `-Wmissing-export-lists` is on so a module added later cannot quietly
  re-expose what it imports.

  **Migration:**

  | was | now |
  | --- | --- |
  | `unGrid g` | `gridVector g` |
  | `Grid v` (length checked at runtime) | `gridFromVector v :: Maybe (Grid cs a)` |
  | `Grid v` (length known, not checkable) | `unsafeGridFromVector v`, from `SizedGrid.Grid.Unsafe` |
  | `Grid . V.scanl1' f . unGrid` | `scanl1Grid f` |

  `SizedGrid.Grid.Unsafe` is deliberately not re-exported by `SizedGrid`, so
  opting out of the invariant shows up in an import list. Reading is not unsafe
  and needs no such import: `gridVector` is exported normally.

* New: `gridFromVector`, the checked counterpart to the old constructor;
  `gridVector`, the read-only accessor; and `scanl1Grid`, a length-preserving
  row-major scan. `scanl1Grid` exists because prefix sums were the one thing
  real code could not express without reaching through the abstraction —
  compose it with `mapLowerDim` to scan each row independently.

* `takeGrid` and `dropGrid` now require `n <= m`.

* `splitHigherDim`'s second component is now `Grid (c (x - y) ': as) a`. It was
  a free type variable, so the caller could annotate the remainder with any
  size at all and get a grid that did not match.

* The `ShrinkableGrid` window constraint is now `x + z <= y + 1`. It was
  `z <= x - y + 1`, which had the number of positions and the source size the
  wrong way round; it only ever typechecked because the sole test used
  `x == y`.

* `gridWindows` is now `gridTiles`. It cuts a grid into *disjoint tiles* along
  its outermost axis and always did; the old name promised a sliding window,
  which is a different operation and still does not exist.

  **Migration:** rename the call. The behaviour is unchanged.

* New `zipLowerDim`, for tiling the second axis:

  ```haskell
  zipLowerDim :: AllSizedKnown as
              => (Grid as x -> [Grid bs y]) -> Grid (c ': as) x -> [Grid (c ': bs) y]
  ```

  `mapLowerDim` combines its per-sub-grid results with `traverse`, so at
  `f ~ []` it is the list applicative: a *cartesian product*. `mapLowerDim
  gridTiles` on a 9x9 board yields 9^9 = 387,420,489 grids, one for every way
  of picking a cell from each row, rather than the 9 columns the caller meant —
  which in practice means it never terminates. `zipLowerDim` zips positionally
  and gives the 9. `mapLowerDim`'s haddock now says which applicatives are
  safe, and both functions are covered by tests.

* `splitVectorBySize` rejects a chunk size of zero, which previously looped
  forever taking empty prefixes.

* Builds with GHC 9.8 through 9.14 via a nix flake; `stack.yaml` and Travis
  configuration removed.

* The `sudoko`, `gameOfLife` and `ising-example` programs build again. They had
  been pinned to lts-11.2/lts-12.7 (GHC 8.2/8.4) with `base < 4.13` bounds, so
  no compiler in use could build them. All three are now packages of the root
  `cabal.project`, so `cabal build all` covers them and they cannot rot
  unnoticed again. `sudoko` gained a `main` — it previously ended in
  `main = undefined` — that prints the board with all nine of its rows, columns
  and squares and reports whether it is solved or invalid.

## 0.2.0.0 -- NOT PUBLISHED

* _WrappedCoord is now an Iso

* IsCoord is now of kind Nat -> *. Introduced IsCoordLifted. This is unfortunatly a breaking change

## 0.1.1.6 -- 2018-11-21

* Reduced bound on generics-sop

## 0.1.1.5 -- 2018-11-20

* Changed test suite to use QuickCheck

## 0.1.1.3 -- 2018-11-14

* Version bumps

## 0.1.1.0 -- 2018-05-10

* Added Field instances for coord
* Added ways of manipulating coords

## 0.1.0.0  -- 2018-04-18

* First version. 
