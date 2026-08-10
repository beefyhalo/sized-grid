# Revision history for sized-grid

## 0.3.0.0 -- NOT PUBLISHED

Correctness release. Every change below is breaking, and each one turns a
silently-wrong result into either a rejected value or a type error.

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

* Builds with GHC 9.8 through 9.14 via a nix flake; `stack.yaml` and Travis
  configuration removed.

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
