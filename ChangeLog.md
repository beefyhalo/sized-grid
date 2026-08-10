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

* The `sudoko`, `gameOfLife` and `ising-example` programs build again. They had
  been pinned to lts-11.2/lts-12.7 (GHC 8.2/8.4) with `base < 4.13` bounds, so
  no compiler in use could build them. All three are now packages of the root
  `cabal.project`, so `cabal build all` covers them and they cannot rot
  unnoticed again. `sudoko` gained a `main` — it previously ended in
  `main = undefined` — that prints the board and its first row, column and
  square.

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
