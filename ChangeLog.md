# Revision history for sized-grid

## 0.3.0.0 -- NOT PUBLISHED

Correctness release. Every change below is breaking, and each one turns a
silently-wrong result — or a name that invited one — into either a rejected
value or a type error.

* `HardWrap` is renamed to `Clamped`, and `SizedGrid.Coord.HardWrap` to
  `SizedGrid.Coord.Clamped`. The type clamps out-of-range values to the nearest
  end; it has never wrapped. `Periodic` is the one that wraps.

  The name was read the wrong way twice, both times producing a wrong answer
  rather than an error: `(.-.)` was written to clamp negative differences to
  `0`, which broke the `AffineSpace` law for every pair with `a < b`, and a
  downstream Manhattan-distance function silently returned `0`. Both are fixed
  below, but the name that suggested them is fixed here.

  **Migration:** `HardWrap` becomes `Clamped` and the field `unHardWrap`
  becomes `unClamped`; the behaviour is unchanged. No deprecated alias is
  provided, because the point of the rename is that the old name reads as a
  claim about behaviour that was never true.

* The neighbourhood API is replaced. `moorePoints` and `vonNeumanPoints` are
  **removed**, and `offsetCoord`, `neighbours`, `mooreNeighbours` and
  `vonNeumannNeighbours` take over.

  The old pair had three problems, and every observed caller worked around all
  three. They included the centre, so callers wrote `filter (/= c)`. They were
  built on `(.+^)`, which clamps on a bounded coord, so an off-grid offset
  folded back onto an edge cell and callers wrote `nubOrd` — at a corner of a
  `Clamped 5` grid, `moorePoints 1` returned nine results of which four were
  distinct. And they demanded up to eight constraints, so some callers gave up
  and hand-rolled the whole thing.

  The replacements exclude the centre, never duplicate, and ask only for
  `All IsCoordLifted cs`. They also work on `Ordinal` axes, which the old ones
  could not: `Ordinal` has no `AffineSpace` instance, so `All AffineSpace cs`
  was unsatisfiable.

  **Migration:** `filter (/= c) (moorePoints 1 c)` becomes `neighbours c`, and
  a `nubOrd` in the same pipeline can go with it. `moorePoints n c` becomes
  `mooreNeighbours n c` and `vonNeumanPoints n c` becomes
  `vonNeumannNeighbours n c` (note the second `n` in von Neumann), both of
  which now exclude the centre. Removal rather than deprecation is deliberate:
  the behaviour changed under an otherwise compatible signature, so the names
  had to go for the change to be a compile error rather than a silent one.

* New: `offsetIsCoord`, a method of `IsCoord`, and `offsetCoord` over a whole
  `Coord`. These are the checked counterpart of `(.+^)`: they return `Nothing`
  when the offset leaves the space rather than clamping back into it.

  Each axis applies its own boundary policy, so on a
  `Coord '[Clamped 5, Periodic 5]` the torus axis wraps while the bounded axis
  refuses. The default for a coord type is the bounds check; `Periodic`
  overrides it and is total.

  `(.+^)` is unchanged. `AffineSpace` requires it to be total, and clamping is
  its honest total form; `offsetCoord` is the operation for callers who need to
  be told about the edge. `offsetCoord` takes the same `Diff (Coord cs)`, so it
  is a drop-in wherever that distinction matters.

* `Diff (Coord cs)` is now `Coord (MapDiff cs)` — a coordinate again, holding
  one `Diff` per axis — instead of an n-tuple. The `CoordDiff` type family and
  its seven hand-written instances are **removed**.

  `CoordDiff` was an open family with one `type instance` per arity, and it
  stopped at six. That ceiling was real: a seven-axis `Coord` had no `Diff`, so
  no `AffineSpace` instance and no `offsetCoord`, and the only fix available to
  a caller was an orphan instance plus a seven-tuple for something the library
  should supply. `MapDiff` already recursed, so the replacement needs no
  per-arity code and has no ceiling.

  This follows `manifolds`, where `Needle` is an associated type and a product
  gets its own structurally. Reusing `Coord` rather than introducing a new
  product type means the displacement inherits `(:|)`, `EmptyCoord`, `Show`,
  `Eq`, `Ord`, `AdditiveGroup` and `Random` from the instances that already
  exist.

  It also removes `IsProductType (CoordDiff cs) (MapDiff cs)` from every
  signature that offsets, including `offsetCoord`'s, which is why `generics-sop`
  no longer appears in the context of functions that have no other reason to
  mention it. The `AffineSpace` instance now asks for
  `(All AffineSpace cs, All AdditiveGroup (MapDiff cs))`: one constraint where
  there were two.

  **Migration:** a tuple literal is no longer a displacement. `c .+^ (-1, -1)`
  becomes `c .+^ ((-1) :| (-1) :| EmptyCoord)`, and destructuring
  `let (dx, dy) = a .-. b` becomes `let (dx :| dy :| EmptyCoord) = a .-. b`.
  Where the tuple reads better, the new `coordFromTuple` and `coordToTuple`
  convert explicitly: `c .+^ coordFromTuple (-1, -1)`. Those two are the only
  signatures left carrying `IsProductType`, and they are arity-generic — one
  function covers a pair and a seven-axis coord — so they add back no per-arity
  code. The conversion is deliberately *not* an implicit part of the class;
  that is what put the constraint in every consumer's context to begin with.

* New: the distances the library already computed and then threw away.
  `coordDistance` (Chebyshev, the metric `mooreNeighbours` is a ball in),
  `coordManhattan` (the metric `vonNeumannNeighbours` is a ball in),
  `axisDistance` for a single axis and `axisDistances` for the per-axis
  breakdown. `axisSteps` and `stepsWithin` are now exported too — the latter is
  the general primitive both neighbourhood functions are one-liners over, and
  the one a BFS-shaped consumer wants because it carries the distances.

  This is purely additive. Nothing that existed changed behaviour.

  `axisSteps` had worked out the true per-axis distance since the neighbourhood
  rewrite, taking the shorter route where a torus axis offers two, and
  `stepsWithin` summed it across axes; then `mooreNeighbours` and
  `vonNeumannNeighbours` both discarded the number and no export exposed it. A
  correct, wrap-aware, mixed-boundary-policy distance was sitting in the library
  where no caller could reach it.

  The scalar behind them is `axisDistanceIsCoord`, a new method of `IsCoord`,
  following `offsetIsCoord`: the default measures straight, which is right for
  an axis with real edges, and `Periodic` overrides it to take the shorter way
  round. So on a `Coord '[Clamped 5, Periodic 5]` the bounded axis measures
  straight while the torus axis wraps, in the same coordinate — the answer a
  caller cannot easily write by hand, and the reason this is worth exporting
  rather than leaving every consumer to reimplement it against `natVal`.

  The distance is *not* built on `stepsWithin`, which is radius-bounded and
  would make an O(d) question cost O(r^d). It is a second implementation of the
  "shorter route wins" rule, so the test suite pins it to the enumeration:
  `axisDistance c v` must equal the distance `axisSteps` records for every `v`
  it reaches, and both neighbourhood functions must be exactly the balls of the
  corresponding metric.

* New: boundary detection. `axisBoundary` says which end of its axis a
  coordinate sits at, `axisBoundaries` reports every axis of a `Coord`,
  `onBoundary` and `isCorner` are the two folds of that, and `interiorCoords`
  enumerates the coordinates that are on no edge. The result type is
  `Maybe Extremum` with `Extremum = AtMin | AtMax`, not a `Bool`: a caller that
  has to *act* on the edge needs to know which end it met.

  This is purely additive. Nothing that existed changed behaviour.

  The library had no way at all to ask "is this coordinate on an edge", so every
  consumer hand-rolled it against `natVal` — and the hand-rolled version is
  wrong the moment an axis is `Periodic`, because it finds four corners on a
  torus, which has none.

  The scalar behind them is `axisBoundaryIsCoord`, a new method of `IsCoord`,
  following `offsetIsCoord` and `axisDistanceIsCoord` exactly: the default is
  the bounds check, which is what an axis with real edges wants, and `Periodic`
  overrides it to `Nothing` everywhere. So `isCorner` is `False` on an
  all-`Periodic` coord by construction rather than by a special case, and on a
  `Coord '[Clamped 5, Periodic 5]` there are no corners either — one axis never
  ends.

  Agreement with `offsetIsCoord` is the law and a property test, as it is for
  `axisDistanceIsCoord`: a coordinate is `AtMin` exactly when stepping down
  leaves the space and `AtMax` exactly when stepping up does. A one-cell axis is
  the one place both hold, and it answers `AtMin`. The empty `Coord '[]` is
  neither `onBoundary` nor a corner: a space with one point has no edge, and a
  vacuous `True` from `isCorner` would break `isCorner c ==> onBoundary c` on
  the one coordinate where it is easiest to get wrong.

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
  `Clamped`, no longer throw `Maybe.fromJust: Nothing`. The derived `enumFrom`
  counted past `maxBound`.

* `Clamped`'s `(.-.)` now returns a true signed displacement instead of
  clamping to `[0, n-1]`. The clamp remains in `(.+^)`. This restores the
  `AffineSpace` law `b .+^ (a .-. b) == a`, which `Clamped` previously
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

* New: `coordSpaceSize`, the number of coordinates in a `Coord cs` — that is,
  `MaxCoordSize` as a value — and `coordFromPosition`, the inverse of
  `coordPosition`. `coordSpaceSize` asks only for `All IsCoordLifted cs` rather
  than `KnownNat (MaxCoordSize cs)`, so it is available in the indexed
  traversals, which is what lets `tabulate` size its vector up front instead of
  growing it by doubling.

* Sizes are passed as required type arguments rather than as `Proxy` values, so
  `takeGrid 2 g` replaces `takeGrid (Proxy @2) g`. No function in the public API
  takes a `Data.Proxy.Proxy` any more.

  The rule the API now follows: a type argument is *visible* when inference
  cannot recover it, and *absent* when it can.

  | was | now |
  | --- | --- |
  | `takeGrid (Proxy @2) g` | `takeGrid 2 g` |
  | `dropGrid (Proxy @2) g` | `dropGrid 2 g` |
  | `maxCoordSize (Proxy @(Periodic 10))` | `maxCoordSize 10` |
  | `maxCoord (Proxy @10) :: Periodic 10` | `maxCoord :: Periodic 10` |
  | `asSizeProxy c $ \(p :: Proxy m) -> ...` | `reifyCoord c $ \m -> ...` |
  | `reifyOrdinal o $ \(p :: Proxy m) -> ...` | `reifyOrdinal o $ \m -> ...` |

  `sCoordSized` is deleted; it existed only to turn one `Proxy` into another.

  `maxCoordSize` is no longer a method of `IsCoord`. It never depended on the
  coord type and no instance ever overrode it, and none could sensibly:
  `asOrdinal` is an `Iso' (c n) (Ordinal n)`, so a lawful coord of size `n` has
  exactly `n` inhabitants. Keeping it a method would have meant either an
  ambiguous class variable or writing the coord type out as a visible argument,
  which collides with the data constructor of the same name.

  `maxCoord` takes no argument at all: its result type already fixes both the
  coord type and the size.

  `asSizeProxy` is now `reifyCoord` — the name went with the `Proxy` it was
  named for, and it does to a coord what `reifyOrdinal` does to an `Ordinal`.

  **This raises the minimum to GHC 9.10**, where `RequiredTypeArguments`
  arrived. `tested-with` and the `base` lower bound have been updated; 9.8 is no
  longer supported.

* The per-module `LANGUAGE` headers are gone wherever `GHC2024` and the
  package's `default-extensions` already implied them — 64 lines across six
  modules, including all 17 of `SizedGrid.Internal.Grid`'s and 16 of
  `SizedGrid.Coord`'s. What remains is only what is genuinely per-module, which
  is `AllowAmbiguousTypes` in three modules and `OverloadedStrings` in one.

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
