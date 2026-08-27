# Revision history for grid-sized

## 0.1.0.0 -- NOT PUBLISHED

First release under the name `grid-sized`, and the version restarts here
because the name is new. The two `NOT PUBLISHED` sections below this one
(0.3.0.0 and 0.2.0.0) are this fork's own work under its old name and ship as
part of this release. The dated 0.1.x sections beneath those are upstream
`sized-grid`'s published history, kept for provenance — they name modules as
`SizedGrid.*` because that is what those modules were called at the time.

* **New.** `Data.Grid.Sized.Stencil.stencilGrid'` and
  `Data.Grid.Sized.Stencil.stencilFoldGrid'` (sized-grid-d6ng): the two
  stencil kernels with the result filled through a mutable vector written with
  `$!`, so each cell is forced where it is computed rather than left as a
  thunk for whoever forces the grid. Same table, same boundary policies, same
  lazily-produced neighbour list within a row, so a rule that stops early
  still stops early.

  New entry points rather than a change to the existing two, which keep their
  documented promise that a boxed grid may hold cells nothing ever looks at.
  On an unboxed grid the primed kernels can only tie, `Data.Vector.Generic.generate`
  having already forced; they are for boxed automaton loops. On 2,500 boxed
  cells x 100 generations: `stencilGrid` 30.6 ms / 210 MB against
  `stencilGrid'` 17.7 ms / 221 MB (1.7x, and 4.4x less copied), and
  `stencilFoldGrid` 17.5 ms / 21 MB against `stencilFoldGrid'` 6.12 ms / 32 MB
  at `-O1` or 3.13 ms / 5.7 MB at `-O2`.

  Note that last pair: `stencilFoldGrid'` is the one entry point in this
  library whose win depends on the optimisation level of the code that *calls*
  it -- 2.9x at `-O1`, 5.6x at `-O2` -- because it is `INLINE` and `-O1` does
  not unbox the accumulator through the fill's `ST` state. sized-grid-49hi
  reported 5.1x for this without qualification; its benchmark component was
  built at `-O2` and this library's is not.

* `Data.Grid.Sized.Stencil.stencilGrid` and
  `Data.Grid.Sized.Stencil.stencilFoldGrid` are `INLINE` rather than
  `INLINABLE` (sized-grid-v6ye). `INLINABLE` offers GHC an unfolding to
  specialise but does not oblige it to inline the body, and where GHC declined
  the caller's rule stayed a lambda-bound variable, so every neighbour and
  every accumulator was boxed through it. On the `50x50` benchmarks:
  `stencilFoldGrid` over a hundred generations of an unboxed grid goes from
  10.3 ms and 149 MB to 1.38 ms and 1.9 MB, and one pass from 48.3 us and
  479 KB to 42.5 us and 215 KB; `stencilGrid` allocates 11% less on both
  representations. No API, signature or semantic change. Object code got
  *smaller* at every call site in tree, so the usual code-size objection to
  `INLINE` does not apply here. `bench/baseline-ghc9.12.3-aarch64-darwin.csv`
  is regenerated to match.

* `Data.Grid.Sized.Optics` is now a facade over `Optics.Coordinate` (which
  also owns the `Field1`..`Field5` orphan instances for `Coord` and `Delta`),
  `Optics.Grid` and `Optics.FocusedGrid`, all newly exposed. Its export list is
  unchanged.

* `Data.Grid.Sized.Internal.Grid` is now an aggregator. Its 1288 lines are
  split into `Internal.Grid.Core` (the representation, its instances and the
  bulk operations), `Internal.Grid.Shape` (split, join, take, drop, slice,
  permute, the lower-dimension maps), `Internal.Grid.Axis` (`MapAxis` and the
  fibre machinery), `Internal.Grid.Windows` (shrink, tile, slide) and
  `Internal.Grid.Nest` (the axis-list recursion behind `collapseGrid`,
  `gridFromList` and the JSON instances). All are hidden, as the aggregator
  already was, and its exported symbols are unchanged.

* `Data.Grid.Sized.Coord` is now a facade. Its 1006 lines are split by domain
  into `Coord.Neighbourhood` (stepping, Moore, von Neumann), `Coord.Path` (rays
  and ordered walks), `Coord.Distance`, `Coord.Boundary`, `Coord.Transform`
  (reflection frames, weaken/strengthen), `Coord.Centre` (centred and punctured
  coordinates) and `Coord.Torus`, all newly exposed, plus a hidden
  `Coord.Internal` holding the representation and its instances. Nothing moved
  out of `Data.Grid.Sized.Coord`'s export list, so an existing import keeps
  working unchanged; the submodules are there for anyone who wants a narrower
  one.

* **Breaking.** A `Coord cs` *is* its row-major position: one `Int` in
  `[0, MaxCoordSize cs)` and nothing else, where it was an `NP I cs` — a
  boxed heterogeneous cons list with a `:*` cell, an `I` box and the axis
  newtype's own box per axis (sized-grid-adr.16).

  Nothing about what the type *means* changes. The axis list still indexes
  the type and still carries the boundary policy, so `Coord '[Clamped 5,
  Periodic 3]` means what it meant; the role stays nominal, so `coerce ::
  Coord '[Clamped 9] -> Coord '[Clamped 3]` is still rejected (now checked by
  `tests/compile-fail/CoordCoerceAcrossSizes.hs`, since with the axis list
  gone from the representation the annotation is the only thing shutting it);
  and `(:|)`/`EmptyCoord` are still the interface and still `COMPLETE`. The
  invariant moves to "the position is in range", maintained by the
  constructors — the same trade `Ordinal` made when it stopped being a GADT.

  What it buys, measured against the previous representation on the same
  machine, nothing slower: `tabulate` 2.3x, `indexGrid` 2.4x, `ifoldl'` 2.1x,
  a game-of-life step over `neighbours` 2.6x (16.4 MB to 2.6 MB), `extend`
  2.5x, a checked `offsetCoord` sweep 2.6x, a torus walk 3.7x (2.68 MB to
  20 bytes), `imap` 1.4x. `coordPosition` is now the identity, which is
  what most of that is: `index`, `tabulate` and every stencil reached it
  per cell.

  Migration, all of it caught by the type checker:

  - **Displacements are `Delta`, not `Coord`.** `Diff (Coord cs)` is now
    `Delta (MapDiff cs)`, built with `:^`/`NoDelta` and `deltaFromTuple`
    rather than `:|`/`EmptyCoord` and `coordFromTuple`. A displacement is
    unbounded and signed, so it cannot be a position, and `MapDiff '[Clamped
    5, Periodic 3]` is `'[Int, Int]` — a list with no sizes in it that
    `MaxCoordSize` does not even reduce over. `Delta` is indexed by the diff
    list, not the axis list, so it stays the *shared* difference space:
    `Delta '[Int, Int]` is still one type usable at every two-axis grid, and
    a direction table does not have to name a shape. See
    `Data.Grid.Sized.Coord.Delta`.
  - **`(:|)` carries `(IsCoordLifted c, IsCoordList cs)`.** That is what pays
    for the `quotRem` one way and the multiply-add the other. Any caller with
    `IsCoordList (c ': cs)` already has it.
  - **`Coord(..)` no longer exports a constructor.** `unCoord` is still there
    and still hands back an `NP I cs`, rebuilt, but now needs `IsCoordList
    cs`. `unsafeCoordFromPosition` is the unchecked way in for a caller that
    already holds an in-range index.
  - **`IsCoordList`'s methods take positions.** `sizeAndPosition` splits into
    `coordListSize` and `npToPosition`, with `npFromPosition` as the inverse;
    `npOffset` and the rest become `posOffset` and friends over `Int`.
  - Constraints *removed*, so nothing breaks: `coordPosition` and `indexGrid`
    need no `IsCoordList` at all, `Eq`/`Ord`/`NFData` on `Coord` need no
    per-axis constraints, and `permuteGrid` drops `IsCoordList ds`.

  `IsCoord` is untouched. Every method it has was already stated per axis
  through `asOrdinal`, so the fold converts at the edges and no boundary
  policy changed — including `Reflective` and `Reflect101`, which adr.16
  named as the gate and which turn out to compute from a bare index already.

* `unsafeOrdinalUnchecked` is `unsafeOrdinal` without the guard, for callers
  that have established the bound by arithmetic rather than by inspecting the
  value (sized-grid-adr.16). This does not reopen sized-grid-adr.14's
  decision that `unsafeOrdinal`'s own guard is unconditional: every
  *construction* of an axis value still goes through the checked one. It is
  for the other direction — decoding a position, where `p \`quot\` stride <
  size` follows from `p < size * stride`, so the guard re-checks a bound
  already proved. Leaving it in cost 2.08x against 1.78x on the `onBoundary`
  sweep and 1.61x against 1.31x on `coordDistance`, because its cold branch
  is what stops the enclosing fold fusing.

* `mapAxis` and `scanAxis` name an axis by position and act along it,
  independently of the others, on a grid of any dimension (sized-grid-e6h).

  `scanl1Grid` composed with `mapLowerDim` gives per-row prefix sums, but
  reaching the *other* axis needed `transposeGrid` first, and `transposeGrid`
  only swaps a fixed pair of axes — there is no version of it for an
  arbitrary pair, so the trick stopped working past two dimensions. `mapAxis
  1 f`, and `scanAxis 1 (+)` built from it, reach the second axis of a grid
  of any dimension the same way regardless, and the summed-area-table
  build-up in `../aoc/src/2018/11.hs` is now `scanAxis 0 (+) . scanAxis 1
  (+)` rather than a double `transposeGrid`.

  The axis position is a `Nat`, resolved through an internal `MapAxis`
  class. In a row-major vector an axis is two numbers — how many elements a
  fibre along it has, and how far apart they are — and both are products of
  sizes the type already knows, so that is all the class recursion produces.
  `mapAxis` gathers each fibre, applies the function and scatters the result
  back; `scanAxis` does not gather at all, since a prefix scan needs only the
  element one stride behind, so it is a single in-order pass over the grid
  that allocates its result and nothing else (sized-grid-adr.5).

  Measured on 300x300, against the hand-fused `transposeGrid`-based pipeline
  a caller writes when the combinator is not worth reaching for: `scanAxis 0`
  is 2.7x that pipeline boxed and 9.4x it unboxed, and the summed-area build
  it was written for is 1.6x boxed and 2.2x unboxed. `scanAxis 1`, the
  contiguous axis, is `mapLowerDim . scanl1Grid` and costs the same as
  writing that out.

* New: `Apply` and `Bind` instances for `Grid`, from `semigroupoids`
  (sized-grid-o9s).

  A prior survey (sized-grid-90f) judged these "low value" as mere synonyms
  for `Applicative` and `Monad`. That undersold them: `AllSizedKnown` is
  `Applicative`'s cost, not `Apply`'s — it is there only for `pure`, which
  materialises a vector of the right length out of nothing, while `(<.>)` is
  a `zipWith` and needs none of it. `Monad` carries the same constraint only
  because its `(>>=)` went through `Representable`'s `index`, whose instance
  context has it; `indexGrid` itself needs only `IsCoordList`. So `Apply` and
  `Bind` are a real capability `Applicative`/`Monad` cannot offer: code
  polymorphic in a grid's axis list with no `KnownNat` evidence on every axis
  can still combine and rebind grids with `(<.>)` and `(>>-)`.

  `Monad`'s `(>>=)` is now defined as `(>>-)`, so the two cannot drift apart.
  `semigroupoids` was already in the build plan, pulled in transitively by
  `adjunctions` and `lens`, so this costs nothing new to the dependency
  closure — only an explicit direct dependency to import it from.

* New: an `Unzip` instance for `Grid`, from `semialign` (sized-grid-hlp0).

  Splitting a grid of pairs gives two grids of the same shape, so this needs
  no size evidence: both halves inherit the source's length. semialign-1.4
  reordered its class hierarchy to put `Unzip` directly above `Functor`,
  making it a superclass of `Semialign`, so without this instance the library
  no longer compiles against 1.4 at all — and the bound has been
  `>=1.3 && <1.5` throughout. The instance is written to satisfy both
  hierarchies, so 1.3, where `Unzip` sits above `Zip` instead, is unaffected.

* `Grid` gains an unboxed sibling, and the two share one implementation
  (sized-grid-up6).

  The grid type is now `GridOf v cs a`, parameterised over its vector, with
  `type Grid = GridOf V.Vector` and a new `Data.Grid.Sized.Unboxed` supplying
  `type UGrid = GridOf U.Vector`. Existing signatures are unaffected: the
  synonym takes no parameters of its own, so `Grid cs` is still partially
  applicable and `Functor (Grid cs)`, `Representable (Grid cs)` and the rest
  read exactly as before.

  The alternative was a second module with its own monomorphic unboxed API.
  That was rejected because of what it would have had to duplicate — the whole
  shape algebra (`takeGrid`, `sliceGrid`, `splitGrid`, `mapLowerDim`,
  `gridTiles`, `ShrinkableGrid`), including the `windowFits` proof and the
  `off + len <= m` restatement that took all of sized-grid-wrc to get right.
  Under one parameterised type that code exists once and both representations
  get all of it.

  What an unboxed grid gives up is everything that must work at every element
  type: `Functor`, `Foldable`, `Traversable`, `Applicative`, `Monad` and
  `Representable`, and with them `FocusedGrid` and the `Comonad` interface. In
  their place are `tabulateGrid`, `indexGrid`, `mapGrid`, `imapGrid`,
  `zipWithGrid` and `foldlGrid'` — plain functions carrying an element
  constraint, exported from `Data.Grid.Sized` and usable at either
  representation.

  Measured on a 300x300 `Int` grid, unboxed against boxed: 3.3x on
  `tabulateGrid`, 3.5x on map-then-sum and on `foldlGrid'`, 2.4x on
  `transposeGrid`, 2.1x on a summed-area-table build, and no change at all on a
  single indexed read. The last figure is the important one — the win is wholly
  in operations that touch the vector wholesale, and a coordinate-walking read
  loop gains nothing, because its cost is the coordinate arithmetic.

  Every generic function carries an `INLINE` or `INLINABLE` pragma, without
  which nothing specialises through the `Vector v a` dictionary and most of the
  advantage is lost. That also sped the *boxed* path up substantially: against
  the previous release `tabulate` is 60% faster and `fmap` 78% faster, the
  latter because `mapGrid` and a following fold can now fuse. `collapseGrid`
  and `toJSON` are 9% and 19% slower, the cost of one vector conversion at the
  boundary of the axis-list recursion; see the note in
  `Data.Grid.Sized.Internal.Grid` for why that is where it was left.

* The package is renamed from `sized-grid` to `grid-sized`, and the module
  prefix from `SizedGrid.*` to `Data.Grid.Sized.*`.

  This fork has diverged past any possible merge back upstream — GHC 9.10+
  minimum, `RequiredTypeArguments` throughout, a sealed `Grid` constructor,
  `Ordinal` as a newtype over `Int`, GHC2024 — and the `sized-grid` name on
  Hackage belongs to its original author, so the fork could never have been
  released under it. `grid-sized` follows the `vector-sized` convention, which
  is the most legible of the three in use for size-in-the-type packages (the
  others being `fixed-` as in `fixed-vector`, and `Static` as in `hmatrix`).

  The module prefix follows the package name rather than staying at
  `SizedGrid.*`, because a package called one thing whose modules are called
  another leaves the old name at every consumer's import site. The `Data.`
  head is there because it is the near-universal convention for a data
  structure library, and because `vector-sized` — the package this one takes
  its name from — ships `Data.Vector.Sized` rather than a top-level
  `Vector.Sized`.

  Against that: `Data.Grid` belongs to Chris Penner's `grids`, so nesting
  beneath it implies a lineage that does not exist. That was weighed and judged
  not decisive. `grids` was last released in 2019, Haskell module names are
  neither registered nor enforced, `Data.Grid` and `Data.Grid.Sized` do not
  collide in a build plan, and `vector-sized` nests inside `vector`'s
  `Data.Vector.*` namespace in exactly the same way — the difference being that
  it genuinely wraps `vector`, where this package wraps nothing.

* The `Grid` layer collapses into the head module. `SizedGrid.Grid.Grid` becomes
  `Data.Grid.Sized` itself, and `Class`, `Focused` and `Unsafe` move up beside
  it.

  The old tree had no module at `SizedGrid.Grid` at all: the `Grid` type lived
  one level further down, at `SizedGrid.Grid.Grid`, with `Class`, `Focused` and
  `Unsafe` beside it. `Coord` was never arranged that way — `SizedGrid.Coord`
  held the type and `SizedGrid.Coord.{Class,Clamped,Periodic}` sat beneath it,
  which is the ordinary `Data.Vector` / `Data.Vector.Mutable` shape.

  Renaming alone would have left `Data.Grid.Sized.Grid`, which still says grid
  twice — at positions 2 and 4 — because the namespace has already said it.
  Every sibling earns its last component: `.Coord`, `.Ordinal`, `.Focused`,
  `.Class`, `.Unsafe` each add information, and `.Grid` adds none.
  `vector-sized`, the package this one takes its name from, has no
  `Data.Vector.Sized.Vector` for the same reason: the type lives in the head
  module, and the head module _is_ the type's module. Ours was a re-export shim
  instead, so the type had been pushed a level down and had to re-say its own
  name. It no longer is one — `Data.Grid.Sized` now publishes the safe half of
  the hidden `Data.Grid.Sized.Internal.Grid` directly, which is where the type
  was always defined.

  | | type | beside it |
  |---|---|---|
  | before | `SizedGrid.Grid.Grid` | `SizedGrid.Grid.{Class,Focused,Unsafe}` |
  | after | `Data.Grid.Sized` | `Data.Grid.Sized.{Class,Focused,Unsafe}` |

  **Migration:** replace `import SizedGrid` with `import Data.Grid.Sized`, and
  any `SizedGrid.X` with `Data.Grid.Sized.X`; in your `.cabal`, `sized-grid`
  becomes `grid-sized`. The imports that are not a plain prefix swap are the
  four in the `Grid` layer: `SizedGrid.Grid.Grid` becomes `Data.Grid.Sized`,
  and `SizedGrid.Grid.{Class,Focused,Unsafe}` become
  `Data.Grid.Sized.{Class,Focused,Unsafe}`. Since `Data.Grid.Sized` re-exports
  all of those but `Unsafe`, importing it alone is usually enough. Nothing else
  changes: no type, class, function or instance is affected by any of this.

## 0.3.0.0 -- NOT PUBLISHED

Correctness release. Every change below is breaking, and each one turns a
silently-wrong result — or a name that invited one — into either a rejected
value or a type error.

* A displacement is an `Int`, not an `Integer`. `Diff (Clamped n)`,
  `Diff (Periodic n)` and the argument of `offsetIsCoord` all change, and so
  does everything built on them: `Diff (Coord cs)` is now `Coord '[Int, ...]`,
  and `offsetCoord`, `offsetCoordUpTo` and `coordRay` ask for
  `AllDiffSame Int cs`.

  **Migration:** replace `Integer` with `Int` where a displacement is named.
  `AllDiffSame Integer cs` becomes `AllDiffSame Int cs`, `Diff x ~ Integer`
  becomes `Diff x ~ Int`, and a helper with a written-out type such as
  `Integer -> Integer -> Coord '[Integer, Integer]` becomes the `Int` version.
  Call sites that pass numeric literals need no change at all, because the
  literals were already polymorphic — but `coordFromTuple` is not by itself a
  guarantee of that: `coordFromTuple (0, toInteger col)` names the old type in
  its second component and stops compiling, while `coordFromTuple (0, col)`
  with `col :: Int` is what it wanted to say. A `fromIntegral` applied to the
  result of `(.-.)` is now the identity and will warn under `-Widentities`.

  Measured against the one downstream package rather than reasoned about: of
  the eight `aoc` executables that import `SizedGrid`, six built unchanged and
  two needed one line each — the `toInteger` above, and a helper declared
  `Coord '[Integer, Integer]`.

  The `Integer` was there for overflow safety, and the safety is kept without
  it. `Clamped`'s `(.+^)` decides by comparing the displacement against
  `hi - i` and `negate i` — both computed from the coordinate and the size, so
  neither can overflow — instead of adding first and clamping the sum, which
  wraps a large positive offset into a negative one and clamps it to the *low*
  edge. `Periodic` reduces the displacement modulo the size before adding it,
  for the same reason. `Test.Ordinal` pins both at `maxBound`, from the top of
  the axis, which is where the two implementations actually disagree.

  This is a performance change and it is worth being exact about what it buys,
  because the issue that prompted it (`sized-grid-0tj`) predicted more. Per axis
  it is decisive: 360,000 offsets on a bare `Clamped 300` went from 8.41 ms and
  22 MB to 2.20 ms and 94 KB, so the arithmetic no longer allocates. Through a
  two-axis `Coord` the same 360,000 offsets went from 32.4 ms and 143 MB to
  28.6 ms and 126 MB — 11%, not the order of magnitude expected.

  So 126 of those 143 MB were never the `Integer`. They are the fold over the
  axis list in the `AffineSpace (Coord cs)` instance, which is a self-recursive
  polymorphic helper and so cannot unroll: the same problem `coordPosition` had
  before its fold became an `IsCoordList` method. That is the next entry.
  `bench/Main.hs` now carries both benchmarks, because only the gap between them
  says which half is paying.

* The `AffineSpace (Coord cs)` instance asks for `AffineCoordList cs` where it
  used to ask for `All AffineSpace cs`. This is the fold the entry above
  measured and left alone.

  **Migration:** most code changes nothing. `All AffineSpace cs` is a
  superclass of `AffineCoordList cs`, so every *use* of `(.+^)` and `(.-.)` on
  a `Coord` still typechecks, and code at a known axis list — including a list
  whose *length* is known but whose element types are variables, such as
  `cs ~ '[x, y]` — discharges the new class by instance resolution without
  naming it. Only a signature that writes `All AffineSpace cs` at a fully
  polymorphic `cs` *in order to* use the instance has to write
  `AffineCoordList cs` instead. In this repository no such signature existed:
  the `All AffineSpace cs` in `gameOfLife` and `README.lhs` turned out to be
  redundant constraints, which GHC already says so under
  `-Wredundant-constraints`.

  The fold is now a method of a class indexed by the axis list, so the
  per-axis dictionary is resolved during instance resolution and the recursion
  unrolls at a concrete list, exactly as `IsCoordList` does for
  `coordPosition`. It is a separate class from `IsCoordList` because the
  per-axis step needs `AffineSpace x`, which `IsCoordLifted` does not supply —
  an `Ordinal` is indexable and has no `AffineSpace` instance at all.

  Measured on the two benchmarks the previous entry left behind, 360,000
  offsets each:

  | | before | after |
  |---|---|---|
  | four corner reads through a `Coord` | 28.5 ms / 126 MB | 2.02 ms / 53 B |
  | one bare `Clamped 300` axis (control) | 2.27 ms / 94 KB | 2.30 ms / 94 KB |

  The control does not move, which is what says this is the fold rather than
  the arithmetic. 53 bytes is the whole benchmark and not per call: offsetting
  a `Coord` no longer allocates. `(.-.)` over 10,000 coordinates went from
  1.4 MB to 17 bytes with it.

* `AllGridSizeKnown` is a class rather than a type family, and `gridFromList`,
  `collapseGrid` and the `Grid` `ToJSON`/`FromJSON` instances no longer ask for
  `SListI cs`.

  **Migration:** delete constraints. A signature that read

  ```haskell
  parse :: (KnownNat n, KnownNat (n * n)) => String -> Maybe (Grid '[Clamped n, Clamped n] Cell)
  ```

  now reads `parse :: KnownNat n => ...`, and any `SListI cs` that was there
  only to satisfy these four is redundant. Nothing gains an obligation. Code
  that mentioned `AllGridSizeKnown` by name still compiles; it is the same name
  with the same meaning.

  A type family cannot solve anything — it expands, and whatever it expands to
  is the caller's problem. `AllGridSizeKnown` expanded to a conjunction
  containing `KnownNat (MaxCoordSize cs)`, which at `'[Clamped n, Clamped n]` is
  `KnownNat (n * (n * 1))`; GHC cannot get that from `KnownNat n`, so every
  caller wrote it out. In the one downstream package five signatures did. As a
  class the same fact is derived during instance resolution, inductively, from
  the per-axis `KnownNat`s — which is what `AllSizedKnown` has always done, and
  this is that treatment applied to the structural recursions.

  The tail's dictionary has to be carried in the value, as a new
  `GridSizeProof` GADT returned by the class's single method: a class dictionary
  cannot be run backwards through its own instance context, so there is no other
  way to recover `AllGridSizeKnown xs` from `AllGridSizeKnown (x ': xs)`.
  Matching on that GADT also refines the axis list to nil or cons, which is what
  `SListI cs` and `Generics.SOP.Shape` were doing before — hence their removal.

  Enabling `ghc-typelits-knownnat` here does not substitute for this, which is
  what the retired `sized-grid-h56` plan assumed: `-fplugin` is not transitive,
  so a consumer solves its own goals with no solver but GHC's. The new
  `downstream` test suite is compiled without the plugins for exactly this
  reason and would not typecheck against the old family.

* `All IsCoordLifted cs` is replaced by the class `IsCoordList cs` throughout
  the API. It has `All IsCoordLifted cs` as a superclass, so it is a rename
  rather than an added obligation, and at a concrete axis list it is discharged
  by instance resolution exactly as the old constraint was.

  **Migration:** in a signature polymorphic in the axes, replace
  `All IsCoordLifted cs` with `IsCoordList cs`. Code working at a concrete grid
  shape needs no change at all; the constraint never appeared there.

  The reason is performance, and it is structural rather than incidental.
  `coordPosition` folds over the axis list, and that fold used to be a
  self-recursive `where` helper. GHC never inlines a self-recursive binding, and
  the recursion is polymorphic — each call is at a shorter list — so specialising
  at a concrete list rewrote the outermost call and left the tail going through
  the same generic worker, holding the dictionary at run time. Every axis but the
  first paid for dictionary peeling, an `Integer` from `natVal`, a trip through
  the `asOrdinal` `Iso` and two boxed `Int`s. Neither `INLINE`, `INLINABLE` nor
  `-fpolymorphic-specialisation` rescues that shape; all were measured.

  `cpara_SList` is not subject to the same limit — it is not self-recursive in
  the Core and it does unroll — but it unrolls only where the axis list is
  concrete, which means `INLINE` the whole way down. `coordPosition` is called
  from polymorphic instance methods (`index`, `gridIndex`), so that splices the
  eliminator somewhere nothing can resolve and builds the closure chain per call:
  584 bytes a call and 50 MB on `index x90000`, worse than the 320 bytes and
  27 MB it started at. An instance method resolves whenever the dictionary is
  known, at specialisation rather than only at inlining, which is why that is the
  shape that survives the library's own call path.

  As a method the fold unrolls and the sizes become literals — a
  two-dimensional `coordPosition` compiles to `x * 50 + y`. Measured on the
  benchmark suite: `index` over 90,000 coordinates went from 27 MB to 38 bytes
  and 8.05 ms to 753 µs, `ifoldl'` from 30 MB to 2.7 MB, `imap` from 35 MB to
  7.6 MB, `tabulate` from 47 MB to 20 MB, and `extract` from 320 bytes to zero.
  Nothing regressed.

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
  `IsCoordList cs`. They also work on `Ordinal` axes, which the old ones
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

  There is deliberately no interior-restricted `Grid` traversal to go with
  `interiorCoords`. `Grid cs` is already `TraversableWithIndex (Coord cs)`, so
  `itraversed . indices (not . onBoundary)` is one, it reads and writes where an
  enumeration only reads, and swapping the predicate gives the border. Naming
  that composition would add a signature and a second way to say it and no
  capability. It is documented on `interiorCoords` and exercised by the test
  suite instead.

* New: an offset that reports where it hit the edge. `offsetCoordUpTo n c d`
  takes up to `n` steps of `d` and answers
  `Either (OffGrid cs) (Coord cs)` — `Right` is the coordinate `n` whole steps
  away, `Left` carries `lastInside` (the final coordinate still on the grid)
  and `stepsTaken` (how many succeeded). `coordRay c d` is the same walk with
  no bound: `c .+^ d`, `c .+^ 2d`, … for as long as the grid lasts.

  This is purely additive. `offsetCoord` is unchanged and is still the right
  answer when the caller does not care where the edge was; it is now also the
  law `offsetCoord c d == either (const Nothing) Just (offsetCoordUpTo 1 c d)`.

  Following `manifolds`' `(.+^|)`, which answers a displacement with
  `Either (Boundary m, Scalar (Needle m)) (Interior m)`: either you arrived in
  the interior, or here is the boundary you met and how far along you got.
  `offsetCoord` throws all of that away — `Nothing` says an offset left the
  grid but not where, not on which axis, and not how many steps succeeded
  first — so any consumer that needed it rediscovered it by hand. The observed
  workaround was worse than a loop: an `aoc` day wanting "three steps that way,
  or nothing" embedded the coordinate into a grid one cell larger, offset three
  times there, weakened back, and drove the type-level arithmetic with three
  `Dict` entailments. `take 3 (coordRay c d)` is that, and a ray is the shape
  every raycasting, beam-tracing or line-of-sight problem has.

  A step is a whole `d`, not a subdivision of one, which is why
  `offsetCoordUpTo` counts steps rather than taking a fraction of a
  displacement the way its continuous model does. On a lattice there is nothing
  between `c` and `c .+^ d` to stop at unless `d` is itself a multiple of a
  shorter displacement, so a caller who wants the finer walk passes the finer
  `d`. That is also why `lastInside` is on the boundary when `d` is one cell
  wide and need not be otherwise, and where it is, `axisBoundaries . lastInside`
  names the edge the walk met.

  A `Periodic` axis never refuses a step, so on an all-`Periodic` coord every
  walk is `Right` and every ray is infinite — a torus has no boundary to
  report, which is the same fact `axisBoundary` states for a single value, and
  the list is lazy, so take what you need. Mixed coords are the interesting
  case: the bounded axis decides when the walk ends while the torus axis keeps
  wrapping.

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
  silently truncate. Its constraints are `AllGridSizeKnown cs`, matching
  `ToJSON`.

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
  `coordPosition`. `coordSpaceSize` asks only for `IsCoordList cs` rather
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

* New: `Eq` and `Show` for `FocusedGrid`, which `Grid` has had all along.
  Equality is on both the grid and the focus, so two grids holding the same
  cells at different focuses are distinct — which is the notion the `Comonad`
  laws want, since `duplicate` is required to preserve the focus.

  These were added because without them the `Comonad` instance was the one part
  of the library whose laws could not be stated as a test. They now are, and
  `fmap extract . duplicate == id` catches a `duplicate` that rebuilds each cell
  from the wrong focus — which nothing else in the suite did.

* Builds with GHC 9.10 through 9.14 via a nix flake; `stack.yaml` and Travis
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
