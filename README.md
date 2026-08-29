grid-sized [![CI](https://github.com/beefyhalo/sized-grid/actions/workflows/ci.yml/badge.svg)](https://github.com/beefyhalo/sized-grid/actions/workflows/ci.yml)
===========

A way of working with grids in Haskell with size encoded at the type level.

The design thesis
=================

**The coordinate type is the boundary policy.**

Every axis of a grid is indexed by a coordinate type that says what happens at
the edge. `Periodic n` wraps, `Clamped n` stops, `Ordinal n` cannot leave at
all. That single decision is what the rest of the API is derived from:

> Any operation that can leave the space is either total, because the type says
> how to come back, or it returns `Maybe`. **Nothing clamps silently.**

The negative half of that sentence is the load-bearing one. A grid library that
silently clamps an out-of-range index is not merely imprecise, it is a library
in which the difference between "the cell you asked for" and "some other cell"
has been erased at the point where you could still have handled it. So
`offsetCoord` reports leaving the grid rather than sliding back onto it,
`offsetCoordUpTo` says where a walk stopped, `numToOrdinal` returns `Maybe`, and
a `Clamped` axis clamps only because you named it `Clamped` — never as a
fallback for an operation that had no better answer.

Read that as the rule for judging any proposed addition. If a new function has
to clamp, wrap, or truncate to stay total, it either takes the coordinate type
that licenses it or it returns `Maybe`. There is no third option.

**A restriction destroys the boundary policy. A pointing preserves it.**

That is the corollary, and it decides the type of every sub-grid. Taking a
window, a tile or a sub-matrix out of a grid *restricts* it: the result has a
smaller extent and no position in the source. Adding a focus — `FocusedGrid`,
`Walker` — *points* at it: the extent is unchanged and a distinguished position
is added. The two do opposite things to the policy.

Periodicity is a property of a whole axis, so a proper sub-window of a periodic
axis is not periodic; and "clamped" means stepping off the edge stays at the
edge, so clamping at a window's edge is a claim about a wall the source does not
have there. Either way the sub-grid invents a seam that is not in the space it
is a view of, and does it silently. So `shrinkGrid`, `gridWindows` and
`gridTiles` return grids whose narrowed axis is `Ordinal` — no walls, no wrap,
and an off-grid step that returns `Nothing` rather than an invented answer —
whatever the source's policy was. Axes they leave at full width keep theirs,
because those have not been restricted.

Offsets are `Ordinal` for the same reason read from the other side. An offset is
an index into a list of positions, not a position in a space, so `shrinkGrid`'s
offset into a `Periodic 9` windowed to 3 is a `Coord '[Ordinal 7]`; as a
`Coord '[Periodic 7]` its `<>` wrapped offset 6 plus offset 3 round to offset 2,
which is arithmetic in no space the caller has.

`FocusedGrid` is the other side of the rule: it keeps the whole grid, so it
keeps the policy, which is exactly what makes its `Comonad` worth having —
`extend f` runs `f` at every cell of the *source*. A caller who genuinely wants
a policy back on a sub-grid restates it, which is a place where they have to
think, which is the point.

Provenance and name
===================

This package began as a fork of [edwardwas'
`sized-grid`](https://github.com/edwardwas/sized-grid) and has diverged past any
possible merge back: GHC 9.10+ minimum, `RequiredTypeArguments` throughout, a
sealed `Grid` constructor, `Ordinal` as a newtype over `Int`, and GHC2024. The
`sized-grid` name on Hackage is his, so this is released as `grid-sized`,
following the `vector-sized` convention, with the version restarted at 0.1.0.0.

Quick tutorial
========

The core datatype of this library is `Grid (cs :: '[k]) (a :: *)`. `cs` is a type level list of coordinate types. We could use a single type level number here, but by using different types we can say what happened when we move outside the bounds of a grid. There are three different coordinate types provided.

* `Ordinal n`: An ordinal can be an integral number between 0 and n - 1. As numbers outside the grid are not possible, this has the most restrictive API. One can convert between an Ordinal and a number of ordinalToNum and numToOrdinal.

* `Clamped n`: Like `Ordinal`, `Clamped` can only hold integral numbers between 0 and n - 1, but it allows a more permissive API by clamping values outside of its range to the nearest end. It is an instance of `Semigroup` and `Monoid`, where `mempty` is 0 and `<>` is saturating addition.

* `Periodic n`: This is the most permissive. When a value is generated outside the given range, it wraps that around using modular arithmetic. Is is an instance of `Semigroup` and `Monoid` like `Clamped`, but also of `AdditiveGroup` allowing negation.

`Clamped` and `Periodic` are both instances of `AffineSpace`, with their `Diff` being `Int`. This means there are many occasions where one doesn't have to work directly with these values (which can be cumbersome) and can instead work with their differences as regular numbers.

The last type value of `Grid` is the type of each element. 

The other main type is `Coord cs`, where `cs` is, again, a type level list of coordinate types. For example, `Coord '[Periodic 3, Clamped 4]` is a coordinate in a 3 by 4 2D space. The different types (`Periodic` and `Clamped`) tell how to handle combining theses different numbers. `Coord cs` is an instance of `Semigroup`, `Monoid` and `AdditiveGroup` as long as each of the coordinates is also an instance of that typeclass. `Coord` is also an instance of `AffineSpace`, where `Diff (Coord cs)` is `Delta (MapDiff cs)` — a displacement, holding one `Diff` per axis. It is written with `:^` and `NoDelta`, the way a position is written with `:|` and `EmptyCoord`; `deltaFromTuple` and `deltaToTuple` convert to and from a tuple of the same arity where that reads better.

For working directly with `Coord`s, one can construct them with `singleCoord` and `appendCoord` and consume and update them with `coordHead` and `coordTail`. They are also instances of `FieldN` from lens, allowing one to directly update or get a certain dimension.

There is a deliberately small number of functions that work over `Grid`: we instead opt for using typeclasses to create the required functionality. `Grid` is an instance of the following types (with some required constraints):

* `Functor`: Update all values in the grid with the same function
* `Applicative`: As the size of the grid is statically known, `pure` just creates a grid with the same element at each point. `<*>` combines the grids point wise.
* `Monad`: `>>=` rebinds each cell against the value at that same position in the result of applying the function.
* `Apply` and `Bind`, from `semigroupoids`: `<.>` and `>>-` do the same point-wise combination and per-cell rebind as `<*>` and `>>=`, but need no axis size known at all — only that `cs` is a coordinate list. Useful when a caller is polymorphic in `cs` with no `KnownNat` evidence to hand; `pure` is what actually needs every axis's size, not applying or binding.
* `Foldable`: Combine each element of the grid
* `Traverse`: Apply an applicative function over the grid
* `IndexedFunctor`, `IndexedFoldable` and `IndexedTraversable`: Like `Functor`, `Foldable` and `Traversable`, but with access to the position at each point. These are from the lens package
* `Distributive`: Like `Traversable`, but the other way round. Allows us to put a functor inside the grid
* `Representable`: `Grid cs a` is isomorphic `Coord cs -> a`, so we can `tabulate` and `index` to make this conversion

We also have a `FocusedGrid` type, which is like `Grid` but has a certain focused position. This means that we lose many instances, but we gain `Comonad` and `ComonadStore`. 

`Grid` is a synonym: underneath it is `GridOf v cs a`, which takes its vector type as a parameter, and `Data.Grid.Sized.Unboxed` supplies `UGrid = GridOf U.Vector` for numeric grids. Every function above that does not need an unconstrained element type — the whole shape algebra, `takeGrid` through `shrinkGrid` — works at either representation, so the unboxed grid is not a second implementation with its own bugs. What it gives up is exactly the typeclasses listed above, since each of them promises to work at *every* element type; in their place are `tabulateGrid`, `indexGrid`, `mapGrid`, `imapGrid`, `zipWithGrid` and `foldlGrid'`, which say the same things as ordinary functions carrying an `Unbox` constraint. Unboxing is worth 2–3.5x on operations that touch the whole vector and nothing at all on indexed reads; see the module documentation before reaching for it.

When dealing with areas around `Coord`s, `neighbours` gives the surrounding cells: the [Moore](https://en.wikipedia.org/wiki/Moore_neighborhood) neighbourhood at radius one, excluding the centre. `mooreNeighbours` and `vonNeumannNeighbours` take a radius, the latter generating a [von Neumann](https://en.wikipedia.org/wiki/Von_Neumann_neighborhood) neighbourhood instead.

Each axis applies its own boundary policy, so a `Clamped` axis simply has fewer neighbours at its edges while a `Periodic` axis always has the full complement. Nothing is ever duplicated and the centre is never included, so there is no result to repair. For a single step in a chosen direction, `offsetCoord` is `(.+^)` that reports leaving the grid instead of clamping back onto it.

The same policy answers where the edges are. `onBoundary` and `isCorner` say whether a `Coord` is on one, `axisBoundary` says which end of a single axis it sits at — `AtMin`, `AtMax` or neither — and `interiorCoords` lists the cells that are on no edge at all, which is exactly the cells whose full Moore neighbourhood exists. A `Periodic` axis has no ends, so a torus reports no boundary and no corners rather than the four a comparison against `natVal` would find.

Because `Grid cs` is `TraversableWithIndex (Coord cs)`, these predicates also give you the interior of a grid as an optic, with no extra API: `itraversed . indices (not . onBoundary)` reads and writes exactly the interior cells, and swapping the predicate gives the border.

To walk rather than step, `coordRay c d` is the ray from `c` in direction `d` — `c .+^ d`, `c .+^ 2d`, and so on for as long as the grid lasts — so `take 3 (coordRay c d)` is three steps that way, and shorter than three exactly when there was not room. When you need to know where a walk stopped rather than only that it did, `offsetCoordUpTo n c d` answers `Left` with the last coordinate still on the grid and the number of steps that succeeded. On an all-`Periodic` coord nothing can stop the walk, so the ray is infinite and lazy.

We introduce two new typeclasses: `IsCoord` and `IsGrid`. `IsGrid` has `gridIndex`, which allows us to get a single element of the grid and lenses to convert between `FocusedGrid` and `Grid`. `IsCoord` has `CoordSized`, which is the size of the coord and an iso to convert between `Ordinal` and the `Coord`.

A third, `IsCoordList cs`, is the one you will actually see in signatures — it says that `cs` is a list of axes a `Coord` can be built from. At a concrete list it is discharged by instance resolution, so working at a known grid shape you never write it; it appears only when you are polymorphic in the axes, as `applyRule` below is. It supersedes the `All IsCoordLifted cs` that used to sit in those signatures and implies it, so it is a rename rather than an extra obligation. It also carries the row-major fold behind `coordPosition` as a method, which is what lets that fold unroll to plain arithmetic instead of walking a dictionary per axis at run time.

Example - Game of Life
=====================

As is traditional for anything with grids and comonads in Haskell, we can reimplement [Conway's Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life).

This is a literate Haskell file, so we start by turning on some language extensions, importing our library and some other utilities.

```haskell
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}

import Data.Grid.Sized

import Control.Comonad
import Control.Lens
import Control.Comonad.Store
import Data.AffineSpace
import GHC.TypeLits
```

We create a datatype for alive or dead.

```haskell
data TileState = Alive | Dead deriving (Eq,Show)
```

We encode the rules of the game via a step function.

```haskell
type Rule = TileState -> [TileState] -> TileState

gameOfLife :: Rule
gameOfLife here neigh =
    let aliveNeigh = length $ filter (== Alive) neigh
    in if | here == Alive && aliveNeigh `elem` [2,3] -> Alive
          | here == Dead && aliveNeigh == 3 -> Alive
          | otherwise -> Dead
```

We can then write a function to apply this to every point in a grid.

```haskell
applyRule :: 
       ( IsCoordList cs
       , All Monoid cs
       , AllSizedKnown cs
       , IsGrid cs (grid cs)
       )
    => Rule
    -> grid cs TileState
    -> grid cs TileState
applyRule rule = over asGrid $ focusedGrid . extend step . focusedAtZero where
    step fg = rule (extract fg) $ map (\p -> peek p fg) $ neighbours $ pos fg

```

We can create a simple drawing function to display it to the screen.

```haskell
displayTileState :: TileState -> Char
displayTileState Alive = '#'
displayTileState Dead = '.'

displayGrid :: (KnownNat x, KnownNat y) => 
      Grid '[f x, g y] TileState -> String
displayGrid = unlines . collapseGrid . fmap displayTileState
```

Let's create a glider, and watch it move!

```haskell
glider :: 
      ( IsCoordLifted x
      , IsCoordLifted y
      , AffineSpace x
      , AffineSpace y
      , Diff x ~ Int
      , Diff y ~ Int
      ) 
      => Coord '[x,y] 
      -> Grid '[x,y] TileState
glider offset = pure Dead 
    & gridIndex (offset .+^ deltaFromTuple (0,-1)) .~ Alive
    & gridIndex (offset .+^ deltaFromTuple (1,0)) .~ Alive
    & gridIndex (offset .+^ deltaFromTuple (-1,1)) .~ Alive
    & gridIndex (offset .+^ deltaFromTuple (0,1)) .~ Alive
    & gridIndex (offset .+^ deltaFromTuple (1,1)) .~ Alive
```

We can now make our glider run! A generation is just one `applyRule`, so the whole
simulation is an `iterate`.

```haskell
start :: Grid '[Periodic 10, Periodic 10] TileState
start = glider (mempty .+^ deltaFromTuple (3,3))

generations :: Grid '[Periodic 10, Periodic 10] TileState 
      -> [Grid '[Periodic 10, Periodic 10] TileState]
generations = iterate (applyRule gameOfLife)

main :: IO ()
main = mapM_ (putStrLn . displayGrid) $ take 4 $ generations start
```
