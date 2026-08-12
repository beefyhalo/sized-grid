[![Build Status](https://travis-ci.org/edwardwas/sized-grid.svg?branch=master)](https://travis-ci.org/edwardwas/sized-grid)

![Hackage](https://img.shields.io/hackage/v/sized-grid)

sized-grid
===========

A way of working with grids in Haskell with size encoded at the type level.

Quick tutorial
========

The core datatype of this library is `Grid (cs :: '[k]) (a :: *)`. `cs` is a type level list of coordinate types. We could use a single type level number here, but by using different types we can say what happened when we move outside the bounds of a grid. There are three different coordinate types provided.

* `Ordinal n`: An ordinal can be an integral number between 0 and n - 1. As numbers outside the grid are not possible, this has the most restrictive API. One can convert between an Ordinal and a number of ordinalToNum and numToOrdinal.

* `Clamped n`: Like `Ordinal`, `Clamped` can only hold integral numbers between 0 and n - 1, but it allows a more permissive API by clamping values outside of its range to the nearest end. It is an instance of `Semigroup` and `Monoid`, where `mempty` is 0 and `<>` is saturating addition.

* `Periodic n`: This is the most permissive. When a value is generated outside the given range, it wraps that around using modular arithmetic. Is is an instance of `Semigroup` and `Monoid` like `Clamped`, but also of `AdditiveGroup` allowing negation.

`Clamped` and `Periodic` are both instances of `AffineSpace`, with their `Diff` being `Integer`. This means there are many occasions where one doesn't have to work directly with these values (which can be cumbersome) and can instead work with their differences as regular numbers.

The last type value of `Grid` is the type of each element. 

The other main type is `Coord cs`, where `cs` is, again, a type level list of coordinate types. For example, `Coord '[Periodic 3, Clamped 4]` is a coordinate in a 3 by 4 2D space. The different types (`Periodic` and `Clamped`) tell how to handle combining theses different numbers. `Coord cs` is an instance of `Semigroup`, `Monoid` and `AdditiveGroup` as long as each of the coordinates is also an instance of that typeclass. `Coord` is also an instance of of `AffineSpace`, where `Diff (Coord cs)` is `Coord (MapDiff cs)` — a coordinate again, holding one `Diff` per axis. So a displacement is written the same way a position is, with `:|`, and inherits every `Coord` instance; `coordFromTuple` and `coordToTuple` convert to and from a tuple of the same arity where that reads better.

For working directly with `Coord`s, one can construct them with `singleCoord` and `appendCoord` and consume and update them with `coordHead` and `coordTail`. They are also instances of `FieldN` from lens, allowing one to directly update or get a certain dimension.

There is a deliberately small number of functions that work over `Grid`: we instead opt for using typeclasses to create the required functionality. `Grid` is an instance of the following types (with some required constraints):

* `Functor`: Update all values in the grid with the same function
* `Applicative`: As the size of the grid is statically known, `pure` just creates a grid with the same element at each point. `<*>` combines the grids point wise.
* `Monad`: I'm not sure if there is much of a need for this, but an instance exists.  
* `Foldable`: Combine each element of the grid
* `Traverse`: Apply an applicative function over the grid
* `IndexedFunctor`, `IndexedFoldable` and `IndexedTraversable`: Like `Functor`, `Foldable` and `Traversable`, but with access to the position at each point. These are from the lens package
* `Distributive`: Like `Traversable`, but the other way round. Allows us to put a functor inside the grid
* `Representable`: `Grid cs a` is isomorphic `Coord cs -> a`, so we can `tabulate` and `index` to make this conversion

We also have a `FocusedGrid` type, which is like `Grid` but has a certain focused position. This means that we lose many instances, but we gain `Comonad` and `ComonadStore`. 

When dealing with areas around `Coord`s, `neighbours` gives the surrounding cells: the [Moore](https://en.wikipedia.org/wiki/Moore_neighborhood) neighbourhood at radius one, excluding the centre. `mooreNeighbours` and `vonNeumannNeighbours` take a radius, the latter generating a [von Neumann](https://en.wikipedia.org/wiki/Von_Neumann_neighborhood) neighbourhood instead.

Each axis applies its own boundary policy, so a `Clamped` axis simply has fewer neighbours at its edges while a `Periodic` axis always has the full complement. Nothing is ever duplicated and the centre is never included, so there is no result to repair. For a single step in a chosen direction, `offsetCoord` is `(.+^)` that reports leaving the grid instead of clamping back onto it.

The same policy answers where the edges are. `onBoundary` and `isCorner` say whether a `Coord` is on one, `axisBoundary` says which end of a single axis it sits at — `AtMin`, `AtMax` or neither — and `interiorCoords` lists the cells that are on no edge at all, which is exactly the cells whose full Moore neighbourhood exists. A `Periodic` axis has no ends, so a torus reports no boundary and no corners rather than the four a comparison against `natVal` would find.

Because `Grid cs` is `TraversableWithIndex (Coord cs)`, these predicates also give you the interior of a grid as an optic, with no extra API: `itraversed . indices (not . onBoundary)` reads and writes exactly the interior cells, and swapping the predicate gives the border.

We introduce two new typeclasses: `IsCoord` and `IsGrid`. `IsGrid` has `gridIndex`, which allows us to get a single element of the grid and lenses to convert between `FocusedGrid` and `Grid`. `IsCoord` has `CoordSized`, which is the size of the coord and an iso to convert between `Ordinal` and the `Coord`.

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

import SizedGrid

import Control.Comonad
import Control.Lens
import Control.Comonad.Store
import Data.AffineSpace
import GHC.TypeLits
import qualified GHC.TypeLits as GHC
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
       ( All IsCoordLifted cs
       , All Monoid cs
       , All Semigroup cs
       , All AffineSpace cs
       , All Eq cs
       , AllDiffSame Integer cs
       , AllSizedKnown cs
       , IsGrid cs (grid cs)
       )
    => Rule
    -> grid cs TileState
    -> grid cs TileState
applyRule rule = over asFocusedGrid $ 
    extend $ \fg -> rule (extract fg) $ map (\p -> peek p fg) $ 
        neighbours $ pos fg

```

We can create a simple drawing function to display it to the screen.

```haskell
displayTileState :: TileState -> Char
displayTileState Alive = '#'
displayTileState Dead = '.'

displayGrid :: (KnownNat (x GHC.* y), KnownNat x, KnownNat y) => 
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
      , Diff x ~ Integer
      , Diff y ~ Integer
      ) 
      => Coord '[x,y] 
      -> Grid '[x,y] TileState
glider offset = pure Dead 
    & gridIndex (offset .+^ coordFromTuple (0,-1)) .~ Alive
    & gridIndex (offset .+^ coordFromTuple (1,0)) .~ Alive
    & gridIndex (offset .+^ coordFromTuple (-1,1)) .~ Alive
    & gridIndex (offset .+^ coordFromTuple (0,1)) .~ Alive
    & gridIndex (offset .+^ coordFromTuple (1,1)) .~ Alive
```

We can now make our glider run! A generation is just one `applyRule`, so the whole
simulation is an `iterate`.

```haskell
start :: Grid '[Periodic 10, Periodic 10] TileState
start = glider (mempty .+^ coordFromTuple (3,3))

generations :: Grid '[Periodic 10, Periodic 10] TileState 
      -> [Grid '[Periodic 10, Periodic 10] TileState]
generations = iterate (applyRule gameOfLife)

main :: IO ()
main = mapM_ (putStrLn . displayGrid) $ take 4 $ generations start
```
