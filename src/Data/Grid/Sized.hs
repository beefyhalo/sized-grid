{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      :  Data.Grid.Sized
-- License     :  MIT -style (see the file LICENSE)
--
-- The `Grid` type, the safe operations over it, and a re-export of the rest of
-- the public API. Import this and you are good to go.
--
-- `Grid` is exported abstractly: you get the type, not the constructor. A
-- @Grid cs a@ is a vector of exactly @MaxCoordSize cs@ elements, and that
-- equality is the invariant the rest of the library relies on -- `index` reads
-- a position it has computed from the type, and ('<*>') zips two vectors it
-- assumes are the same length. A public constructor makes both of those
-- assumptions the caller's problem, silently.
--
-- To get a grid: `gridFromVector` or `gridFromList` (both checked), `tabulate`,
-- `pure`, or any of the shape-changing functions here. To read one: `gridVector`,
-- `index`, or the `Foldable` instance. If you genuinely need to assert a length
-- the compiler cannot see -- the usual case is applying a length-preserving
-- vector function the library does not wrap -- "Data.Grid.Sized.Unsafe" has the
-- escape hatch, and it is the one public module this one does not re-export.
--
-- == Boxed and unboxed
--
-- @Grid@ is the boxed grid, and it is a synonym: the type underneath is
-- `GridOf`, which takes the vector as a parameter. Everything here that does not
-- need an unconstrained element type -- which is all of the shape algebra --
-- works at any vector, so "Data.Grid.Sized.Unboxed" gets it for free rather
-- than by duplication. See that module for when the unboxed representation is
-- worth reaching for, and 'Data.Grid.Sized.Internal.Grid' for why it is a
-- parameter rather than a second implementation.
--
-- The functions grouped under \"Bulk operations\" are the ones an unboxed grid
-- needs, because their unconstrained counterparts (`fmap`,
-- `Data.Functor.Rep.tabulate`, and the rest) are class methods and a class
-- method may not constrain its element type. On a boxed grid they are just
-- those methods under another name.
module Data.Grid.Sized
    ( -- * The grid type
      Grid
    , GridOf
      -- * Construction
    , gridFromVector
    , gridFromList
      -- * Access
    , gridVector
    , collapseGrid
      -- * Bulk operations
    , tabulateGrid
    , indexGrid
    , mapGrid
    , imapGrid
    , zipWithGrid
    , foldlGrid'
      -- * Type-level machinery
    , CollapseGrid
      -- * Rearranging
    , transposeGrid
    , splitGrid
    , combineGrid
    , combineHigherDim
    , splitHigherDim
    , dropGrid
    , takeGrid
    , mapLowerDim
    , zipLowerDim
    , scanl1Grid
    , MapAxis(..)
    , mapAxis
    , scanAxis
      -- * Windows and tiles
    , ShrinkableGrid(..)
    , gridTiles
    , gridWindows
      -- * Vector helpers
    , splitVectorBySize
      -- * The rest of the public API
    , module X
      -- * Rexported for generics-sop
    , All
    , SListI
    , Compose
    , I(..)
    ) where

import           Data.Grid.Sized.Class            as X
import           Data.Grid.Sized.Coord            as X
import           Data.Grid.Sized.Coord.Clamped    as X
import           Data.Grid.Sized.Coord.Class      as X
import           Data.Grid.Sized.Coord.Periodic   as X
import           Data.Grid.Sized.Coord.Reflect101 as X
import           Data.Grid.Sized.Coord.Reflective as X
import           Data.Grid.Sized.Focused          as X
import           Data.Grid.Sized.Ordinal          as X
import           Data.Grid.Sized.Stencil          as X

-- The `Grid` type is defined in the hidden "Data.Grid.Sized.Internal.Grid", and
-- this module publishes the safe half of it -- hence the explicit export list
-- above rather than a `module X` re-export, which would carry the constructor
-- and `sliceGrid` out with everything else.
import           Data.Grid.Sized.Internal.Grid

import           Generics.SOP
