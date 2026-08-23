{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | The `Grid` type, the safe operations over it, and a re-export of the rest
-- of the public API.
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
    , cell
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
    , permuteGrid
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
    , axis
    , scanAxis
      -- * Windows and tiles
    , ShrinkableGrid(..)
    , gridTiles
    , tiles
    , gridWindows
    , windows
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
