{-# LANGUAGE DataKinds #-}

-- | What both halves of the demo agree on: the board, what a cell is, and the
-- events a viewer replays.
module Maze.Model
  ( -- * The board
    Cs
  , Tile(..)
  , side
  , solidGrid
  , coordAt
  , coordXY
  , startCell
  , goalCell
    -- * Moving on it
  , directions
  , twoSteps
  , traceTo
    -- * What the viewer replays
  , Move(..)
  ) where

import           Data.Grid.Sized

import           Control.Comonad.Store (pos)
import           Control.Lens          (review)
import           Data.Maybe            (fromMaybe)

-- | The board.
--
-- 'Clamped', which is the whole reason this demo is on this shape. A maze has
-- walls, and the interesting question at a wall is not \"what does the
-- coordinate become\" --- it is \"is there anything there at all\". On a
-- clamped axis 'offsetCoord' answers 'Nothing' rather than inventing an
-- answer, so the generator and the solver below never test an index against a
-- bound. They ask for the step and take 'Nothing' as the wall.
--
-- Odd, because a maze on a cell grid needs a wall between neighbouring cells:
-- the cells live at odd coordinates and the walls at even ones, so carving
-- from one cell to the next is a two-step walk that opens what it passes
-- through.
type Cs = '[ Clamped 61, Clamped 61]

side :: Int
side = 61

data Tile
    = Wall
    | Floor
    deriving (Eq, Show)

solidGrid :: Grid Cs Tile
solidGrid = tabulateGrid (const Wall)

coordAt :: Int -> Int -> Maybe (Coord Cs)
coordAt cx cy =
    (\a b -> review asOrdinal a :| review asOrdinal b :| EmptyCoord) <$>
    numToOrdinal cx <*>
    numToOrdinal cy

-- | A coordinate as its two axis indices. Row-major, so this is one division.
coordXY :: Coord Cs -> (Int, Int)
coordXY c = coordPosition c `divMod` side

-- | The corner cells the maze runs between. Both are odd, so both are cells
-- rather than walls, and the generator starts at the first.
startCell, goalCell :: Coord Cs
startCell = fromMaybe (error "startCell is off the board") (coordAt 1 1)
goalCell = fromMaybe (error "goalCell is off the board") (coordAt (side - 2) (side - 2))

-- | The four orthogonal steps.
--
-- A 'Delta' is indexed by the /difference/ list rather than the axis list, so
-- this table is written once for Z^2 and is not specific to @Cs@ --- the same
-- four values would drive a walk on a @Periodic@ or @Reflective@ board.
directions :: [Delta '[ Int, Int]]
directions =
    [ deltaFromTuple (1, 0)
    , deltaFromTuple (-1, 0)
    , deltaFromTuple (0, 1)
    , deltaFromTuple (0, -1)
    ]

-- | The same step twice: cell to wall to the next cell.
--
-- A 'Path' rather than a doubled displacement on purpose. 'walkPath' takes the
-- steps one at a time through 'offsetCoord', so a walk that would leave the
-- board halfway fails even where its net displacement would have landed back
-- on it --- which on a maze is the difference between \"there is a cell two
-- along\" and \"there is a cell two along and a wall I may open between here
-- and it\".
twoSteps :: Delta '[ Int, Int] -> Path Cs
twoSteps d = Path [d, d]

-- | Walk a 'Path' from the focus and report both where it landed and what is
-- there.
--
-- 'tracePath' gives the second half and 'walkPath' the first;  this is the
-- pair, from one walk, because every caller here wants both.
traceTo :: Path Cs -> FocusedGrid Cs Tile -> Maybe (Coord Cs, Tile)
traceTo p fg =
    (\c -> (c, indexGrid (focusedGrid fg) c)) <$> walkPath (pos fg) p

-- | One move of either phase, for the viewer to replay.
data Move
    = Opened (Coord Cs)
    -- ^ This cell has been carved out of the rock.
    | Moved (Coord Cs)
    -- ^ The carving head is now here --- either because it advanced, or
    -- because it ran out of room and backtracked to a cell it had left.
    | Reached (Coord Cs) Int
    -- ^ The solver reached this cell, this many steps from the start.
    | Solved [Coord Cs]
    -- ^ The route, start first.
    | Unreachable
    -- ^ The solver ran out of frontier without finding the goal.
