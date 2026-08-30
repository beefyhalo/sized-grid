-- | The backtracking search, as the stream of boards it passes through.
--
-- The solver used to be a function from a board to @Maybe Board@, which is
-- everything a caller needs and nothing a viewer can watch: the interesting
-- part --- which cell it is working on, what it tried there, and how far it
-- unwinds when that fails --- happened inside one call and was thrown away.
--
-- 'solveTrace' is the same search written to /emit/ that instead of
-- discarding it. It is lazy and its steps are produced one at a time, so a
-- renderer can pull a few per frame and let the rest go: the animation holds
-- the unconsumed tail and nothing else, and a search that would run for
-- minutes still starts drawing immediately.
--
-- 'solveBoard' is then the last step of the trace, so the answer and the
-- animation cannot disagree --- there is only one search here.
module Sudoku.Solve
  ( Step (..),
    stepBoard,
    stepFocus,
    solveTrace,
    solveBoard,
    traceStats,
    Stats (..),
  )
where

import Data.Grid.Sized hiding (All, Compose)
import Data.List (find)
import Data.Maybe (isJust, isNothing)
import Sudoku.Board

-- | One move of the search.
data Step
  = -- | A candidate was written into an empty cell. The board is the one
    -- /after/ the write.
    Place (Coord Cs) Symbol Board
  | -- | Everything below a write failed, so the cell is empty again. The board
    -- is the one after the erasure --- that is, the board the write was made
    -- against.
    Undo (Coord Cs) Board
  | -- | The last step of every trace: the solved board, or 'Nothing' if the
    -- search exhausted every possibility.
    Done (Maybe Board)

stepBoard :: Step -> Maybe Board
stepBoard (Place _ _ b) = Just b
stepBoard (Undo _ b) = Just b
stepBoard (Done b) = b

-- | The cell a step is about, for a renderer to highlight.
stepFocus :: Step -> Maybe (Coord Cs)
stepFocus (Place p _ _) = Just p
stepFocus (Undo p _) = Just p
stepFocus (Done _) = Nothing

-- | The search, emitting every write and every erasure in the order it makes
-- them, and ending in exactly one 'Done'.
--
-- The only pruning is 'candidateAllowed', which is what the search did before:
-- a symbol goes in only where its row, column and square do not already hold
-- it. That makes every board the search reaches a legal one, so a board with
-- no empty cell left is solved by construction and needs no second check.
solveTrace :: Board -> [Step]
solveTrace board0
  | gameIsInvalid board0 = [Done Nothing]
  | otherwise = go board0 (\() -> [Done Nothing])
  where
    -- @go board fail@ emits the search below @board@, running @fail@ if that
    -- subtree has no solution. Writing the failure continuation explicitly is
    -- what lets the 'Undo' events come out in the right order and at the
    -- right time: an erasure is emitted when, and only when, the subtree under
    -- the write it undoes has been exhausted.
    go board onFail =
      case findEmpty board of
        Nothing -> [Done (Just board)]
        Just point ->
          let tryAll [] = onFail ()
              tryAll (s : ss) =
                let board' = placeSymbol point (Just s) board
                 in Place point s board'
                      : go board' (\() -> Undo point board : tryAll ss)
           in tryAll
                [ s
                | s <- indexGrid (allValues board) point,
                  candidateAllowed point board s
                ]
    findEmpty :: Board -> Maybe (Coord Cs)
    findEmpty currentBoard =
      find (isNothing . indexGrid currentBoard) allCoord

-- | The answer, as the end of the trace.
solveBoard :: Board -> Maybe Board
solveBoard = end . solveTrace
  where
    end [Done b] = b
    end (_ : ss) = end ss
    end [] = Nothing

-- | What a whole trace cost, for the text mode to report. Forces the entire
-- search, so only worth asking on a puzzle you are prepared to wait for.
data Stats = Stats
  { statsPlacements :: !Int,
    statsBacktracks :: !Int,
    statsSolved :: !Bool
  }

traceStats :: [Step] -> Stats
traceStats = go 0 0
  where
    go !p !u (Place {} : ss) = go (p + 1) u ss
    go !p !u (Undo {} : ss) = go p (u + 1) ss
    go !p !u (Done b : _) = Stats p u (isJust b)
    go !p !u [] = Stats p u False
