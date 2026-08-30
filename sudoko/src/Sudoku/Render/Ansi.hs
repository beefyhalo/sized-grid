-- | The same animation as "Sudoku.Render.Gloss", in a terminal.
--
-- Why this exists at all, and why it is thirty lines of escape codes rather
-- than a TUI framework, is the finding of sized-grid-nnww.6: a grid redraw
-- needs a cursor-home, a colour and a reset, and nothing a widget library
-- offers is worth a dependency tree the demo would then carry. The whole
-- vocabulary is 'home', 'clearScreen' and 'sgr' below.
--
-- Degrades on purpose. When stdout is not a terminal --- a pipe, a CI log ---
-- there is no cursor to move and no point pacing frames to a human, so the
-- escape codes and the delay are both dropped and the frames are simply
-- appended. That is the case the gloss front end cannot serve at all.
module Sudoku.Render.Ansi
  ( animate,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import Control.Monad (when)
import Data.Grid.Sized (Coord, allCoord, indexGrid)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, isJust)
import Sudoku.Board
import Sudoku.Solve
import System.IO
  ( BufferMode (..),
    hFlush,
    hIsTerminalDevice,
    hSetBuffering,
    stdout,
  )

-- * Escape codes

esc :: String -> String
esc s = "\ESC[" ++ s

home, clearScreen, hideCursor, showCursor :: String
home = esc "H"
clearScreen = esc "2J" ++ home
hideCursor = esc "?25l"
showCursor = esc "?25h"

-- | Select-graphic-rendition, by code: @0@ reset, @36@ cyan, @7@ reverse
-- video, @2@ dim.
sgr :: Int -> String
sgr n = esc (show n ++ "m")

-- * The loop

-- | Replay a board's search in the terminal at the given steps per second.
animate :: Float -> Board -> IO ()
animate rate givens = do
  tty <- hIsTerminalDevice stdout
  hSetBuffering stdout (BlockBuffering Nothing)
  let start = when tty (putStr (clearScreen ++ hideCursor))
      stop = when tty (putStr (showCursor ++ sgr 0)) >> hFlush stdout
  bracket_ start stop $ do
    final <- go tty (solveTrace givens) givens Nothing 0 0
    putStrLn ""
    putStrLn final
    hFlush stdout
  where
    -- At most fifty frames a second; above that the steps are batched into one
    -- frame rather than the terminal being asked to redraw faster than it can.
    frameRate = min 50 (max 1 rate)
    perFrame = max 1 (round (rate / frameRate)) :: Int
    delay = round (1e6 / frameRate) :: Int
    go tty steps board focus placed undone =
      case steps of
        [] -> pure "no solution (the trace ended without one)"
        _ -> do
          let (taken, rest) = splitAt perFrame steps
              (board', focus', placed', undone') =
                foldl apply (board, focus, placed, undone) taken
          frame tty board' focus' placed' undone'
          case [mb | Done mb <- taken] of
            (mb : _) ->
              pure $
                if isJust mb
                  then
                    "solved in "
                      ++ show placed'
                      ++ " placements and "
                      ++ show undone'
                      ++ " backtracks"
                  else
                    "no solution after "
                      ++ show placed'
                      ++ " placements and "
                      ++ show undone'
                      ++ " backtracks"
            [] -> do
              when tty (threadDelay delay)
              go tty rest board' focus' placed' undone'
    apply (board, _, placed, undone) s =
      case s of
        Place p _ b -> (b, Just (p, True), placed + 1, undone)
        Undo p b -> (b, Just (p, False), placed, undone + 1)
        Done mb -> (fromMaybe board mb, Nothing, placed, undone)
    frame tty board focus placed undone = do
      putStr (if tty then home else "\n")
      putStr (render tty givens board focus placed undone)
      hFlush stdout

-- * Drawing

render ::
  Bool ->
  Board ->
  Board ->
  Maybe (Coord Cs, Bool) ->
  Int ->
  Int ->
  String
render tty givensBoard board focus placed undone =
  unlines (concatMap bandRows [0 .. 2] ++ ["", status])
  where
    bandRows band =
      [rule | band > 0] ++ [row r | r <- [3 * band .. 3 * band + 2]]
    rule = "-------+-------+-------"
    -- Bands of three, joined by the same '|' the rule above puts a '+' at, so
    -- the 3x3 squares line up down the page.
    row r =
      intercalate
        "|"
        [ concat [" " ++ cellAt r c | c <- [3 * b .. 3 * b + 2]] ++ " "
        | b <- [0 .. 2 :: Int]
        ]
    cellAt r c =
      let p = coordAt r c
          given = isJust (indexGrid givensBoard p)
          here = indexGrid board p
          glyph = displaySymbol here
       in case focus of
            Just (q, wrote)
              | q == p -> paint (if wrote then 7 else 2) glyph
            _
              | given -> glyph
              | otherwise -> paint 36 glyph
    paint code s
      | tty = sgr code ++ s ++ sgr 0
      | otherwise = s
    status =
      show placed ++ " placed, " ++ show undone ++ " backtracked"

-- | The coordinate at a row and column, read out of the grid's own
-- enumeration rather than built here: 'allCoord' is row-major, so cell
-- @(r, c)@ is at @9r + c@ and this module needs no 'Ordinal' arithmetic of
-- its own.
coordAt :: Int -> Int -> Coord Cs
coordAt r c = allCoord !! (9 * r + c)
