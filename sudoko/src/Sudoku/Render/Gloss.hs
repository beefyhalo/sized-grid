-- | The animated view of the search: a window that replays 'solveTrace' one
-- step at a time.
--
-- Given clues, symbols the search has written and the cell it is working on
-- are all drawn differently, so what the window shows is not just the board
-- but the /shape of the search/ --- a run of writes marching down the grid,
-- then a burst of erasures unwinding back up it.
module Sudoku.Render.Gloss
  ( animate,
  )
where

import Data.Grid.Sized (Coord, allCoord, indexGrid)
import Data.Maybe (isJust)
import Graphics.Gloss.Interface.Pure.Game
import Sudoku.Board
import Sudoku.Solve

-- | What happened to the cell the search is looking at.
data Highlight
  = Written
  | Erased
  deriving (Eq)

data Status
  = Searching
  | Solved
  | NoSolution
  deriving (Eq)

data View = View
  { -- | The board as it was handed in. A cell is a clue exactly when this one
    -- has a symbol there, which is the only thing that distinguishes a clue
    -- from a symbol the search happened to get right.
    viewGivens :: Board,
    viewBoard :: Board,
    viewFocus :: Maybe (Coord Cs, Highlight),
    -- | The unconsumed tail of the trace. Held rather than the whole trace, so
    -- the steps already drawn become garbage and a long search costs no more
    -- memory than a short one.
    viewRest :: [Step],
    viewSteps :: !Int,
    viewPlaced :: !Int,
    viewUndone :: !Int,
    viewStatus :: Status,
    viewRunning :: Bool,
    -- | Steps per second.
    viewRate :: Float,
    -- | Steps earned by elapsed time but not yet taken, so a rate below one
    -- frame per step is still honoured exactly.
    viewOwed :: Float,
    -- | The window's real size, from 'EventResize'. See 'fitTo'.
    viewWin :: (Int, Int)
  }

startView :: (Int, Int) -> Float -> Board -> View
startView win rate board =
  View
    { viewGivens = board,
      viewBoard = board,
      viewFocus = Nothing,
      viewRest = solveTrace board,
      viewSteps = 0,
      viewPlaced = 0,
      viewUndone = 0,
      viewStatus = Searching,
      viewRunning = True,
      viewRate = rate,
      viewOwed = 0,
      viewWin = win
    }

-- | Replay a board's search in a window. Blocks until the window is closed.
animate :: Float -> Board -> IO ()
animate rate board =
  play
    (InWindow "Sudoku -- grid-sized" defaultWindow (20, 20))
    white
    60
    (startView defaultWindow rate board)
    draw
    onEvent
    onTick

-- * Stepping

-- | Take one step of the trace.
step1 :: View -> View
step1 v =
  case viewRest v of
    [] -> v {viewStatus = NoSolution, viewRunning = False}
    (s : ss) ->
      let v' = v {viewRest = ss, viewSteps = viewSteps v + 1}
       in case s of
            Place p _ b ->
              v'
                { viewBoard = b,
                  viewFocus = Just (p, Written),
                  viewPlaced = viewPlaced v + 1
                }
            Undo p b ->
              v'
                { viewBoard = b,
                  viewFocus = Just (p, Erased),
                  viewUndone = viewUndone v + 1
                }
            Done mb ->
              v'
                { viewBoard = maybe (viewBoard v) id mb,
                  viewFocus = Nothing,
                  viewStatus = if isJust mb then Solved else NoSolution,
                  viewRunning = False
                }

onTick :: Float -> View -> View
onTick dt v
  | not (viewRunning v) = v
  | otherwise =
      let owed = viewOwed v + dt * viewRate v
          n = floor owed :: Int
       in go n v {viewOwed = owed - fromIntegral n}
  where
    go 0 w = w
    go k w
      | viewRunning w = go (k - 1) (step1 w)
      | otherwise = w

-- * Input

onEvent :: Event -> View -> View
onEvent (EventResize wh) v = v {viewWin = wh}
onEvent (EventKey (Char c) Down _ _) v = onKey c v
onEvent (EventKey (SpecialKey KeySpace) Down _ _) v = toggleRunning v
onEvent _ v = v

toggleRunning :: View -> View
toggleRunning v
  -- Once the search is over there is nothing to resume, so space restarts
  -- rather than silently doing nothing.
  | viewStatus v /= Searching =
      startView (viewWin v) (viewRate v) (viewGivens v)
  | otherwise = v {viewRunning = not (viewRunning v)}

onKey :: Char -> View -> View
onKey 'p' v = toggleRunning v
onKey 'r' v = startView (viewWin v) (viewRate v) (viewGivens v)
onKey 's' v
  | viewStatus v == Searching = (step1 v) {viewRunning = False}
  | otherwise = v
onKey k v
  -- Both spellings of each key: a keyboard that needs shift for @+@ reports
  -- @+@, and one that does not reports @=@.
  | k `elem` ("+=" :: String) = v {viewRate = min 20000 (viewRate v * 2)}
  | k `elem` ("-_" :: String) = v {viewRate = max 1 (viewRate v / 2)}
  | otherwise = v

-- * Drawing

-- | Side of one cell, in pixels.
cellSize :: Float
cellSize = 56

-- | Screen position of the centre of cell @(row, col)@. Row 0 is the top row,
-- so the row index counts downwards while the screen's y counts up.
cellCentre :: Int -> Int -> (Float, Float)
cellCentre r c =
  ( -4 * cellSize + fromIntegral c * cellSize,
    116 - fromIntegral r * cellSize
  )

givenFill, plainFill, writtenFill, erasedFill :: Color
givenFill = greyN 0.88
plainFill = white
writtenFill = makeColor 0.72 0.92 0.72 1
erasedFill = makeColor 0.97 0.76 0.76 1

givenInk, writtenInk :: Color
givenInk = black
writtenInk = makeColor 0.1 0.25 0.7 1

draw :: View -> Picture
draw v =
  fitTo (viewWin v) $
    pictures [pictures (map cellPicture allCoord), boardLines, hud v]
  where
    cellPicture p =
      let (r, c) = coordRowCol p
          (x, y) = cellCentre r c
          isGiven = isJust (indexGrid (viewGivens v) p)
          fill =
            case viewFocus v of
              Just (q, h)
                | q == p ->
                    if h == Written
                      then writtenFill
                      else erasedFill
              _
                | isGiven -> givenFill
                | otherwise -> plainFill
          ink =
            if isGiven
              then givenInk
              else writtenInk
          glyph =
            case indexGrid (viewBoard v) p of
              Nothing -> blank
              Just s ->
                -- gloss draws text from the baseline at the origin
                -- rightwards; a GLUT stroke digit is about 104 units
                -- wide and 100 tall, so half of each at this scale is
                -- what puts the glyph in the middle of its cell.
                translate (-15) (-15) $
                  scale 0.3 0.3 $
                    color ink $
                      text [symbolChar s]
       in translate x y $
            pictures
              [ color fill $ rectangleSolid cellSize cellSize,
                glyph
              ]

-- | The cell rules, with every third one heavier so the 3x3 squares --- the
-- slices 'squares' cuts out --- are visible.
boardLines :: Picture
boardLines = pictures (map vertical edges ++ map horizontal edges)
  where
    edges = [0 .. 9 :: Int]
    lo = -4.5 * cellSize
    hi = 4.5 * cellSize
    top = 116 + 0.5 * cellSize
    at i = lo + fromIntegral i * cellSize
    ink i =
      if i `mod` 3 == 0
        then black
        else greyN 0.75
    thickness i =
      if i `mod` 3 == 0
        then 2
        else 1
    vertical i =
      color (ink i) $
        polygon
          [ (at i - thickness i, top - 9 * cellSize),
            (at i + thickness i, top - 9 * cellSize),
            (at i + thickness i, top),
            (at i - thickness i, top)
          ]
    horizontal i =
      let y = top - fromIntegral i * cellSize
       in color (ink i) $
            polygon
              [ (lo, y - thickness i),
                (hi, y - thickness i),
                (hi, y + thickness i),
                (lo, y + thickness i)
              ]

hud :: View -> Picture
hud v =
  pictures
    ( zipWith bodyLine [0 :: Int ..] body
        ++ zipWith keyLine [0 :: Int ..] legend
    )
  where
    bodyLine i s =
      translate (-300) (360 - 26 * fromIntegral i) $
        scale 0.13 0.13 $
          color (greyN 0.2) $
            text s
    keyLine i s =
      translate (-300) (360 - 26 * 4 - 20 * fromIntegral i) $
        scale 0.1 0.1 $
          color (greyN 0.45) $
            text s
    legend =
      [ "keys:  space / p run/pause   s single step   r restart",
        "       +/- faster/slower"
      ]
    body =
      [ "Sudoku by backtracking search",
        "step "
          ++ show (viewSteps v)
          ++ "    "
          ++ show (viewPlaced v)
          ++ " placed    "
          ++ show (viewUndone v)
          ++ " backtracked",
        status
          ++ "    "
          ++ ( if viewRunning v
                 then "running"
                 else "paused"
             ),
        showRate (viewRate v) ++ " steps/s"
      ]
    status =
      case viewStatus v of
        Searching -> "searching"
        Solved -> "solved"
        NoSolution -> "no solution"

-- | A rate without a trailing @.0@ on the whole numbers it usually is.
showRate :: Float -> String
showRate x
  | x >= 1 = show (round x :: Int)
  | otherwise = show x

-- * Fitting the layout to the window

-- | The coordinate space everything above is laid out in, the window this demo
-- opens at, and the scale between them.
--
-- See the note on @Automata.Ant.fitTo@ in the automata example: the layout is
-- written against a fixed space and the picture is scaled, because a hardcoded
-- window can be taller than the screen and gloss's @getScreenSize@ throws
-- rather than saying it does not know. sized-grid-23y3.
designW, designH :: Float
designW = 640
designH = 780

defaultWindow :: (Int, Int)
defaultWindow = (640, 780)

fitTo :: (Int, Int) -> Picture -> Picture
fitTo (w, h) = scale k k
  where
    k = min (fromIntegral w / designW) (fromIntegral h / designH)
