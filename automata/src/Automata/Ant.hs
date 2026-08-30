{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

-- | Langton's ant: the case the stencil engine cannot take.
--
-- Wireworld and the game of life are /bulk/ rules --- every cell is rewritten
-- from its own neighbourhood, all at once, and a 'Stencil' is precisely the
-- table that makes that cheap. Langton's ant is not that. One cell changes per
-- step, chosen by where a walker happens to be, and the walker carries a
-- heading that no cell holds. Trying to write it as a rule over the grid means
-- storing the ant in the grid and rediscovering it every generation.
--
-- So it runs on 'Walker' instead: a 'FocusedGrid' and a heading, and
-- 'stepWalker' to move. The interesting part is that 'stepWalker' is /total/
-- --- a walker with a heading always lands somewhere --- and what "somewhere"
-- means is the axis type's business and nobody else's. That is why the
-- topology here is a command-line flag rather than a fork of the ant:
--
--   * on 'Periodic' the ant walks off one edge and back on the other;
--   * on 'Clamped' it walks into the wall and stays there, still turning and
--     still flipping the cell under it, because @('.+^')@ on a clamped axis
--     is a retraction and not a step;
--   * on 'Reflective' it bounces, and 'Data.Grid.Sized.Coord.transportCoord'
--     hands back a /reversed/ heading, which is the whole reason a walker
--     carries its heading through the step rather than adding a displacement
--     itself.
--
-- The three runs below are the same eleven lines of ant. Only the type
-- changes.
module Automata.Ant
  ( Topology (..),
    topologyName,
    run,
  )
where

import Control.Comonad (extract)
import Control.Comonad.Store (pos)
import Control.Lens ((%~), (&))
import Data.Grid.Sized
import Graphics.Gloss.Interface.Pure.Game

-- | Which boundary policy to give both axes.
data Topology
  = Torus
  | Walls
  | Mirror
  deriving (Eq, Show)

topologyName :: Topology -> String
topologyName Torus = "Periodic 101 (walks off one edge and back on the other)"
topologyName Walls = "Clamped 101 (walks into the wall and stays)"
topologyName Mirror = "Reflective 101 (bounces, heading reversed)"

-- | The constraints one ant needs of its board, in one name because they are
-- stated four times below.
type Ant cs =
  ( IsCoordList cs,
    AllSizedKnown cs,
    SListI cs,
    TransportCoordList cs,
    StepFrameFlips cs,
    AllDiffSame Int cs,
    All CentredAxis cs,
    MapDiff cs ~ '[Int, Int]
  )

-- | Turn ninety degrees. A heading is a 'Delta', which is indexed by the
-- /difference/ list and not by the axis list --- so these two are written once
-- and work on every 101x101 board below, whatever its axes do at the edge.
turnRight, turnLeft :: Delta '[Int, Int] -> Delta '[Int, Int]
turnRight (dx :^ dy :^ NoDelta) = dy :^ negate dx :^ NoDelta
turnLeft (dx :^ dy :^ NoDelta) = negate dy :^ dx :^ NoDelta

-- | The whole automaton.
--
-- On a white cell turn right, on a black cell turn left; flip the cell; step.
-- 'extract' is the cell under the focus and 'pos' is where the focus is ---
-- the two 'Control.Comonad.Store.ComonadStore' operations --- and
-- 'stepWalker' is the step. Nothing here mentions an index, a bound or an
-- edge.
stepAnt :: (Ant cs) => Walker cs Bool -> Walker cs Bool
stepAnt (Walker fg heading _) =
  let onBlack = extract fg
      heading'
        | onBlack = turnLeft heading
        | otherwise = turnRight heading
      fg' = fg & gridIndex (pos fg) %~ not
   in stepWalker (Walker fg' heading' False)

-- * The window

data World cs = World
  { worldAnt :: Walker cs Bool,
    worldSteps :: !Int,
    worldRunning :: Bool,
    -- | Steps per second.
    worldRate :: Float,
    worldOwed :: Float,
    worldTopology :: String,
    -- | The window's real size, from 'EventResize'. See 'fitTo'.
    worldWin :: (Int, Int)
  }

startWorld :: forall cs. (Ant cs) => (Int, Int) -> Float -> String -> World cs
startWorld win rate name =
  World
    { worldAnt =
        Walker
          (FocusedGrid (tabulateGrid (const False)) centreCoord)
          -- Heading @(0, 1)@: along the second axis, which is the one
          -- drawn upwards.
          (0 :^ 1 :^ NoDelta)
          False,
      worldSteps = 0,
      worldRunning = True,
      worldRate = rate,
      worldOwed = 0,
      worldTopology = name,
      worldWin = win
    }

-- | Run the ant on the board the topology names. The three arms differ only in
-- the axis type they instantiate at.
run :: Topology -> Float -> IO ()
run t rate =
  case t of
    Torus -> go @'[Periodic 101, Periodic 101]
    Walls -> go @'[Clamped 101, Clamped 101]
    Mirror -> go @'[Reflective 101, Reflective 101]
  where
    -- The two axes are named so that 'draw' can ask a coordinate for its
    -- pair of indices; the three arms below determine them.
    go :: forall cs a b. (Ant cs, cs ~ '[a, b]) => IO ()
    go =
      play
        (InWindow "Langton's ant -- grid-sized" defaultWindow (20, 20))
        (greyN 0.97)
        60
        (startWorld @cs defaultWindow rate (topologyName t))
        draw
        onEvent
        onTick

onTick :: (Ant cs) => Float -> World cs -> World cs
onTick dt w
  | not (worldRunning w) = w
  | otherwise =
      let owed = worldOwed w + dt * worldRate w
          n = floor owed :: Int
       in (iterate step1 w {worldOwed = owed - fromIntegral n} !! n)
  where
    step1 v = v {worldAnt = stepAnt (worldAnt v), worldSteps = worldSteps v + 1}

onEvent :: (Ant cs) => Event -> World cs -> World cs
onEvent (EventResize wh) w = w {worldWin = wh}
onEvent (EventKey (Char c) Down _ _) w = onKey c w
onEvent _ w = w

onKey :: (Ant cs) => Char -> World cs -> World cs
onKey 't' w = w {worldRunning = not (worldRunning w)}
onKey 'r' w = startWorld (worldWin w) (worldRate w) (worldTopology w)
onKey k w
  | k `elem` ("+=" :: String) = w {worldRate = min 8000 (worldRate w * 2)}
  | k `elem` ("-_" :: String) = w {worldRate = max 1 (worldRate w / 2)}
  | otherwise = w

tileSize :: Float
tileSize = 8

-- | Screen position of the centre of a cell, taken from its two axis indices
-- rather than through @('.-.')@, which is a displacement and would put the far
-- half of a 'Periodic' board off the left of the window.
--
-- 'coordIndices2' reads the divisor off the axis type, so this no longer needs
-- the board's side as a literal of its own to agree with three type-level
-- @101@s by hand (sized-grid-bzzy). It still reads no axis /type/, which is
-- what lets one drawing function serve all three topologies.
cellCentre :: (IsCoordList '[a, b]) => Coord '[a, b] -> (Float, Float)
cellCentre c =
  let (i, j) = coordIndices2 c
   in ( tileSize * (fromIntegral i - 50),
        tileSize * (fromIntegral j - 50) - 80
      )

draw :: forall cs a b. (Ant cs, cs ~ '[a, b]) => World cs -> Picture
draw w = fitTo (worldWin w) $ pictures (blacks ++ [ant, hud w])
  where
    fg = walkerGrid (worldAnt w)
    blacks =
      [ translate x y (color (greyN 0.1) (rectangleSolid tileSize tileSize))
      | c <- allCoord @cs,
        indexGrid (focusedGrid fg) c,
        let (x, y) = cellCentre c
      ]
    ant =
      let (x, y) = cellCentre (pos fg)
       in translate x y $
            color (makeColor 0.9 0.15 0.1 1) $
              rectangleSolid (tileSize + 2) (tileSize + 2)

hud :: World cs -> Picture
hud w =
  pictures
    ( zipWith bodyLine [0 :: Int ..] body
        ++ zipWith keyLine [0 :: Int ..] legend
    )
  where
    bodyLine i s =
      translate (-430) (455 - 26 * fromIntegral i) $
        scale 0.13 0.13 $
          color (greyN 0.2) $
            text s
    keyLine i s =
      translate (-430) (455 - 26 * 3 - 20 * fromIntegral i) $
        scale 0.1 0.1 $
          color (greyN 0.45) $
            text s
    legend =
      [ "keys:  t run/pause   r restart   +/- faster/slower",
        "       pick the board with --torus, --walls or --mirror"
      ]
    body =
      [ "Langton's ant on " ++ worldTopology w,
        "step "
          ++ show (worldSteps w)
          ++ "    "
          ++ ( if worldRunning w
                 then "running"
                 else "paused"
             ),
        show (round (worldRate w) :: Int) ++ " steps/s"
      ]

-- * Fitting the layout to the window

-- | The coordinate space everything above is laid out in.
designW, designH :: Float
designW = 900
designH = 1000

-- | The window this demo opens at, and the scale that keeps the layout inside
-- whatever size the window actually is.
--
-- The positions above are written against 'designW' by 'designH' and left
-- alone; the whole picture is scaled instead. Hardcoding the window is what
-- this used to do, and it does not work --- it asked for 900x1000 and was run
-- on a 1470x956 desktop, where a window taller than the screen cannot be seen.
-- Asking the screen how big it is does not work either: gloss's
-- @getScreenSize@ throws where GLFW cannot name a monitor. So: open small
-- enough for any laptop, then listen for 'EventResize' and fit. Drag the frame
-- and the whole thing rescales. See sized-grid-23y3.
defaultWindow :: (Int, Int)
defaultWindow = (740, 820)

fitTo :: (Int, Int) -> Picture -> Picture
fitTo (w, h) = scale k k
  where
    k = min (fromIntegral w / designW) (fromIntegral h / designH)
