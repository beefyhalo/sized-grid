{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

-- | How a board looks, and the question sized-grid-lopy.1 exists to answer:
-- how do you draw a surface that does not lie flat, so a player can /plan/ a
-- move through the seam rather than discover it?
--
-- Three views, switchable with a key, because the answer was never going to
-- come from reasoning about it:
--
--   * 'Flat' draws the strip as the rectangle it is stored as, and then draws
--     the far side of the seam past each edge --- dimmed, and in the row the
--     mirror sends it to. The whole level is visible at once, which is what
--     Sokoban normally needs, and the consequence of a push through the seam
--     is on screen before the push is made. What it cannot show is that the
--     two edges /are/ the same edge. It says so, and the player believes it.
--
--   * 'Centred' puts the player in the middle and draws the surface in the
--     player's own frame, out to a fixed radius. Locally ordinary, globally
--     impossible, which is what living on a Mobius strip is like --- and
--     exactly the difficulty: crossing the seam looks like nothing at all,
--     because in the player's frame nothing happened.
--
--   * 'Band' draws the strip as the band it is, twist and all, and is the one
--     view that does not ask the player to take the surface on trust. It is
--     also the one view nobody can play in: half the board is round the back.
--     See "Sokoban.Band", and sized-grid-lopy.6.
--
-- Everything here draws a board, and nothing here knows what a game is doing
-- around one. The window, its screens and its keys are "Sokoban.Shell". The
-- two were one module until sized-grid-lopy.8, which is when the board stopped
-- being the whole of what is on screen.
--
-- The layout is written in a fixed design space and the whole picture scaled
-- to the window, rather than laid out against the window's real size. See the
-- note on @Maze.Render.fitTo@: a hardcoded window can be taller than the
-- screen, and gloss's @getScreenSize@ throws rather than saying it does not
-- know. sized-grid-23y3.
module Sokoban.Render
  ( -- * The views
    View (..),
    viewName,
    viewNote,
    drawView,

    -- * Boards drawn somewhere other than the board box
    bandInto,
    thumbnail,

    -- * Laying a picture out
    boardBox,
    fitInto,

    -- * The palette the shell writes in
    background,
    inkColour,
    faintColour,
    seamColour,
    goalColour,
    doneColour,
    playerColour,
    markColour,
  )
where

import Data.Char (toUpper)
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Sokoban.Band (Band, bandExtent, bandPicture)
import Sokoban.Board
import Sokoban.Rules

-- | Which view is on screen.
data View
  = Flat
  | Centred
  | Band
  deriving (Eq, Show, Enum, Bounded)

viewName :: View -> String
viewName Flat = "flat, with the far side of each edge drawn past it"
viewName Centred = "centred, in the player's own frame"
viewName Band = "the band itself, one edge and no ends"

-- | The one sentence a view needs said beside it, which is not what it is
-- called: this is what the player is meant to read off it.
--
-- Takes the surface because the band's sentence is a claim about an edge, and
-- two of the three surfaces have none --- and on those there is no band drawn
-- for it to be a claim about.
viewNote :: Surface -> View -> String
viewNote _ Flat =
  "the coloured tabs pair the edges: leave one and you arrive at the tab of the same colour"
viewNote _ Centred =
  "drawn in the player's frame, so crossing a seam looks like nothing at all"
viewNote surface Band
  | surfaceIsBand surface =
      "the pink line is the edge of the strip: one line, twice round, and there is only the one"
  | otherwise =
      "this surface has no picture in space -- press v for one that has"

-- * Colours

background,
  wallColour,
  floorColour,
  voidColour,
  goalColour,
  crateColour,
  doneColour,
  playerColour,
  seamColour,
  markColour,
  edgeColour,
  inkColour,
  faintColour ::
    Color
background = greyN 0.12
wallColour = greyN 0.3
floorColour = greyN 0.72

-- | Off the strip altogether, which only the centred view can show.
voidColour = greyN 0.16

goalColour = makeColor 1 0.72 0.22 1

crateColour = makeColor 0.74 0.47 0.26 1

doneColour = makeColor 0.35 0.82 0.42 1

playerColour = makeColor 0.35 0.66 0.96 1

-- | The two edges that are not edges. Deliberately loud: everything this game
-- is about happens here.
seamColour = makeColor 0.95 0.35 0.75 1

-- | A cell being called out. White, and not the seam's pink, because the one
-- thing it is used for --- the lap a finished level takes around the strip ---
-- passes over the seam tabs and has to stay visible on top of them.
markColour = makeColor 1 1 1 1

-- | The two that are.
edgeColour = greyN 0.45

inkColour = greyN 0.9

faintColour = greyN 0.55

-- | One colour per pair of cells a seam joins, keyed on the pair rather than
-- on any particular gluing: the tab where a step leaves and the tab where it
-- arrives ask with the same two numbers the other way round, so they come out
-- the same colour whatever the seam does.
--
-- Distinct hues rather than a ramp, because what the player has to read off is
-- which two edges are the same edge, and \"these two are the same colour\" is a
-- question a ramp makes harder rather than easier.
pairColour :: Int -> Int -> Color
pairColour i j = cycle palette !! min i j
  where
    palette =
      [ makeColor 0.95 0.35 0.75 1,
        makeColor 0.35 0.85 0.85 1,
        makeColor 0.95 0.75 0.25 1,
        makeColor 0.55 0.85 0.4 1,
        makeColor 0.7 0.6 0.95 1
      ]

-- * Drawing one cell

tile :: Float
tile = 34

-- | One cell and whatever is standing on it.
--
-- The @mark@ is a cell to call out, drawn last so that it survives whatever is
-- under it. Putting it here rather than in each view is what gets it drawn in
-- the ghost columns too, and that is the point of it: a marked cell shows up
-- at both ends of the seam at once, which is exactly what the ghosts exist to
-- say.
cellPicture :: (KnownStrip w h) => Frame -> Maybe (Spot w h) -> Game w h -> Spot w h -> Picture
cellPicture frame mark game s =
  pictures $
    [color (base here) (rectangleSolid tile tile)]
      ++ concat
        [ [ color (withAlpha 0.4 goalColour) (rectangleSolid (tile - 12) (tile - 12)),
            color goalColour (rectangleWire (tile - 9) (tile - 9))
          ]
        | here == Goal
        ]
      ++ [ color
             ( if onGoal
                 then doneColour
                 else crateColour
             )
             (rectangleSolid (tile - 9) (tile - 9))
         | hasCrate
         ]
      ++ [color playerColour (circleSolid (tile / 2 - 5)) | isPlayer]
      ++ [ color background (facingWedge (dirOf frame (playTurn now) (playFacing now)))
         | isPlayer
         ]
      ++ [color markColour (rectangleWire (tile - 3) (tile - 3)) | marked]
      ++ [color markColour (rectangleWire (tile - 7) (tile - 7)) | marked]
  where
    now = gamePlay game
    here = tileAt game s
    onGoal = Set.member (spotCoord s) (levelGoals (gameLevel game))
    hasCrate = crateAt now s
    isPlayer = spotCoord s == spotCoord (playPlayer now)
    marked = fmap spotCoord mark == Just (spotCoord s)
    base Wall = wallColour
    base _ = floorColour

-- | Which way the player last moved, as a wedge on the player's disc.
--
-- Not a rule --- a Sokoban pushes by walking into a crate, so facing is never
-- an input --- but on this surface what the next step does depends on which
-- way it points, so it is worth seeing.
facingWedge :: Dir -> Picture
facingWedge dir =
  rotate turn (translate 0 (tile / 5) (polygon [(-4, -4), (4, -4), (0, 4)]))
  where
    turn =
      case dir of
        DirUp -> 0
        DirRight -> 90
        DirDown -> 180
        DirLeft -> 270

-- | Dim a finished picture by laying the background over it. gloss has no
-- opacity for a 'Picture', and the cells inside set their own colours, so a
-- 'color' wrapper would not reach them.
dimmed :: Float -> Picture -> Picture
dimmed amount p =
  pictures [p, color (withAlpha amount background) (rectangleSolid tile tile)]

-- * The flat view

-- | How far past each side the picture is continued, in cells: sideways and
-- upwards. Zero where the surface has a genuine edge there, since there is
-- nothing on the far side of one to draw.
ghostDepth :: forall w h. (KnownStrip w h) => Surface -> (Int, Int)
ghostDepth surface = (depth (around, 0) 3 around, depth (0, across) 2 across)
  where
    (around, across) = stripSize @w @h
    depth just_outside most n
      | isJust (spotBeyond @w @h surface just_outside) = min most n
      | otherwise = 0

drawFlat ::
  forall w h.
  (KnownStrip w h) =>
  Surface ->
  Frame ->
  Maybe (Spot w h) ->
  Game w h ->
  Picture
drawFlat surface frame mark game =
  pictures (concat [ghostCells, boardCells, seamTags, edges])
  where
    (around, across) = stripSize @w @h
    halfW = fromIntegral around * tile / 2
    halfH = fromIntegral across * tile / 2
    (ghostCols, ghostRows) = ghostDepth @w @h surface
    beyond = spotBeyond @w @h surface
    -- Column @x@, row @y@ of the picture, either of which may be outside the
    -- board: that is what a ghost is.
    place x y =
      translate
        (tile * (fromIntegral x - fromIntegral (around - 1) / 2))
        (tile * (fromIntegral y - fromIntegral (across - 1) / 2))
    boardCells =
      [ place x y (maybe blank (cellPicture frame mark game) (spotAt @w @h x y))
      | x <- [0 .. around - 1],
        y <- [0 .. across - 1]
      ]
    -- What is past each side, drawn dimmed where the picture would run out.
    -- The consequence of a push through a seam is on screen before the push is
    -- made, which is the whole reason this view exists.
    ghostCells =
      [ place col row (dimmed 0.62 (cellPicture frame mark game s))
      | (col, row, s) <-
          [ (col, y, s)
          | y <- [0 .. across - 1],
            k <- [1 .. ghostCols],
            col <- [around - 1 + k, -k],
            Just s <- [beyond (col, y)]
          ]
            ++ [ (x, row, s)
               | x <- [0 .. around - 1],
                 k <- [1 .. ghostRows],
                 row <- [across - 1 + k, -k],
                 Just s <- [beyond (x, row)]
               ]
      ]
    -- A tab outside every glued half-edge, coloured by the half-edge it leads
    -- to. Leaving here puts you at the tab of the same colour, and the player
    -- reads the pairing off the picture instead of being told it.
    seamTags = concatMap tag edgeCells
    edgeCells =
      [(around - 1, y, DirRight) | y <- [0 .. across - 1]]
        ++ [(0, y, DirLeft) | y <- [0 .. across - 1]]
        ++ [(x, across - 1, DirUp) | x <- [0 .. around - 1]]
        ++ [(x, 0, DirDown) | x <- [0 .. around - 1]]
    tag (x, y, dir) =
      case beyond (outside (x, y) dir) of
        Nothing -> []
        Just there ->
          let (x', y') = spotXY there
              sideways = dir == DirLeft || dir == DirRight
              hue
                | sideways = pairColour y y'
                | otherwise = pairColour x x'
              at
                | sideways = translate (edgeOf dir) (cellAt y across)
                | otherwise = translate (cellAt x around) (edgeOf dir)
              bar
                | sideways = rectangleSolid 5 (tile - 3)
                | otherwise = rectangleSolid (tile - 3) 5
           in [at (color hue bar)]
    outside (x, y) DirRight = (x + 1, y)
    outside (x, y) DirLeft = (x - 1, y)
    outside (x, y) DirUp = (x, y + 1)
    outside (x, y) DirDown = (x, y - 1)
    edgeOf DirRight = halfW + 2
    edgeOf DirLeft = -halfW - 2
    edgeOf DirUp = halfH + 2
    edgeOf DirDown = -halfH - 2
    cellAt i n = tile * (fromIntegral i - fromIntegral (n - 1) / 2)
    -- And a plain grey line along any side the surface genuinely ends at, so
    -- that a boundary and a seam do not look alike. A Mobius strip has two of
    -- these and the other surfaces have none.
    edges =
      [ color edgeColour (line (span' dir))
      | dir <- [DirUp, DirDown, DirLeft, DirRight],
        ghostFor dir == 0
      ]
    ghostFor DirUp = ghostRows
    ghostFor DirDown = ghostRows
    ghostFor _ = ghostCols
    span' DirUp = [(-halfW - 12, halfH), (halfW + 12, halfH)]
    span' DirDown = [(-halfW - 12, -halfH), (halfW + 12, -halfH)]
    span' DirLeft = [(-halfW, -halfH - 12), (-halfW, halfH + 12)]
    span' DirRight = [(halfW, -halfH - 12), (halfW, halfH + 12)]

flatSize :: forall w h. (KnownStrip w h) => Surface -> Game w h -> (Float, Float)
flatSize surface _ =
  ( fromIntegral (around + 2 * ghostCols) * tile,
    fromIntegral (across + 2 * ghostRows) * tile
  )
  where
    (around, across) = stripSize @w @h
    (ghostCols, ghostRows) = ghostDepth @w @h surface

-- * The player-centred view

-- | How far around the player the surface is drawn.
radius :: Int
radius = 5

drawCentred ::
  forall w h.
  (KnownStrip w h) =>
  Surface ->
  Frame ->
  Maybe (Spot w h) ->
  Game w h ->
  Picture
drawCentred surface frame mark game = pictures (cells ++ [horizon])
  where
    now = gamePlay game
    origin = (playPlayer now, playTurn now)
    cells =
      [ translate (tile * fromIntegral dx) (tile * fromIntegral dy) $
          case cellAround surface origin (dx, dy) of
            Nothing -> color voidColour (rectangleSolid (tile - 2) (tile - 2))
            Just (s, _) -> cellPicture frame mark game s
      | dx <- [-radius .. radius],
        dy <- [-radius .. radius]
      ]
    -- A ring at the edge of what is drawn, so the view reads as a window on a
    -- surface rather than as a board with a border.
    horizon =
      color (withAlpha 0.5 seamColour) $
        rectangleWire (tile * fromIntegral (2 * radius + 1)) (tile * fromIntegral (2 * radius + 1))

centredSize :: (Float, Float)
centredSize = (side, side)
  where
    side = tile * fromIntegral (2 * radius + 1)

-- * The band

-- | The strip as a band, with every cell reduced to the one colour a facet has
-- room for.
drawBand :: forall w h. (KnownStrip w h) => Band -> Maybe (Spot w h) -> Game w h -> Picture
drawBand band mark game = bandPicture band around across paint
  where
    (around, across) = stripSize @w @h
    paint x y =
      case spotAt @w @h x y of
        Nothing -> voidColour
        Just s
          | fmap spotCoord mark == Just (spotCoord s) -> markColour
          | otherwise -> cellColour game s

bandSize :: forall w h. (KnownStrip w h) => Band -> Game w h -> (Float, Float)
bandSize band _ = bandExtent band around across
  where
    (around, across) = stripSize @w @h

-- | The band, blown up to fill a box of its own. What a title screen wants:
-- the picture, and none of the window that usually surrounds it.
bandInto :: (KnownStrip w h) => (Float, Float) -> Band -> Game w h -> Picture
bandInto box band g = fitInto 8 box (bandSize band g) (drawBand band Nothing g)

-- | A cell as a single colour.
--
-- 'cellPicture' draws a goal as a ring inside its cell and a crate as a square
-- inside that, which is how a player tells a covered goal from an uncovered
-- one at a glance. None of that survives being wrapped round a band at this
-- size, so the order here is by what matters: whoever is standing on the cell
-- first, then the crate, then the terrain.
cellColour :: (KnownStrip w h) => Game w h -> Spot w h -> Color
cellColour game s
  | spotCoord s == spotCoord (playPlayer now) = playerColour
  | crateAt now s = if onGoal then doneColour else crateColour
  | otherwise =
      case tileAt game s of
        Wall -> wallColour
        Goal -> goalColour
        Floor -> floorColour
  where
    now = gamePlay game
    onGoal = Set.member (spotCoord s) (levelGoals (gameLevel game))

-- * Thumbnails

-- | A level small enough to put eight of on one screen: the chart alone, one
-- flat colour per cell, no ghosts and no seam tabs.
--
-- Reduced the same way the band reduces a cell, and for the same reason ---
-- there is no room for a ring inside a square at this size --- so a level
-- recognised on the level screen looks the way it looks on the band.
thumbnail :: forall w h. (KnownStrip w h) => (Float, Float) -> Game w h -> Picture
thumbnail box game =
  fitInto 1 box (fromIntegral around * tile, fromIntegral across * tile) $
    pictures
      [ translate
          (tile * (fromIntegral x - fromIntegral (around - 1) / 2))
          (tile * (fromIntegral y - fromIntegral (across - 1) / 2))
          (color (colourAt x y) (rectangleSolid (tile - 1) (tile - 1)))
      | x <- [0 .. around - 1],
        y <- [0 .. across - 1]
      ]
  where
    (around, across) = stripSize @w @h
    colourAt x y = maybe voidColour (cellColour game) (spotAt @w @h x y)

-- * The window's board

-- | The area a level is drawn into, between the readout above it and the keys
-- below.
boardBox :: (Float, Float)
boardBox = (860, 470)

-- | One view of one game, scaled into 'boardBox'.
--
-- The @mark@ is a cell to call out, and every view draws it in its own terms:
-- a ring on the flat board and in its ghosts, a ring in the player's frame, a
-- white facet on the band. It is 'Nothing' for all of ordinary play.
drawView :: (KnownStrip w h) => View -> Band -> Frame -> Maybe (Spot w h) -> Game w h -> Picture
drawView view band frame mark g =
  translate 0 (-20) $
    case view of
      Flat -> fitInto 2.2 boardBox (flatSize surface g) (drawFlat surface frame mark g)
      Centred -> fitInto 2.2 boardBox centredSize (drawCentred surface frame mark g)
      -- A band is drawn at whatever size its own geometry comes out, and that
      -- is a long way under the box for a short strip, so it is allowed to be
      -- blown up much further than a board of cells would want to be. Nothing
      -- in it is a fixed number of pixels.
      Band
        | surfaceIsBand surface ->
            fitInto 6 boardBox (bandSize band g) (drawBand band mark g)
        | otherwise -> noBand surface
  where
    surface = gameSurface g

-- | What the band view has to say about a surface it cannot draw.
--
-- Not an empty screen and not a fallback to another view: the player asked for
-- this one, and the honest answer is that this surface does not fit in the
-- space the picture is drawn in.
noBand :: Surface -> Picture
noBand surface =
  pictures
    [ say 40 (greyN 0.75) (capital (surfaceTitle surface) ++ " does not fit in space."),
      say (-10) faintColour "It can only be drawn passing through itself, and this game",
      say (-46) faintColour "has no picture of that. Press v for one that does fit."
    ]
  where
    say y c str =
      translate (-(4.2 * fromIntegral (length str))) y (scale 0.13 0.13 (color c (text str)))
    capital (c : rest) = toUpper c : rest
    capital [] = []

-- | Scale a picture of a known size to fill a box, up to a limit the caller
-- sets. A one-row strip would otherwise be drawn at the height of the window,
-- and a wide one would run off the sides.
fitInto :: Float -> (Float, Float) -> (Float, Float) -> Picture -> Picture
fitInto most (boxW, boxH) (w, h) = scale k k
  where
    k = min most (min (boxW / w) (boxH / h))
