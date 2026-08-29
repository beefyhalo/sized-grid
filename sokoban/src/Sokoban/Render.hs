{-# LANGUAGE DataKinds #-}

-- | The window, and the question sized-grid-lopy.1 exists to answer: how do
-- you draw a surface that does not lie flat, so a player can /plan/ a move
-- through the seam rather than discover it?
--
-- Two views, switchable with a key, because the answer was never going to come
-- from reasoning about it:
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
-- The layout is written in a fixed design space and the whole picture scaled
-- to the window, rather than laid out against the window's real size. See the
-- note on @Maze.Render.fitTo@: a hardcoded window can be taller than the
-- screen, and gloss's @getScreenSize@ throws rather than saying it does not
-- know. sized-grid-23y3.
module Sokoban.Render
  ( View(..)
  , viewName
  , App(..)
  , Session(..)
  , newApp
  , defaultWindow
  , drawApp
  , onEvent
  , run
  ) where

import           Sokoban.Board
import           Sokoban.Level
import           Sokoban.Rules

import           Data.Maybe                         (fromMaybe)
import qualified Data.Set                           as Set
import           Graphics.Gloss.Interface.Pure.Game

-- | Which of the two candidate views is on screen.
data View
    = Flat
    | Centred
    deriving (Eq, Show)

viewName :: View -> String
viewName Flat    = "flat, with the far side of each edge drawn past it"
viewName Centred = "centred, in the player's own frame"

-- | A game whose board size is known only at run time. gloss's state is one
-- type and a list of levels is a list of sizes, so the size is packed away
-- here and unpacked by whatever needs it.
data Session where
    Session :: KnownStrip w h => Game w h -> Session

data App = App
    { appLevels  :: [SomeLevel]
    , appIndex   :: !Int
    , appGame    :: Session
    , appView    :: View
    , appFrame   :: Frame
    , appWin     :: (Int, Int)
    -- ^ The window's real size, from 'EventResize'.
    , appOutcome :: Outcome
    }

newApp :: (Int, Int) -> [SomeLevel] -> App
newApp win levels =
    App
    { appLevels = levels
    , appIndex = 0
    , appGame = sessionAt levels 0
    , appView = Flat
    , appFrame = ChartFrame
    , appWin = win
    , appOutcome = Walked
    }

sessionAt :: [SomeLevel] -> Int -> Session
sessionAt levels i =
    case drop (i `mod` max 1 (length levels)) levels of
        (SomeLevel l:_) -> Session (newGame l)
        []              -> error "sessionAt: no levels"

defaultWindow :: (Int, Int)
defaultWindow = (900, 700)

-- | Open the window. Blocks until it is closed.
run :: [SomeLevel] -> IO ()
run [] = error "run: no levels"
run levels =
    play
        (InWindow "Sokoban on a Mobius strip" defaultWindow (40, 40))
        background
        30
        (newApp defaultWindow levels)
        drawApp
        onEvent
        (const id)

-- * Input

onEvent :: Event -> App -> App
onEvent (EventResize wh) app = app {appWin = wh}
onEvent (EventKey k Down _ _) app = onKey k app
onEvent _ app = app

onKey :: Key -> App -> App
onKey (SpecialKey KeyLeft) app = press DirLeft app
onKey (SpecialKey KeyRight) app = press DirRight app
onKey (SpecialKey KeyUp) app = press DirUp app
onKey (SpecialKey KeyDown) app = press DirDown app
onKey (Char c) app =
    case c of
        'a' -> press DirLeft app
        'h' -> press DirLeft app
        'd' -> press DirRight app
        'l' -> press DirRight app
        'w' -> press DirUp app
        'k' -> press DirUp app
        's' -> press DirDown app
        'j' -> press DirDown app
        'u' -> onSession (\g -> fromMaybe g (undo g)) app
        'r' -> onSession restart app
        'n' -> goTo (appIndex app + 1) app
        'p' -> goTo (appIndex app - 1) app
        'v' -> app {appView = flipView (appView app)}
        'f' -> app {appFrame = flipFrame (appFrame app)}
        _   -> app
  where
    flipView Flat     = Centred
    flipView Centred  = Flat
    flipFrame ChartFrame  = PlayerFrame
    flipFrame PlayerFrame = ChartFrame
onKey _ app = app

goTo :: Int -> App -> App
goTo i app =
    app
    {appIndex = wrapped, appGame = sessionAt (appLevels app) wrapped, appOutcome = Walked}
  where
    n = max 1 (length (appLevels app))
    wrapped = (i + n) `mod` n

onSession :: (forall w h. KnownStrip w h => Game w h -> Game w h) -> App -> App
onSession f app =
    case appGame app of
        Session g -> app {appGame = Session (f g)}

press :: Dir -> App -> App
press dir app =
    case appGame app of
        Session g ->
            let (g', o) = move (appFrame app) dir g
            in app {appGame = Session g', appOutcome = o}

-- * Colours

background, wallColour, floorColour, voidColour, goalColour, crateColour, doneColour, playerColour, seamColour, edgeColour, inkColour, faintColour ::
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

-- | One colour per pair of rows the seam joins. Distinct hues rather than a
-- ramp, because what the player has to read off is which two rows are the same
-- row, and \"these two are the same colour\" is a question a ramp makes harder
-- rather than easier.
pairColour :: Int -> Int -> Color
pairColour across y = cycle palette !! min y (across - 1 - y)
  where
    palette =
        [ makeColor 0.95 0.35 0.75 1
        , makeColor 0.35 0.85 0.85 1
        , makeColor 0.95 0.75 0.25 1
        , makeColor 0.55 0.85 0.4 1
        , makeColor 0.7 0.6 0.95 1
        ]

-- | The two that are.
edgeColour = greyN 0.45

inkColour = greyN 0.9

faintColour = greyN 0.55

-- * Drawing one cell

tile :: Float
tile = 34

-- | One cell and whatever is standing on it.
cellPicture :: KnownStrip w h => Frame -> Game w h -> Spot w h -> Picture
cellPicture frame game s =
    pictures $
    [color (base here) (rectangleSolid tile tile)] ++
    concat
        [ [ color (withAlpha 0.4 goalColour) (rectangleSolid (tile - 12) (tile - 12))
          , color goalColour (rectangleWire (tile - 9) (tile - 9))
          ]
        | here == Goal
        ] ++
    [ color
        (if onGoal
             then doneColour
             else crateColour)
        (rectangleSolid (tile - 9) (tile - 9))
    | hasCrate
    ] ++
    [color playerColour (circleSolid (tile / 2 - 5)) | isPlayer] ++
    [ color background (facingWedge (dirOf frame (playFlipped now) (playFacing now)))
    | isPlayer
    ]
  where
    now = gamePlay game
    here = tileAt game s
    onGoal = Set.member (spotCoord s) (levelGoals (gameLevel game))
    hasCrate = crateAt now s
    isPlayer = spotCoord s == spotCoord (playPlayer now)
    base Wall = wallColour
    base _    = floorColour

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
            DirUp    -> 0
            DirRight -> 90
            DirDown  -> 180
            DirLeft  -> 270

-- | Dim a finished picture by laying the background over it. gloss has no
-- opacity for a 'Picture', and the cells inside set their own colours, so a
-- 'color' wrapper would not reach them.
dimmed :: Float -> Picture -> Picture
dimmed amount p =
    pictures [p, color (withAlpha amount background) (rectangleSolid tile tile)]

-- * The flat view

drawFlat :: forall w h. KnownStrip w h => Frame -> Game w h -> Picture
drawFlat frame game =
    pictures $
    concat [ghostCells, boardCells, seamTags, [strait halfH, strait (-halfH)]]
  where
    (around, across) = stripSize @w @h
    halfW = fromIntegral around * tile / 2
    halfH = fromIntegral across * tile / 2
    ghostCols = min 3 around
    cell x y = fromMaybe blank (cellPicture frame game <$> spotAt @w @h x y)
    -- Column @x@ of the picture, which may be outside the board: that is what
    -- a ghost column is.
    place x y =
        translate
            (tile * (fromIntegral x - fromIntegral (around - 1) / 2))
            (tile * (fromIntegral y - fromIntegral (across - 1) / 2))
    boardCells = [place x y (cell x y) | x <- [0 .. around - 1], y <- [0 .. across - 1]]
    -- Leaving the board sideways puts you in the row the mirror sends this one
    -- to, so past the right edge is the /start/ of that row and past the left
    -- edge is the /end/ of it.
    mirrorRow y = across - 1 - y
    ghostCells =
        [ place column y (dimmed 0.62 (cell source (mirrorRow y)))
        | y <- [0 .. across - 1]
        , (column, source) <-
              [(around + k, k) | k <- [0 .. ghostCols - 1]] ++
              [(-k, around - k) | k <- [1 .. ghostCols]]
        ]
    -- The seam, drawn one row at a time and coloured by which row it joins
    -- to. Crossing the right edge of row y puts you at the left edge of row
    -- @across - 1 - y@, so those two get the same colour --- and the player
    -- reads the pairing off the picture instead of being told it.
    seamTags =
        [ translate x (tile * (fromIntegral y - fromIntegral (across - 1) / 2)) $
        color (pairColour across y) (rectangleSolid 5 (tile - 3))
        | y <- [0 .. across - 1]
        , x <- [-halfW - 2, halfW + 2]
        ]
    strait y = color edgeColour (line [(-halfW - 12, y), (halfW + 12, y)])

-- * The player-centred view

-- | How far around the player the surface is drawn.
radius :: Int
radius = 5

drawCentred :: forall w h. KnownStrip w h => Frame -> Game w h -> Picture
drawCentred frame game = pictures (cells ++ [horizon])
  where
    now = gamePlay game
    origin = (playPlayer now, playFlipped now)
    cells =
        [ translate (tile * fromIntegral dx) (tile * fromIntegral dy) $
        case cellAround origin (dx, dy) of
            Nothing -> color voidColour (rectangleSolid (tile - 2) (tile - 2))
            Just (s, _) -> cellPicture frame game s
        | dx <- [-radius .. radius]
        , dy <- [-radius .. radius]
        ]
    -- A ring at the edge of what is drawn, so the view reads as a window on a
    -- surface rather than as a board with a border.
    horizon =
        color (withAlpha 0.5 seamColour) $
        rectangleWire (tile * fromIntegral (2 * radius + 1)) (tile * fromIntegral (2 * radius + 1))

-- * The window

drawApp :: App -> Picture
drawApp app = fitTo (appWin app) (pictures [hud app, board])
  where
    board =
        case appGame app of
            Session g ->
                case appView app of
                    Flat    -> fitBoard (flatSize g) (drawFlat (appFrame app) g)
                    Centred -> fitBoard centredSize (drawCentred (appFrame app) g)

-- | The area the board is drawn into, between the readout above and the keys
-- below.
boardBox :: (Float, Float)
boardBox = (860, 470)

-- | Scale a board picture to fill 'boardBox', within reason. A one-row strip
-- would otherwise be drawn at the height of the window, and a wide one would
-- run off the sides.
fitBoard :: (Float, Float) -> Picture -> Picture
fitBoard (w, h) = translate 0 (-20) . scale k k
  where
    (boxW, boxH) = boardBox
    k = min 2.2 (min (boxW / w) (boxH / h))

flatSize :: forall w h. KnownStrip w h => Game w h -> (Float, Float)
flatSize _ = (fromIntegral (around + 2 * min 3 around) * tile, fromIntegral across * tile)
  where
    (around, across) = stripSize @w @h

centredSize :: (Float, Float)
centredSize = (side, side)
  where
    side = tile * fromIntegral (2 * radius + 1)

hud :: App -> Picture
hud app =
    pictures (zipWith bodyLine [0 ..] body ++ zipWith keyLine [0 ..] (note ++ legend))
  where
    bodyLine :: Int -> String -> Picture
    bodyLine i s =
        translate (-430) (330 - 24 * fromIntegral i) $
        scale 0.12 0.12 $ color inkColour $ text s
    keyLine :: Int -> String -> Picture
    keyLine i s =
        translate (-430) (-292 - 20 * fromIntegral i) $
        scale 0.1 0.1 $ color faintColour $ text s
    note =
        [ case appView app of
              Flat ->
                  "the coloured tabs pair the edges: leave one row and you arrive at the tab of the same colour"
              Centred ->
                  "drawn in the player's frame, so crossing the seam looks like nothing at all"
        ]
    legend =
        [ "arrows or wasd / hjkl move    u undo    r restart    n / p level    v view    f frame"
        ]
    body =
        case appGame app of
            Session g ->
                let now = gamePlay g
                    lvl = gameLevel g
                in [ show (appIndex app + 1) ++ ". " ++ levelName lvl ++
                     (if solved g
                          then "        SOLVED"
                          else "")
                   , "goals left " ++ show (goalsLeft g) ++ "     moves " ++
                     show (playMoves now) ++ "     pushes " ++
                     show (playPushes now) ++ "     last: " ++
                     outcomeName (appOutcome app)
                   , "view: " ++ viewName (appView app) ++ "     frame: " ++
                     frameLabel (appFrame app) ++
                     (if playFlipped now
                          then " (the player is upside down)"
                          else "")
                   ]
    frameLabel ChartFrame  = "chart"
    frameLabel PlayerFrame = "player"

-- | The coordinate space the layout above is written in. The picture is
-- scaled to whatever the window turns out to be.
designW, designH :: Float
designW = 900

designH = 740

fitTo :: (Int, Int) -> Picture -> Picture
fitTo (w, h) = scale k k
  where
    k = min (fromIntegral w / designW) (fromIntegral h / designH)
