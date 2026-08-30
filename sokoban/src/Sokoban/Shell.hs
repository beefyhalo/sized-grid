{-# LANGUAGE DataKinds #-}

-- | The program the board lives inside: what is on screen, what a key does,
-- and the way out.
--
-- Until sized-grid-lopy.8 there was no such thing. The window opened on level
-- one, solving a level changed one word in a readout, the next level arrived
-- only if the player happened to press @n@, and the only way to stop was to
-- close the window. Everything a person who does not write Haskell meets first
-- was missing, which is what this module is.
--
-- Four things are on screen, and 'Stage' says which:
--
--   * 'Title' is where a Mobius strip gets explained, because by the time a
--     level is on screen it is too late to explain it. The band turns behind
--     the sentence that does it (sized-grid-lopy.6).
--
--   * 'Levels' is every level at once, ticked where it is finished. It is the
--     progress screen and the level select and the pause screen, which are the
--     same screen: eight levels are few enough to draw all of.
--
--   * 'Playing' is the game.
--
--   * 'Cleared' is the moment after a level is finished, and it is not a
--     dialog box. The marker takes a lap of the strip --- twice around, which
--     is what it takes to come home --- through the seam that the level was
--     about. The board says why the level was a level, using the board.
--
-- == Leaving
--
-- gloss's 'Graphics.Gloss.Interface.Pure.Game.play' has no way out: its event
-- handler is pure, so it cannot exit, and the window's close button is the
-- only door. So the window runs on 'playIO' instead, and 'onEvent' stays pure
-- and total --- it sets 'appQuit' and the IO wrapper is the only thing that
-- acts on it. That keeps every key testable, and it is what lets
-- @sokoban-shot@ drive the whole game through the real key handler.
module Sokoban.Shell
  ( -- * What is on screen
    Stage (..),
    App (..),
    Session (..),
    newApp,
    defaultWindow,

    -- * Running it
    drawApp,
    onEvent,
    step,
    run,

    -- * The lap a finished level takes
    lapMark,
    lapPace,
  )
where

import Data.Char (isDigit, isLower, isUpper)
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Graphics.Gloss.Interface.IO.Game
import Sokoban.Band (Band, bandSpin, defaultBand)
import Sokoban.Board
import Sokoban.Level
import Sokoban.Render
import Sokoban.Rules
import System.Exit (exitSuccess)

-- | Which of the four screens is up.
data Stage
  = Title
  | Levels
  | Playing
  | -- | Seconds since the level was finished, which is what the lap is drawn
    -- from. Not a countdown: nothing happens when it runs out except that the
    -- marker stops, because a player who wants to look at a finished board
    -- should be allowed to.
    Cleared !Float
  deriving (Eq, Show)

-- | A game whose board size is known only at run time. gloss's state is one
-- type and a list of levels is a list of sizes, so the size is packed away
-- here and unpacked by whatever needs it.
data Session where
  Session :: (KnownStrip w h) => Game w h -> Session

data App = App
  { appLevels :: [SomeLevel],
    appIndex :: !Int,
    appGame :: Session,
    appStage :: !Stage,
    -- | Which levels have been finished, by index.
    --
    -- Beside the games rather than in one, because a 'Session' is rebuilt from
    -- its level every time the player moves to it and would forget. This is
    -- the only thing in the app that outlives a level.
    appFinished :: !(Set Int),
    -- | Where the cursor is on the level screen, which is not where the player
    -- is. Choosing a level throws the current game away, so hovering over one
    -- must not.
    appPick :: !Int,
    appView :: View,
    appFrame :: Frame,
    -- | How the band is drawn and how far round it has turned, which is the
    -- only thing in this game that moves without a key being pressed.
    --
    -- State rather than a function of a clock, so that a picture of the game
    -- is a function of the game: that is what lets @sokoban-shot@ photograph
    -- the band from a chosen angle.
    appBand :: !Band,
    -- | The window's real size, from 'EventResize'.
    appWin :: (Int, Int),
    appOutcome :: Outcome,
    -- | Set by a key, acted on by 'run' and nobody else. See the module
    -- header.
    appQuit :: !Bool
  }

newApp :: (Int, Int) -> [SomeLevel] -> App
newApp win levels =
  App
    { appLevels = levels,
      appIndex = 0,
      appGame = sessionAt levels 0,
      appStage = Title,
      appFinished = Set.empty,
      appPick = 0,
      appView = Flat,
      appFrame = ChartFrame,
      appBand = defaultBand,
      appWin = win,
      appOutcome = Walked,
      appQuit = False
    }

sessionAt :: [SomeLevel] -> Int -> Session
sessionAt levels i =
  case drop (i `mod` max 1 (length levels)) levels of
    (SomeLevel l : _) -> Session (newGame l)
    [] -> error "sessionAt: no levels"

levelCount :: App -> Int
levelCount = length . appLevels

defaultWindow :: (Int, Int)
defaultWindow = (900, 700)

-- | Open the window. Blocks until it is closed or quit out of.
run :: [SomeLevel] -> IO ()
run [] = error "run: no levels"
run levels =
  playIO
    (InWindow "Sokoban on a Mobius strip" defaultWindow (40, 40))
    background
    30
    (newApp defaultWindow levels)
    (pure . drawApp)
    (\e w -> leave (onEvent e w))
    (\dt w -> pure (step dt w))
  where
    leave w
      | appQuit w = exitSuccess
      | otherwise = pure w

-- | Turn the band, and age the lap a finished level is taking.
--
-- A revolution every half minute --- slow on purpose, since the point of the
-- band is that the surface comes back to itself the other way up, and a player
-- who has to chase a cell round has not seen it happen.
step :: Float -> App -> App
step dt app =
  app
    { appBand = band {bandSpin = bandSpin band + dt / 30},
      appStage = age (appStage app)
    }
  where
    band = appBand app
    age (Cleared t) = Cleared (t + dt)
    age other = other

-- * Input

onEvent :: Event -> App -> App
onEvent (EventResize wh) app = app {appWin = wh}
onEvent (EventKey k Down _ _) app = onKey k app
onEvent _ app = app

-- | One key, in whichever screen is up.
--
-- @q@ leaves from anywhere, and so does the close button. Escape is the way
-- back one screen, which from a level is the level screen and from there is
-- the title.
onKey :: Key -> App -> App
onKey k app
  | k == Char 'q' = app {appQuit = True}
  | otherwise =
      case appStage app of
        Title -> onTitleKey k app
        Levels -> onLevelsKey k app
        Playing -> onPlayKey k app
        Cleared _ -> onClearedKey k app

onTitleKey :: Key -> App -> App
onTitleKey k app
  | isGo k = app {appStage = Playing}
  | k == Char 'l' = toLevels app
  | k == SpecialKey KeyEsc = app {appQuit = True}
  | otherwise = app

onLevelsKey :: Key -> App -> App
onLevelsKey k app
  | isGo k = (goTo (appPick app) app) {appStage = Playing}
  | k == SpecialKey KeyEsc = app {appStage = Title}
  | otherwise =
      case dirKey k of
        Just DirLeft -> movePick (-1)
        Just DirRight -> movePick 1
        Just DirUp -> movePick (-columns (levelCount app))
        Just DirDown -> movePick (columns (levelCount app))
        Nothing -> app
  where
    n = max 1 (levelCount app)
    movePick d = app {appPick = (appPick app + d + n) `mod` n}

onPlayKey :: Key -> App -> App
onPlayKey k app
  | k == SpecialKey KeyEsc = toLevels app
  | Just dir <- dirKey k = press dir app
  | otherwise =
      case k of
        Char 'u' -> onSession (\g -> fromMaybe g (undo g)) app
        Char 'r' -> onSession restart app
        Char 'n' -> (goTo (appIndex app + 1) app) {appStage = Playing}
        Char 'p' -> (goTo (appIndex app - 1) app) {appStage = Playing}
        _ -> onLookKey k app

-- | The keys that still work once a level is over.
--
-- Undo among them, and deliberately: taking back the winning move puts the
-- player back in the level, which is what somebody who wanted a shorter
-- solution is reaching for.
onClearedKey :: Key -> App -> App
onClearedKey k app
  | isGo k = advance app
  | k == SpecialKey KeyEsc = toLevels app
  | otherwise =
      case k of
        Char 'u' -> (onSession (\g -> fromMaybe g (undo g)) app) {appStage = Playing}
        Char 'r' -> (onSession restart app) {appStage = Playing}
        _ -> onLookKey k app

-- | The two keys that only change how the game is being looked at, and so mean
-- the same thing on every screen that has a board on it.
onLookKey :: Key -> App -> App
onLookKey (Char 'v') app = app {appView = nextView (appView app)}
onLookKey (Char 'f') app = app {appFrame = flipFrame (appFrame app)}
onLookKey _ app = app

nextView :: View -> View
nextView v
  | v == maxBound = minBound
  | otherwise = succ v

flipFrame :: Frame -> Frame
flipFrame ChartFrame = PlayerFrame
flipFrame PlayerFrame = ChartFrame

-- | Whichever key means yes. Both, because which one gloss reports a space as
-- depends on the backend.
isGo :: Key -> Bool
isGo k = k `elem` [SpecialKey KeySpace, SpecialKey KeyEnter, Char ' ']

dirKey :: Key -> Maybe Dir
dirKey (SpecialKey KeyLeft) = Just DirLeft
dirKey (SpecialKey KeyRight) = Just DirRight
dirKey (SpecialKey KeyUp) = Just DirUp
dirKey (SpecialKey KeyDown) = Just DirDown
dirKey (Char 'a') = Just DirLeft
dirKey (Char 'h') = Just DirLeft
dirKey (Char 'd') = Just DirRight
dirKey (Char 'l') = Just DirRight
dirKey (Char 'w') = Just DirUp
dirKey (Char 'k') = Just DirUp
dirKey (Char 's') = Just DirDown
dirKey (Char 'j') = Just DirDown
dirKey _ = Nothing

toLevels :: App -> App
toLevels app = app {appStage = Levels, appPick = appIndex app}

-- | On to the next level, or to the level screen if that was the last one.
--
-- The level screen is where the game ends rather than a finale of its own,
-- because with every level ticked it already is one --- and it is the only
-- screen from which there is something left to do.
advance :: App -> App
advance app
  | appIndex app + 1 < levelCount app =
      (goTo (appIndex app + 1) app) {appStage = Playing}
  | otherwise = toLevels app

goTo :: Int -> App -> App
goTo i app =
  app
    { appIndex = wrapped,
      appPick = wrapped,
      appGame = sessionAt (appLevels app) wrapped,
      appOutcome = Walked
    }
  where
    n = max 1 (levelCount app)
    wrapped = (i + n) `mod` n

onSession :: (forall w h. (KnownStrip w h) => Game w h -> Game w h) -> App -> App
onSession f app =
  case appGame app of
    Session g -> app {appGame = Session (f g)}

-- | A direction key, and the one place a level is noticed to be over.
press :: Dir -> App -> App
press dir app =
  case appGame app of
    Session g ->
      let (g', o) = move (appFrame app) dir g
          moved = app {appGame = Session g', appOutcome = o}
       in if solved g'
            then
              moved
                { appStage = Cleared 0,
                  appFinished = Set.insert (appIndex app) (appFinished app)
                }
            else moved

-- * Drawing

drawApp :: App -> Picture
drawApp app =
  fitTo (appWin app) $
    case appStage app of
      Title -> drawTitle app
      Levels -> drawLevels app
      Playing -> drawBoardScreen app Nothing
      Cleared t -> drawBoardScreen app (Just t)

-- | The coordinate space every layout here is written in. The picture is
-- scaled to whatever the window turns out to be.
designW, designH :: Float
designW = 900
designH = 740

fitTo :: (Int, Int) -> Picture -> Picture
fitTo (w, h) = scale k k
  where
    k = min (fromIntegral w / designW) (fromIntegral h / designH)

-- | How wide a string comes out, at scale one.
--
-- gloss draws text with a proportional stroke font and offers no way to ask
-- how wide the result was, so a line cannot be centred without a guess. These
-- are that guess, and they are measured rather than derived: a title and a
-- paragraph were photographed, their pixel widths divided by their letters,
-- and the four classes solved for. Good to a few per cent, which is a couple
-- of pixels on a centred line and nothing at all on a fitted one.
wide :: Float -> String -> Float
wide s = (* s) . sum . map glyph
  where
    glyph c
      | isUpper c = 85
      | isDigit c = 70
      | isLower c = 71
      | c == ' ' = 45
      | otherwise = 50

-- | A line of text with its left edge at @x@.
say :: Float -> Float -> Float -> Color -> String -> Picture
say x y s c str = translate x y (scale s s (color c (text str)))

-- | A line of text centred on the window.
mid :: Float -> Float -> Color -> String -> Picture
mid y s c str = say (-(wide s str / 2)) y s c str

-- | A line of text that has to fit a width, however long it turns out to be.
-- Level names are prose and cards are 200 pixels wide.
fitSay :: Float -> Float -> Float -> Float -> Color -> String -> Picture
fitSay x y room s c str = say x y (min s (room / max 1 (wide 1 str))) c str

-- * The title screen

-- | The way in, and the only place the surface gets explained: by the time a
-- level is on screen it is too late.
--
-- What it says is the surface's own sentence rather than one written here, so
-- that a level pack on a Klein bottle opens by describing a Klein bottle
-- (sized-grid-lopy.7). The picture behind it is the level about to be played,
-- as a band where the surface is one and flat where it is not.
drawTitle :: App -> Picture
drawTitle app =
  pictures $
    [ mid 296 0.26 inkColour "SOKOBAN ON A MOBIUS STRIP",
      picture
    ]
      ++ zipWith blurbLine [0 :: Int ..] blurb
      ++ [mid (-296) 0.11 faintColour "space  play      l  the levels      q  quit"]
  where
    -- Set as a block and not line by line: separately centred lines are ragged
    -- down both sides, which reads as a mistake rather than as a paragraph.
    titleShrink = 300 / snd boardBox
    blurbLeft = -(maximum (map (wide 0.105) blurb) / 2)
    blurbLine i = say blurbLeft (-150 - 26 * fromIntegral i) 0.105 inkColour
    picture =
      case appGame app of
        Session g
          | surfaceIsBand (gameSurface g) ->
              translate 0 70 (bandInto (740, 300) (appBand app) g)
          | otherwise ->
              -- The flat view fits itself to the whole board box, which on the
              -- title screen runs into the title. Scaling the finished picture
              -- down to the band's box is enough, since nothing in it is a
              -- fixed number of pixels.
              translate 0 70 $
                scale titleShrink titleShrink $
                  drawView Flat (appBand app) ChartFrame Nothing g
    blurb =
      case appGame app of
        Session g -> wrapTo 68 (surfaceNote (gameSurface g))

-- | Break a paragraph into lines of at most @room@ characters, greedily.
--
-- A surface describes itself in one string, because a surface is a fact and
-- not a layout, and this is where that becomes four lines of a title screen.
wrapTo :: Int -> String -> [String]
wrapTo room = foldl place [] . words
  where
    place [] w = [w]
    place ls w
      | length (last ls) + 1 + length w <= room = init ls ++ [last ls ++ " " ++ w]
      | otherwise = ls ++ [w]

-- * The level screen

-- | How many levels fit across the screen. Four, and the rows follow from it.
columns :: Int -> Int
columns n = min 4 (max 1 n)

drawLevels :: App -> Picture
drawLevels app =
  pictures $
    [mid 300 0.16 inkColour heading]
      ++ map card [0 .. levelCount app - 1]
      ++ [ mid
             (-320)
             0.11
             faintColour
             "arrows  choose      space  play it      esc  the title      q  quit"
         ]
  where
    n = levelCount app
    done = Set.size (appFinished app)
    heading
      | done == n = "All " ++ show n ++ " finished."
      | otherwise = show done ++ " of " ++ show n ++ " finished"
    cols = columns n
    rows = (n + cols - 1) `div` cols
    boxW = 860 / fromIntegral cols
    boxH = 460 / fromIntegral rows
    card i =
      translate x y $
        pictures $
          [ color (frame' i) (rectangleWire (boxW - 12) (boxH - 12)),
            translate 0 12 picture,
            fitSay
              (-(boxW / 2) + 12)
              (boxH / 2 - 22)
              (boxW - 50)
              0.075
              faintColour
              surface,
            fitSay
              (-(boxW / 2) + 12)
              (-(boxH / 2) + 16)
              (boxW - 24)
              0.09
              (label i)
              (show (i + 1) ++ ". " ++ name)
          ]
            ++ [ translate (boxW / 2 - 20) (boxH / 2 - 20) (color doneColour (circleSolid 6))
               | Set.member i (appFinished app)
               ]
      where
        col = i `mod` cols
        row = i `div` cols
        x = -430 + boxW * (fromIntegral col + 0.5)
        y = 230 - boxH * (fromIntegral row + 0.5)
        -- Both come out of the same unpacking, since the size a level is drawn
        -- at cannot leave the case that knows it.
        (picture, name, surface) =
          case sessionAt (appLevels app) i of
            Session s ->
              ( thumbnail (boxW - 44, boxH - 74) s,
                levelName (gameLevel s),
                surfaceName (gameSurface s)
              )
    frame' i
      | i == appPick app = seamColour
      | Set.member i (appFinished app) = doneColour
      | otherwise = faintColour
    label i
      | i == appPick app = inkColour
      | otherwise = faintColour

-- * A level, playing or just finished

drawBoardScreen :: App -> Maybe Float -> Picture
drawBoardScreen app cleared =
  case appGame app of
    Session g ->
      pictures
        [ readout app g cleared,
          drawView (appView app) (appBand app) (appFrame app) (cleared >>= flip lapMark g) g,
          footer app cleared
        ]

-- | Where the lap a finished level takes has got to, or 'Nothing' once it is
-- home.
--
-- Twice around, because that is what it takes: one lap of a Mobius strip lands
-- in the row on the far side of the middle and only the second closes. It is
-- walked with the game's own step, so what the player watches is the surface
-- and not an animation of it.
lapMark :: forall w h. (KnownStrip w h) => Float -> Game w h -> Maybe (Spot w h)
lapMark t g
  | walked > whole = Nothing
  | otherwise =
      fst
        <$> walkFrom
          (gameSurface g)
          ChartFrame
          (playPlayer now, playTurn now)
          (replicate walked DirRight)
  where
    now = gamePlay g
    (around, _) = stripSize @w @h
    whole = 2 * around
    walked = max 0 (floor (t / lapPace))

-- | Seconds the lap spends on each cell.
lapPace :: Float
lapPace = 0.13

readout :: App -> Game w h -> Maybe Float -> Picture
readout app g cleared = pictures (zipWith bodyLine [0 ..] body)
  where
    bodyLine :: Int -> (Color, String) -> Picture
    bodyLine i (c, s) = say (-430) (330 - 24 * fromIntegral i) 0.12 c s
    now = gamePlay g
    lvl = gameLevel g
    solvedNow = isJust cleared
    body =
      [ ( if solvedNow then doneColour else inkColour,
          show (appIndex app + 1)
            ++ ". "
            ++ levelName lvl
            ++ (if solvedNow then "        SOLVED" else "")
        ),
        ( inkColour,
          "goals left "
            ++ show (goalsLeft g)
            ++ "     moves "
            ++ show (playMoves now)
            ++ "     pushes "
            ++ show (playPushes now)
            ++ "     last: "
            ++ outcomeName (appOutcome app)
        ),
        ( faintColour,
          "view: "
            ++ viewName (appView app)
            ++ "     frame: "
            ++ frameLabel (appFrame app)
            ++ ( case turnNote (playTurn now) of
                   "" -> ""
                   note -> " (the player is " ++ note ++ ")"
               )
        )
      ]
    frameLabel ChartFrame = "chart"
    frameLabel PlayerFrame = "player"

-- | The surface the current level is on, which the footer needs and cannot
-- get at through the existential.
appSurface :: App -> Surface
appSurface app =
  case appGame app of
    Session g -> gameSurface g

footer :: App -> Maybe Float -> Picture
footer app cleared =
  pictures
    [ say (-430) (-292) 0.1 faintColour (viewNote (appSurface app) (appView app)),
      say (-430) (-312) 0.1 faintColour keys
    ]
  where
    keys =
      case cleared of
        Just _
          | appIndex app + 1 < levelCount app ->
              "space  the next level    u  take it back    r  again    esc  the levels    q  quit"
          | otherwise ->
              "space  the level screen    u  take it back    r  again    esc  the levels    q  quit"
        Nothing ->
          "arrows or wasd / hjkl move    u undo    r restart    n / p level    v view    f frame    esc levels    q quit"
