-- 'axisSize' names its axis only under a type family, so every call has to say
-- which axis it means with a visible type application; the signature cannot be
-- inferred from the arguments because it has none.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Lens
import Data.Grid.Sized
import Data.Grid.Sized.Unboxed
import Data.Maybe (mapMaybe)
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word8)
import GHC.TypeLits
import Graphics.Gloss.Interface.Pure.Game
import System.Random
  ( StdGen,
    newStdGen,
    randoms,
    split,
  )

data TileState
  = Alive
  | Dead
  deriving (Eq, Show)

flipTileState :: TileState -> TileState
flipTileState Alive = Dead
flipTileState Dead = Alive

-- | Unboxed as a single byte, so a 'UGrid' can hold a board of these without
-- a pointer per tile.
instance VU.IsoUnbox TileState Word8 where
  toURepr Dead = 0
  toURepr Alive = 1
  fromURepr 0 = Dead
  fromURepr _ = Alive
  {-# INLINE toURepr #-}
  {-# INLINE fromURepr #-}

newtype instance VU.MVector s TileState = MV_TileState (VU.MVector s Word8)

newtype instance VU.Vector TileState = V_TileState (VU.Vector Word8)

deriving via (TileState `VU.As` Word8) instance VGM.MVector VU.MVector TileState

deriving via (TileState `VU.As` Word8) instance VG.Vector VU.Vector TileState

instance VU.Unbox TileState

-- * Rules

newtype Rule (n :: Nat) = Rule
  { runRule :: TileState -> [TileState] -> TileState
  }

-- | A two-state totalistic rule, given as its birth and survival counts.
--
-- Every rule this demo offers is one of these, which is the point of leaving
-- 'Rule' a function rather than narrowing it to a pair of lists: /this/ is the
-- totalistic family, and 'Rule' is wider than it. A rule that read where its
-- live neighbours were rather than only how many there were would still be a
-- 'Rule', and would still run through 'applyRule' unchanged.
totalistic :: [Int] -> [Int] -> Rule n
totalistic born survives = Rule $ \here neigh ->
  let aliveNeigh = length $ filter (== Alive) neigh
   in if
        | here == Alive && aliveNeigh `elem` survives -> Alive
        | here == Dead && aliveNeigh `elem` born -> Alive
        | otherwise -> Dead

-- | Conway's rule, named separately from 'rules' because 'main' starts on it:
-- @head rules@ would say the same thing and would be partial.
conwayRule :: (String, Rule n)
conwayRule = ("Life B3/S23", totalistic [3] [2, 3])

-- | The rules @n@ cycles through. Conway's is first, so the demo still opens
-- on Game of Life.
--
-- @n@ is phantom in 'Rule', so this list is polymorphic in the dimension: the
-- neighbour count is whatever the 'Stencil' handed it, and a rule quoted for
-- eight neighbours simply behaves differently when given four. That is worth
-- watching rather than worth preventing --- see 'neighbourhoods'.
rules :: [(String, Rule n)]
rules =
  conwayRule
    : [ ("HighLife B36/S23", totalistic [3, 6] [2, 3]),
        ("Seeds B2/S", totalistic [2] []),
        ("Day&Night B3678/S34678", totalistic [3, 6, 7, 8] [3, 4, 6, 7, 8])
      ]

-- | The game-of-life neighbourhood, named separately for the same reason
-- 'conwayRule' is.
mooreR1 :: (IsCoordList cs) => (String, Stencil cs)
mooreR1 = ("Moore r=1 (8)", mooreStencil 1)

-- | The neighbourhoods @v@ cycles through.
--
-- Constrained rather than a CAF, so each entry is rebuilt when it is selected.
-- That is deliberate: a 'Stencil' is a table sized by the whole grid, and
-- holding four of them alive for the lifetime of the window to save a
-- keypress's worth of work is the wrong trade. The one in play is kept in
-- 'WorldState'; the rest are rebuilt on demand.
neighbourhoods :: (IsCoordList cs) => [(String, Stencil cs)]
neighbourhoods =
  mooreR1
    : [ ("von Neumann r=1 (4)", vonNeumannStencil 1),
        ("Moore r=2 (24)", mooreStencil 2),
        ("von Neumann r=2 (8)", vonNeumannStencil 2)
      ]

-- | One tick, as a bulk pass over the unboxed grid: for every coordinate,
-- read its neighbours back out of the old grid and decide the new tile.
-- The neighbourhood comes in as a precomputed 'Stencil' rather than being
-- enumerated per cell, since the same positions would otherwise be rebuilt
-- sixty times a second for no reason.
--
-- Both arguments are values, which is what makes the rule and the
-- neighbourhood runtime choices: swapping either changes what the board does
-- with no other change anywhere, this function included.
applyRule ::
  Rule n ->
  Stencil cs ->
  UGrid cs TileState ->
  UGrid cs TileState
applyRule rule s = stencilGrid s (runRule rule)

-- * Starting boards

-- | A named starting pattern, written as a picture of itself: @\'O\'@ is a
-- live cell and every other character is dead, with row 0 the /top/ row, so
-- the literal below reads the way the board draws.
data Preset = Preset
  { presetName :: String,
    presetRows :: [String],
    presetAnchor :: Anchor
  }

-- | Where a preset's bottom-left corner goes.
data Anchor
  = Centred
  | -- | Inset from the top-right corner, so a pattern that travels up and to
    -- the right reaches the edge within seconds. Only useful on a wrapping
    -- axis, which is the point.
    NearTopRight Int

-- | The board the demo opens on, named separately for the same reason
-- 'conwayRule' is.
--
-- This orientation travels @(+1, +1)@ every four generations, so starting it a
-- few cells from the far corner puts it across the seam almost immediately: on
-- a 'Periodic' board it reappears at the opposite edge, and nothing in
-- 'applyRule' or in the rule checks a bound to make that happen. The wrap is in
-- the coordinate type.
gliderPreset :: Preset
gliderPreset =
  Preset
    "Glider"
    [ ".OO",
      "O.O",
      "..O"
    ]
    (NearTopRight 6)

-- | The presets @p@ cycles through.
presets :: [Preset]
presets =
  gliderPreset
    : [ Preset
          "Gosper glider gun"
          [ "........................O...........",
            "......................O.O...........",
            "............OO......OO............OO",
            "...........O...O....OO............OO",
            "OO........O.....O...OO..............",
            "OO........O...O.OO....O.O...........",
            "..........O.....O.......O...........",
            "...........O...O....................",
            "............OO......................"
          ]
          Centred,
        Preset
          "R-pentomino"
          [ ".OO",
            "OO.",
            ".O."
          ]
          Centred,
        Preset
          "Acorn"
          [ ".O.....",
            "...O...",
            "OO..OOO"
          ]
          Centred
      ]

-- | The size of one axis, as a plain 'Int'.
axisSize :: forall x. (IsCoordLifted x) => Int
axisSize = ordinalSize @(CoordNat x)

-- | The live cells of a preset, as coordinates on the board it is laid onto.
--
-- The rows are read bottom-up, since 'presetRows' puts row 0 at the top and
-- the second axis is drawn upwards, and the result is taken modulo the axis
-- sizes so a pattern larger than the board wraps rather than being silently
-- truncated.
presetCoords ::
  forall x y.
  (IsCoordLifted x, IsCoordLifted y) =>
  Preset ->
  [Coord '[x, y]]
presetCoords Preset {..} =
  mapMaybe
    (\(cx, cy) -> coordAt (baseX + cx) (baseY + cy))
    [ (cx, cy)
    | (cy, row) <- zip [0 ..] (reverse presetRows),
      (cx, ch) <- zip [0 ..] row,
      ch == 'O'
    ]
  where
    height = length presetRows
    width = maximum (0 : map length presetRows)
    (baseX, baseY) =
      case presetAnchor of
        Centred ->
          ((axisSize @x - width) `div` 2, (axisSize @y - height) `div` 2)
        NearTopRight gap ->
          (axisSize @x - width - gap, axisSize @y - height - gap)

-- | A coordinate from a pair of axis indices, wrapped into range.
coordAt ::
  forall x y.
  (IsCoordLifted x, IsCoordLifted y) =>
  Int ->
  Int ->
  Maybe (Coord '[x, y])
coordAt cx cy =
  (\a b -> review asOrdinal a :| review asOrdinal b :| EmptyCoord)
    <$> numToOrdinal (cx `mod` axisSize @x)
    <*> numToOrdinal (cy `mod` axisSize @y)

emptyBoard :: (IsCoordList cs) => UGrid cs TileState
emptyBoard = tabulateGrid (const Dead)

presetBoard ::
  forall x y.
  (IsCoordLifted x, IsCoordLifted y) =>
  Preset ->
  UGrid '[x, y] TileState
presetBoard p =
  let live = map coordPosition (presetCoords @x @y p)
   in tabulateGrid (\c -> if coordPosition c `elem` live then Alive else Dead)

randomBoard ::
  forall cs.
  (IsCoordList cs) =>
  StdGen ->
  (UGrid cs TileState, StdGen)
randomBoard g0 =
  let (gA, gB) = split g0
      v :: VU.Vector TileState
      v =
        VU.fromListN
          (coordSpaceSize @cs)
          [if b then Alive else Dead | b <- randoms gA]
   in (tabulateGrid (\c -> v VU.! coordPosition c), gB)

-- * The world

data DisplayInfo = DisplayInfo
  { tileSize :: Float,
    -- | Screen position of the centre of the cell at 'zeroCoord'.
    originX :: Float,
    originY :: Float,
    hudLeft :: Float,
    -- | Where the first HUD line sits. Derived from the window rather than
    -- written down, because the window is derived from the screen.
    hudTop :: Float,
    -- | What the board's axis types are, for the HUD. A string rather than
    -- something derived, because the interesting half of it --- @Periodic@ ---
    -- is a type name and not a number.
    topologyName :: String
  }

-- | Height reserved above the board for the HUD: five lines at 26 units and
-- two at 20, plus margins.
hudHeight :: Float
hudHeight = 180

-- | Lay the board out inside a window of the given size.
--
-- Nothing here is a constant chosen to look right at one window size, because
-- there is no one window size: 'main' takes the window from the screen. The
-- tile is whatever makes 60 of them fit in the smaller of the two directions
-- once the HUD has taken its strip off the top, and the board is then centred
-- horizontally and sat on the bottom margin.
displayFor :: Int -> Int -> String -> DisplayInfo
displayFor w h name =
  DisplayInfo
    { tileSize = ts,
      originX = -((59 * ts) / 2),
      originY = -(fh / 2) + pad + ts / 2,
      hudLeft = -(fw / 2) + 20,
      hudTop = fh / 2 - 30,
      topologyName = name
    }
  where
    fw = fromIntegral w
    fh = fromIntegral h
    pad = 12
    ts = min ((fw - 2 * pad) / 60) ((fh - hudHeight - 2 * pad) / 60)

data WorldState cs = WorldState
  { _grid :: UGrid cs TileState,
    _timeElapsedSinceLastTick :: Float,
    _tickInterval :: Float,
    _generation :: Int,
    _rule :: Rule (Length cs),
    _ruleIx :: Int,
    -- | The board's neighbourhood, built when it is selected and held until
    -- the selection changes. It is a field rather than something 'tickWorld'
    -- computes because that is the whole trade a 'Stencil' offers: building one
    -- costs a tick's worth of work, and it then makes every subsequent tick an
    -- order of magnitude cheaper.
    _neighbourhood :: Stencil cs,
    _neighbourhoodIx :: Int,
    _presetIx :: Int,
    _boardName :: String,
    _isTicking :: Bool,
    _rng :: StdGen,
    -- | Recomputed whenever the window changes size, which is the only way
    -- this demo learns how big it is. See 'defaultWindow'.
    _display :: DisplayInfo
  }

makeLenses ''WorldState

-- | The tick interval never reaches zero and never grows past two seconds, so
-- @-@ held down cannot stall the simulation and @+@ cannot busy-loop it.
clampInterval :: Float -> Float
clampInterval = max 0.01 . min 2

-- | Cycle to the next entry of a non-empty list, returning its index too.
cycleNext :: [a] -> Int -> (Int, a)
cycleNext xs i = let j = (i + 1) `mod` length xs in (j, xs !! j)

gridPositionFromScreenCoord ::
  ( IsCoordLifted x,
    IsCoordLifted y
  ) =>
  DisplayInfo ->
  Float ->
  Float ->
  Maybe (Coord '[x, y])
gridPositionFromScreenCoord DisplayInfo {..} x y =
  let x' :: Int = floor ((x - originX) / tileSize + 0.5)
      y' :: Int = floor ((y - originY) / tileSize + 0.5)
   in (\a b -> review asOrdinal a :| review asOrdinal b :| EmptyCoord)
        <$> numToOrdinal x'
        <*> numToOrdinal y'

drawWorld ::
  forall cs a b.
  ( cs ~ '[a, b],
    IsCoordList cs
  ) =>
  DisplayInfo ->
  WorldState cs ->
  Picture
drawWorld DisplayInfo {..} ws =
  let image Alive = color black $ rectangleSolid tileSize tileSize
      image Dead = color (greyN 0.85) $ rectangleWire tileSize tileSize
      tile :: Coord cs -> TileState -> Picture
      tile p a =
        -- The cell's row-major indices, and deliberately NOT
        -- @p '.-.' mempty@, which is what this drew with until
        -- sized-grid-23y3.
        --
        -- On a 'Periodic' axis @('.-.')@ is the /shortest signed route/
        -- between two coordinates, so cell 59 of 60 comes back as -1
        -- rather than 59: the right-hand half of the board is drawn to the
        -- left of the left-hand half, the picture ends up centred on cell
        -- zero and spanning -29..+30 tiles, and most of it falls outside
        -- the window. That is the correct answer for a torus and the wrong
        -- one for a picture, which wants an index and not a displacement.
        let (i, j) = coordIndices2 p
         in translate
              (originX + tileSize * fromIntegral i)
              (originY + tileSize * fromIntegral j)
              $ image a
   in pictures $
        zipWith tile (allCoord @cs) (VG.toList (gridVector (ws ^. grid)))

-- | The heads-up display, in screen coordinates: what the simulation is doing,
-- and which key changes it.
drawHud :: WorldState cs -> DisplayInfo -> Picture
drawHud ws DisplayInfo {..} =
  pictures
    ( zipWith bodyLine [0 :: Int ..] body
        ++ zipWith keyLine [0 :: Int ..] legend
    )
  where
    bodyLine i s =
      translate hudLeft (hudTop - 25 * fromIntegral i) $
        scale 0.13 0.13 $
          color (greyN 0.2) $
            text s
    keyLine i s =
      translate hudLeft (hudTop - 25 * 5 - 20 * fromIntegral i) $
        scale 0.1 0.1 $
          color (greyN 0.45) $
            text s
    legend =
      [ "keys:  t run/pause   r randomise   c clear   p next preset   \
        \n next rule   v next neighbourhood",
        "       +/- faster/slower   click a cell to toggle it"
      ]
    interval = ws ^. tickInterval
    body =
      [ "Game of Life on " ++ topologyName,
        "generation "
          ++ show (ws ^. generation)
          ++ "    "
          ++ (if ws ^. isTicking then "running" else "paused"),
        "tick "
          ++ showF interval
          ++ "s ("
          ++ showF (1 / interval)
          ++ " gen/s)",
        "rule " ++ ruleName ++ "    neighbourhood " ++ neighbourhoodName,
        "board " ++ ws ^. boardName
      ]
    ruleName = fst (rules !! (ws ^. ruleIx) :: (String, Rule 0))
    -- 'neighbourhoods' is constrained, so reading a name out of it at @cs@
    -- would put an 'IsCoordList' on this function for the sake of a string
    -- --- and would build a stencil for the whole board every frame to throw
    -- it away. The names are the same at every shape, so they are read at the
    -- smallest one there is.
    neighbourhoodName =
      fst
        ( neighbourhoods !! (ws ^. neighbourhoodIx) ::
            (String, Stencil '[Ordinal 1])
        )

-- | Two decimal places, without bringing in @printf@ for one number.
showF :: Float -> String
showF x = show (fromIntegral (round (x * 100) :: Int) / 100 :: Float)

updateWorld ::
  forall x y.
  ( IsCoordLifted x,
    IsCoordLifted y
  ) =>
  Event ->
  WorldState '[x, y] ->
  WorldState '[x, y]
-- The one place the window's real size arrives. gloss reports a resize when
-- the user drags the frame, and on most backends once when the window opens,
-- so the layout snaps to the truth without this demo ever having to ask the
-- screen how big it is --- see 'defaultWindow' for why asking is a bad idea.
updateWorld (EventResize (w, h)) world =
  world & display .~ displayFor w h (topologyName (world ^. display))
updateWorld (EventKey (MouseButton LeftButton) Up _ (x, y)) world =
  case gridPositionFromScreenCoord (world ^. display) x y of
    Just p ->
      world
        & grid %~ imapGrid (\c a -> if c == p then flipTileState a else a)
        & boardName .~ "hand-edited"
    Nothing -> world
updateWorld (EventKey (Char c) Down _ _) world = onKey c world
updateWorld _ world = world

onKey ::
  forall x y.
  (IsCoordLifted x, IsCoordLifted y) =>
  Char ->
  WorldState '[x, y] ->
  WorldState '[x, y]
onKey 't' world = world & isTicking %~ not
onKey 'c' world =
  world & grid .~ emptyBoard & boardName .~ "empty" & generation .~ 0
onKey 'r' world =
  let (g, gen') = randomBoard (world ^. rng)
   in world
        & grid .~ g
        & rng .~ gen'
        & boardName .~ "random"
        & generation .~ 0
onKey 'p' world =
  let (i, p) = cycleNext presets (world ^. presetIx)
   in world
        & presetIx .~ i
        & grid .~ presetBoard p
        & boardName .~ presetName p
        & generation .~ 0
-- The two below are the whole of issue nnww.3: the rule and the neighbourhood
-- are values in 'WorldState', so selecting another one is a field update and
-- nothing downstream --- 'applyRule', 'tickWorld', the board type --- changes
-- at all.
onKey 'n' world =
  let (i, (_, r)) = cycleNext rules (world ^. ruleIx)
   in world & ruleIx .~ i & rule .~ r
onKey 'v' world =
  let (i, (_, s)) = cycleNext neighbourhoods (world ^. neighbourhoodIx)
   in world & neighbourhoodIx .~ i & neighbourhood .~ s
onKey k world
  -- Both spellings of each key: a keyboard that needs shift for @+@ reports
  -- @+@, and one that does not reports @=@.
  | k `elem` ("+=" :: String) = world & tickInterval %~ clampInterval . (/ 1.5)
  | k `elem` ("-_" :: String) = world & tickInterval %~ clampInterval . (* 1.5)
  | otherwise = world

tickWorld ::
  Float ->
  WorldState cs ->
  WorldState cs
tickWorld dt world
  | not (world ^. isTicking) = world
  | world ^. timeElapsedSinceLastTick + dt >= world ^. tickInterval =
      world
        & grid %~ applyRule (world ^. rule) (world ^. neighbourhood)
        & generation +~ 1
        & timeElapsedSinceLastTick +~ dt - world ^. tickInterval
  | otherwise = world & timeElapsedSinceLastTick +~ dt

main :: IO ()
main = do
  g <- newStdGen
  let (winW, winH) = defaultWindow
      firstPreset = gliderPreset
      startGame :: WorldState '[Periodic 60, Periodic 60] =
        WorldState
          { _grid = presetBoard firstPreset,
            _timeElapsedSinceLastTick = 0,
            _tickInterval = 0.1,
            _generation = 0,
            _rule = snd conwayRule,
            _ruleIx = 0,
            _neighbourhood = snd mooreR1,
            _neighbourhoodIx = 0,
            _presetIx = 0,
            _boardName = presetName firstPreset,
            _isTicking = False,
            _rng = g,
            _display = displayFor winW winH "Periodic 60 x Periodic 60"
          }
  play
    (InWindow "Game of Life -- grid-sized" (winW, winH) (20, 20))
    white
    60
    startGame
    (\ws -> pictures [drawWorld (ws ^. display) ws, drawHud ws (ws ^. display)])
    updateWorld
    tickWorld

-- | The window this demo opens at, before it is told any better.
--
-- Deliberately NOT derived from the screen. gloss offers
-- @Graphics.Gloss.Interface.Environment.getScreenSize@, but under the GLFW
-- backend that is three 'Data.Maybe.fromJust's deep
-- (@Backend/GLFW.hs:191-197@) and /throws/ where GLFW cannot name a monitor,
-- which happens headless, over SSH, and --- observed while fixing
-- sized-grid-23y3 --- intermittently on a perfectly ordinary desktop. It also
-- spins up a second backend before 'play' initialises its own. A demo that
-- dies before opening its window because it could not measure the screen is
-- strictly worse than one that opens at a reasonable size and then listens.
--
-- So: a size that fits any laptop screen made this decade, and 'updateWorld'
-- adapts the moment gloss reports the window's real size. Drag the frame and
-- the board and HUD re-lay themselves.
defaultWindow :: (Int, Int)
defaultWindow = (780, 820)
