-- 'axisSize' names its axis only under a type family, so every call has to say
-- which axis it means with a visible type application; the signature cannot be
-- inferred from the arguments because it has none.
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Main (main) where

import           Data.Grid.Sized
import           Data.Grid.Sized.Unboxed

import           Control.Lens
import           Data.AffineSpace
import           Data.Maybe                         (mapMaybe)
import qualified Data.Vector.Generic                as VG
import qualified Data.Vector.Generic.Mutable        as VGM
import qualified Data.Vector.Unboxed                as VU
import           Data.Word                          (Word8)
import           GHC.TypeLits
import           Graphics.Gloss.Interface.Pure.Game
import           System.Random                      (StdGen, newStdGen, randoms,
                                                     split)

data TileState
    = Alive
    | Dead
    deriving (Eq, Show)

flipTileState :: TileState -> TileState
flipTileState Alive = Dead
flipTileState Dead  = Alive

-- | Unboxed as a single byte, so a 'UGrid' can hold a board of these without
-- a pointer per tile.
instance VU.IsoUnbox TileState Word8 where
    toURepr Dead  = 0
    toURepr Alive = 1
    fromURepr 0   = Dead
    fromURepr _   = Alive
    {-# INLINE toURepr #-}
    {-# INLINE fromURepr #-}

newtype instance VU.MVector s TileState = MV_TileState (VU.MVector s Word8)
newtype instance VU.Vector    TileState = V_TileState  (VU.Vector    Word8)
deriving via (TileState `VU.As` Word8) instance VGM.MVector VU.MVector TileState
deriving via (TileState `VU.As` Word8) instance VG.Vector   VU.Vector  TileState
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
    in if | here == Alive && aliveNeigh `elem` survives -> Alive
          | here == Dead  && aliveNeigh `elem` born -> Alive
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
    conwayRule :
    [ ("HighLife B36/S23", totalistic [3, 6] [2, 3])
    , ("Seeds B2/S",       totalistic [2] [])
    , ("Day&Night B3678/S34678", totalistic [3, 6, 7, 8] [3, 4, 6, 7, 8])
    ]

-- | The game-of-life neighbourhood, named separately for the same reason
-- 'conwayRule' is.
mooreR1 :: IsCoordList cs => (String, Stencil cs)
mooreR1 = ("Moore r=1 (8)", mooreStencil 1)

-- | The neighbourhoods @v@ cycles through.
--
-- Constrained rather than a CAF, so each entry is rebuilt when it is selected.
-- That is deliberate: a 'Stencil' is a table sized by the whole grid, and
-- holding four of them alive for the lifetime of the window to save a
-- keypress's worth of work is the wrong trade. The one in play is kept in
-- 'WorldState'; the rest are rebuilt on demand.
neighbourhoods :: IsCoordList cs => [(String, Stencil cs)]
neighbourhoods =
    mooreR1 :
    [ ("von Neumann r=1 (4)", vonNeumannStencil 1)
    , ("Moore r=2 (24)",      mooreStencil 2)
    , ("von Neumann r=2 (8)", vonNeumannStencil 2)
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
       Rule n
    -> Stencil cs
    -> UGrid cs TileState
    -> UGrid cs TileState
applyRule rule s = stencilGrid s (runRule rule)

-- * Starting boards

-- | A named starting pattern, written as a picture of itself: @\'O\'@ is a
-- live cell and every other character is dead, with row 0 the /top/ row, so
-- the literal below reads the way the board draws.
data Preset = Preset
    { presetName   :: String
    , presetRows   :: [String]
    , presetAnchor :: Anchor
    }

-- | Where a preset's bottom-left corner goes.
data Anchor
    = Centred
    | NearTopRight Int
    -- ^ Inset from the top-right corner, so a pattern that travels up and to
    -- the right reaches the edge within seconds. Only useful on a wrapping
    -- axis, which is the point.

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
        [ ".OO"
        , "O.O"
        , "..O"
        ]
        (NearTopRight 6)

-- | The presets @p@ cycles through.
presets :: [Preset]
presets =
    gliderPreset :
    [ Preset
          "Gosper glider gun"
          [ "........................O..........."
          , "......................O.O..........."
          , "............OO......OO............OO"
          , "...........O...O....OO............OO"
          , "OO........O.....O...OO.............."
          , "OO........O...O.OO....O.O..........."
          , "..........O.....O.......O..........."
          , "...........O...O...................."
          , "............OO......................"
          ]
          Centred
    , Preset
          "R-pentomino"
          [ ".OO"
          , "OO."
          , ".O."
          ]
          Centred
    , Preset
          "Acorn"
          [ ".O....."
          , "...O..."
          , "OO..OOO"
          ]
          Centred
    ]

-- | The size of one axis, as a plain 'Int'.
axisSize :: forall x. IsCoordLifted x => Int
axisSize = ordinalSize @(CoordNat x)

-- | The live cells of a preset, as coordinates on the board it is laid onto.
--
-- The rows are read bottom-up, since 'presetRows' puts row 0 at the top and
-- the second axis is drawn upwards, and the result is taken modulo the axis
-- sizes so a pattern larger than the board wraps rather than being silently
-- truncated.
presetCoords ::
       forall x y. (IsCoordLifted x, IsCoordLifted y)
    => Preset
    -> [Coord '[ x, y]]
presetCoords Preset {..} =
    mapMaybe
        (\(cx, cy) -> coordAt (baseX + cx) (baseY + cy))
        [ (cx, cy)
        | (cy, row) <- zip [0 ..] (reverse presetRows)
        , (cx, ch) <- zip [0 ..] row
        , ch == 'O'
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
       forall x y. (IsCoordLifted x, IsCoordLifted y)
    => Int
    -> Int
    -> Maybe (Coord '[ x, y])
coordAt cx cy =
    (\a b -> review asOrdinal a :| review asOrdinal b :| EmptyCoord) <$>
    numToOrdinal (cx `mod` axisSize @x) <*>
    numToOrdinal (cy `mod` axisSize @y)

emptyBoard :: IsCoordList cs => UGrid cs TileState
emptyBoard = tabulateGrid (const Dead)

presetBoard ::
       forall x y. (IsCoordLifted x, IsCoordLifted y)
    => Preset
    -> UGrid '[ x, y] TileState
presetBoard p =
    let live = map coordPosition (presetCoords @x @y p)
    in tabulateGrid (\c -> if coordPosition c `elem` live then Alive else Dead)

randomBoard ::
       forall cs. IsCoordList cs
    => StdGen
    -> (UGrid cs TileState, StdGen)
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
    { tileSize     :: Float
    , originX      :: Float
    -- ^ Screen position of the centre of the cell at 'zeroCoord'.
    , originY      :: Float
    , topologyName :: String
    -- ^ What the board's axis types are, for the HUD. A string rather than
    -- something derived, because the interesting half of it --- @Periodic@ ---
    -- is a type name and not a number.
    }

data WorldState cs = WorldState
    { _grid                     :: UGrid cs TileState
    , _timeElapsedSinceLastTick :: Float
    , _tickInterval             :: Float
    , _generation               :: Int
    , _rule                     :: Rule (Length cs)
    , _ruleIx                   :: Int
    , _neighbourhood            :: Stencil cs
    -- ^ The board's neighbourhood, built when it is selected and held until
    -- the selection changes. It is a field rather than something 'tickWorld'
    -- computes because that is the whole trade a 'Stencil' offers: building one
    -- costs a tick's worth of work, and it then makes every subsequent tick an
    -- order of magnitude cheaper.
    , _neighbourhoodIx          :: Int
    , _presetIx                 :: Int
    , _boardName                :: String
    , _isTicking                :: Bool
    , _rng                      :: StdGen
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
       ( IsCoordLifted x
       , IsCoordLifted y
       )
    => DisplayInfo
    -> Float
    -> Float
    -> Maybe (Coord '[ x, y])
gridPositionFromScreenCoord DisplayInfo{..} x y =
    let x' :: Int = floor ((x - originX) / tileSize + 0.5)
        y' :: Int = floor ((y - originY) / tileSize + 0.5)
    in (\a b -> review asOrdinal a :| review asOrdinal b :| EmptyCoord) <$>
       numToOrdinal x' <*>
       numToOrdinal y'

drawWorld :: forall cs a b.
       ( cs ~ '[ a, b]
       , All Monoid cs
       , IsCoordList cs
       , All AffineSpace cs
       , All Integral (MapDiff cs)
       )
    => DisplayInfo
    -> WorldState cs
    -> Picture
drawWorld DisplayInfo{..} ws =
    let image Alive = color black $ rectangleSolid tileSize tileSize
        image Dead  = color (greyN 0.85) $ rectangleWire tileSize tileSize
        tile :: Coord cs -> TileState -> Picture
        tile p a =
            let (x :^ y :^ NoDelta) = p .-. mempty
            in translate (originX + tileSize * fromIntegral x)
                         (originY + tileSize * fromIntegral y) $
               image a
    in pictures $
       zipWith tile (allCoord @cs) (VG.toList (gridVector (ws ^. grid)))

-- | The heads-up display, in screen coordinates: what the simulation is doing,
-- and which key changes it.
drawHud :: WorldState cs -> DisplayInfo -> Picture
drawHud ws DisplayInfo{..} =
    pictures
        (zipWith bodyLine [0 :: Int ..] body ++
         zipWith keyLine [0 :: Int ..] legend)
  where
    bodyLine i s =
        translate (-490) (528 - 25 * fromIntegral i) $
        scale 0.13 0.13 $ color (greyN 0.2) $ text s
    keyLine i s =
        translate (-490) (528 - 25 * 5 - 20 * fromIntegral i) $
        scale 0.1 0.1 $ color (greyN 0.45) $ text s
    legend =
        [ "keys:  t run/pause   r randomise   c clear   p next preset   \
          \n next rule   v next neighbourhood"
        , "       +/- faster/slower   click a cell to toggle it"
        ]
    interval = ws ^. tickInterval
    body =
        [ "Game of Life on " ++ topologyName
        , "generation " ++ show (ws ^. generation) ++ "    " ++
          (if ws ^. isTicking then "running" else "paused")
        , "tick " ++ showF interval ++ "s (" ++ showF (1 / interval) ++
          " gen/s)"
        , "rule " ++ ruleName ++ "    neighbourhood " ++ neighbourhoodName
        , "board " ++ ws ^. boardName
        ]
    ruleName = fst (rules !! (ws ^. ruleIx) :: (String, Rule 0))
    -- 'neighbourhoods' is constrained, so reading a name out of it at @cs@
    -- would put an 'IsCoordList' on this function for the sake of a string
    -- --- and would build a stencil for the whole board every frame to throw
    -- it away. The names are the same at every shape, so they are read at the
    -- smallest one there is.
    neighbourhoodName =
        fst
            (neighbourhoods !! (ws ^. neighbourhoodIx) ::
                 (String, Stencil '[ Ordinal 1]))

-- | Two decimal places, without bringing in @printf@ for one number.
showF :: Float -> String
showF x = show (fromIntegral (round (x * 100) :: Int) / 100 :: Float)

updateWorld :: forall x y .
       ( IsCoordLifted x
       , IsCoordLifted y
       )
    => DisplayInfo
    -> Event
    -> WorldState '[ x, y]
    -> WorldState '[ x, y]
updateWorld di (EventKey (MouseButton LeftButton) Up _ (x, y)) world =
    case gridPositionFromScreenCoord di x y of
        Just p ->
            world &
            grid %~ imapGrid (\c a -> if c == p then flipTileState a else a) &
            boardName .~ "hand-edited"
        Nothing -> world
updateWorld _ (EventKey (Char c) Down _ _) world = onKey c world
updateWorld _ _ world = world

onKey ::
       forall x y. (IsCoordLifted x, IsCoordLifted y)
    => Char
    -> WorldState '[ x, y]
    -> WorldState '[ x, y]
onKey 't' world = world & isTicking %~ not
onKey 'c' world =
    world & grid .~ emptyBoard & boardName .~ "empty" & generation .~ 0
onKey 'r' world =
    let (g, gen') = randomBoard (world ^. rng)
    in world & grid .~ g & rng .~ gen' & boardName .~ "random" &
       generation .~ 0
onKey 'p' world =
    let (i, p) = cycleNext presets (world ^. presetIx)
    in world & presetIx .~ i & grid .~ presetBoard p &
       boardName .~ presetName p & generation .~ 0
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
       Float
    -> WorldState cs
    -> WorldState cs
tickWorld dt world
    | not (world ^. isTicking) = world
    | world ^. timeElapsedSinceLastTick + dt >= world ^. tickInterval =
        world & grid %~ applyRule (world ^. rule) (world ^. neighbourhood)
              & generation +~ 1
              & timeElapsedSinceLastTick +~ dt - world ^. tickInterval
    | otherwise = world & timeElapsedSinceLastTick +~ dt

main :: IO ()
main = do
    g <- newStdGen
    let firstPreset = gliderPreset
        startGame :: WorldState '[ Periodic 60, Periodic 60] =
            WorldState
            { _grid = presetBoard firstPreset
            , _timeElapsedSinceLastTick = 0
            , _tickInterval = 0.1
            , _generation = 0
            , _rule = snd conwayRule
            , _ruleIx = 0
            , _neighbourhood = snd mooreR1
            , _neighbourhoodIx = 0
            , _presetIx = 0
            , _boardName = presetName firstPreset
            , _isTicking = False
            , _rng = g
            }
        -- 60 tiles of 15px is 900px of board, left-aligned in a 1000px window
        -- with the HUD in the 180px above it.
        di =
            DisplayInfo
            { tileSize = 15
            , originX = -442.5
            , originY = -532.5
            , topologyName = "Periodic 60 x Periodic 60"
            }
    play
        (InWindow "Game of Life -- grid-sized" (1000, 1120) (1, 1))
        white
        60
        startGame
        (\ws -> pictures [drawWorld di ws, drawHud ws di])
        (updateWorld di)
        tickWorld
