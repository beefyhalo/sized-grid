{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Wireworld: electricity, as a cellular automaton.
--
-- Here to answer one question about the game-of-life example --- is that
-- @Stencil@ + @stencilGrid@ machinery a game-of-life loop, or an automaton
-- engine? Wireworld is the awkward case. It has four states rather than two,
-- and its transition is not totalistic in the game-of-life sense: three of the
-- four states ignore their neighbours entirely and the fourth counts only one
-- of the other three.
--
-- It needs nothing new. The rule is @Cell -> [Cell] -> Cell@, which is what
-- 'stencilGrid' already takes, and the grid is a 'UGrid' of a four-value type
-- packed into a byte, which is what the game-of-life example already does with
-- two. The one thing that /is/ different is the axis type --- 'Clamped' rather
-- than 'Periodic', because a circuit has edges and a wire that came back round
-- the other side would be a wire nobody drew --- and that difference costs no
-- code here at all. It is one word in 'Cs'.
module Automata.Wireworld
  ( run
  ) where

import           Data.Grid.Sized
import           Data.Grid.Sized.Unboxed

import           Control.Lens                       (review)
import           Data.Maybe                         (mapMaybe)
import qualified Data.Vector.Generic                as VG
import qualified Data.Vector.Generic.Mutable        as VGM
import qualified Data.Vector.Unboxed                as VU
import           Data.Word                          (Word8)
import           Graphics.Gloss.Interface.Pure.Game

-- | A cell of a Wireworld circuit. An electron is the pair 'ElectronHead'
-- followed by 'ElectronTail'; the tail is what stops it running backwards
-- down the wire it just came along.
data Cell
    = Empty
    | Conductor
    | ElectronHead
    | ElectronTail
    deriving (Eq, Show)

instance VU.IsoUnbox Cell Word8 where
    toURepr Empty        = 0
    toURepr Conductor    = 1
    toURepr ElectronHead = 2
    toURepr ElectronTail = 3
    fromURepr 0          = Empty
    fromURepr 1          = Conductor
    fromURepr 2          = ElectronHead
    fromURepr _          = ElectronTail
    {-# INLINE toURepr #-}
    {-# INLINE fromURepr #-}

newtype instance VU.MVector s Cell = MV_Cell (VU.MVector s Word8)
newtype instance VU.Vector    Cell = V_Cell  (VU.Vector    Word8)
deriving via (Cell `VU.As` Word8) instance VGM.MVector VU.MVector Cell
deriving via (Cell `VU.As` Word8) instance VG.Vector   VU.Vector  Cell
instance VU.Unbox Cell

-- | The board. 'Clamped', not 'Periodic': see the module header.
--
-- Only just big enough for the largest circuit below, which is 20 by 16. A
-- roomier board is not neutral: nothing draws an empty cell, so a circuit on a
-- large board is a small bright thing adrift in a lot of background.
type Cs = '[ Clamped 28, Clamped 28]

-- | The whole of Wireworld.
--
-- Note what this is not: it is not @count the live neighbours and look the
-- answer up@. Empty stays empty whatever surrounds it, a head becomes a tail
-- and a tail becomes conductor with no reference to the neighbourhood, and
-- only 'Conductor' reads it --- and then only for heads. 'stencilGrid' does
-- not care: its rule is a function of the cell and its neighbours, and this
-- is one.
wireworld :: Cell -> [Cell] -> Cell
wireworld here neigh =
    case here of
        Empty -> Empty
        ElectronHead -> ElectronTail
        ElectronTail -> Conductor
        Conductor
            | heads == 1 || heads == 2 -> ElectronHead
            | otherwise -> Conductor
  where
    heads = length (filter (== ElectronHead) neigh)

-- | One generation. The same call the game-of-life example makes, at a
-- different element type and a different rule.
step :: Stencil Cs -> UGrid Cs Cell -> UGrid Cs Cell
step s = stencilGrid s wireworld

neighbourhood :: Stencil Cs
neighbourhood = mooreStencil 1

-- * Circuits

-- | A named circuit, drawn as itself: @C@ conductor, @H@ electron head, @t@
-- electron tail, anything else empty. Row 0 is the top row.
data Circuit = Circuit
    { circuitName :: String
    , circuitRows :: [String]
    }

-- | Both of these were checked by simulation before they were written down: a
-- loop runs one electron round forever and emits exactly one down its output
-- wire per revolution, and the two-clock scene stays at four electrons over
-- three thousand generations rather than running away.
--
-- The corners are the part worth looking at. A right-angle corner in a
-- one-cell wire puts the cell /before/ the corner diagonally next to the cell
-- /after/ it, and since the neighbourhood is Moore the electron uses that
-- shortcut to run backwards past its own tail and the loop fills with heads.
-- Cutting every corner diagonally, as below, leaves only consecutive cells
-- adjacent.
circuits :: [Circuit]
circuits =
    clockCircuit :
    [ Circuit
          "Two clocks, two periods"
          [ "..CCCC.............."
          , ".C....C............."
          , "C......C............"
          , "C......C............"
          , ".C....C............."
          , "..HtCC.............."
          , "......CCCCCCCCCCCCC."
          , "...................."
          , "..CCCCCCCC.........."
          , ".C........C........."
          , "C..........C........"
          , "C..........C........"
          , "C..........C........"
          , ".C........C........."
          , "..HtCCCCCC.........."
          , "..........CCCCCCCCC."
          ]
    ]

-- | The circuit the demo opens on, named separately so 'main' need not take
-- the head of a list.
clockCircuit :: Circuit
clockCircuit =
    Circuit
        "Clock and output"
        [ "..CCCC........."
        , ".C....C........"
        , "C......C......."
        , "C......C......."
        , "C......C......."
        , "C......C......."
        , ".C....C........"
        , "..HtCC........."
        , "......CCCCCCCC."
        ]

axisSize :: forall x. IsCoordLifted x => Int
axisSize = ordinalSize @(CoordNat x)

-- | Lay a circuit out centred on the board.
circuitBoard :: Circuit -> UGrid Cs Cell
circuitBoard circuit =
    let live =
            mapMaybe
                (\(cx, cy, v) -> (\c -> (coordPosition c, v)) <$> coordAt cx cy)
                [ (baseX + cx, baseY + cy, v)
                | (cy, row) <- zip [0 ..] (reverse rows)
                , (cx, ch) <- zip [0 ..] row
                , Just v <- [cellOf ch]
                ]
    in tabulateGrid (\c -> maybe Empty id (lookup (coordPosition c) live))
  where
    rows = circuitRows circuit
    height = length rows
    width = maximum (0 : map length rows)
    baseX = (axisSize @(Clamped 28) - width) `div` 2
    baseY = (axisSize @(Clamped 28) - height) `div` 2
    cellOf 'C' = Just Conductor
    cellOf 'H' = Just ElectronHead
    cellOf 't' = Just ElectronTail
    cellOf _   = Nothing

coordAt :: Int -> Int -> Maybe (Coord Cs)
coordAt cx cy =
    (\a b -> review asOrdinal a :| review asOrdinal b :| EmptyCoord) <$>
    numToOrdinal cx <*>
    numToOrdinal cy

-- * The window

data World = World
    { worldGrid       :: UGrid Cs Cell
    , worldCircuitIx  :: Int
    , worldName       :: String
    , worldGeneration :: !Int
    , worldRunning    :: Bool
    , worldInterval   :: Float
    , worldElapsed    :: Float
    , worldWin        :: (Int, Int)
    -- ^ The window's real size, from 'EventResize'. See 'fitTo'.
    }

startWorld :: (Int, Int) -> Float -> Circuit -> Int -> World
startWorld win interval c ix =
    World
    { worldGrid = circuitBoard c
    , worldCircuitIx = ix
    , worldName = circuitName c
    , worldGeneration = 0
    , worldRunning = True
    , worldInterval = interval
    , worldElapsed = 0
    , worldWin = win
    }

-- | Run the Wireworld demo at the given generations per second.
run :: Float -> IO ()
run rate =
    play
        (InWindow "Wireworld -- grid-sized" defaultWindow (20, 20))
        (greyN 0.12)
        60
        (startWorld defaultWindow (1 / rate) clockCircuit 0)
        draw
        onEvent
        onTick

onTick :: Float -> World -> World
onTick dt w
    | not (worldRunning w) = w
    | worldElapsed w + dt >= worldInterval w =
        w { worldGrid = step neighbourhood (worldGrid w)
          , worldGeneration = worldGeneration w + 1
          , worldElapsed = worldElapsed w + dt - worldInterval w
          }
    | otherwise = w {worldElapsed = worldElapsed w + dt}

onEvent :: Event -> World -> World
onEvent (EventResize wh) w = w {worldWin = wh}
onEvent (EventKey (Char c) Down _ _) w = onKey c w
onEvent _ w = w

onKey :: Char -> World -> World
onKey 't' w = w {worldRunning = not (worldRunning w)}
onKey 'r' w =
    startWorld
        (worldWin w)
        (worldInterval w)
        (circuits !! worldCircuitIx w)
        (worldCircuitIx w)
onKey 'p' w =
    let ix = (worldCircuitIx w + 1) `mod` length circuits
    in startWorld (worldWin w) (worldInterval w) (circuits !! ix) ix
onKey k w
    | k `elem` ("+=" :: String) =
        w {worldInterval = clampInterval (worldInterval w / 1.5)}
    | k `elem` ("-_" :: String) =
        w {worldInterval = clampInterval (worldInterval w * 1.5)}
    | otherwise = w

clampInterval :: Float -> Float
clampInterval = max 0.01 . min 2

-- * Drawing

tileSize :: Float
tileSize = 30

-- | Screen position of the centre of a cell. Row-major, so the flat position
-- divides into the two axis indices; taking them that way rather than
-- through @('.-.')@ keeps this working whatever the axis types are.
cellCentre :: Coord Cs -> (Float, Float)
cellCentre c =
    let (i, j) = coordPosition c `divMod` axisSize @(Clamped 28)
    in ( tileSize * (fromIntegral i - 13.5)
       , tileSize * (fromIntegral j - 13.5) - 100
       )

cellColour :: Cell -> Maybe Color
cellColour Empty        = Nothing
cellColour Conductor    = Just (makeColor 0.35 0.28 0.05 1)
cellColour ElectronHead = Just (makeColor 0.45 0.75 1.0 1)
cellColour ElectronTail = Just (makeColor 0.85 0.35 0.95 1)

draw :: World -> Picture
draw w = fitTo (worldWin w) $ pictures (cells ++ [hud w])
  where
    cells =
        [ translate x y (color col (rectangleSolid (tileSize - 2) (tileSize - 2)))
        | (c, v) <- zip allCoord (VG.toList (gridVector (worldGrid w)))
        , Just col <- [cellColour v]
        , let (x, y) = cellCentre c
        ]

hud :: World -> Picture
hud w =
    pictures
        (zipWith bodyLine [0 :: Int ..] body ++
         zipWith keyLine [0 :: Int ..] legend)
  where
    bodyLine i s =
        translate (-430) (495 - 26 * fromIntegral i) $
        scale 0.13 0.13 $ color (greyN 0.85) $ text s
    keyLine i s =
        translate (-430) (495 - 26 * 4 - 20 * fromIntegral i) $
        scale 0.1 0.1 $ color (greyN 0.55) $ text s
    legend =
        [ "keys:  t run/pause   r restart   p next circuit   +/- faster/slower"
        , "       conductor is amber, an electron head blue and its tail violet"
        ]
    body =
        [ "Wireworld on Clamped 28 x Clamped 28"
        , "circuit " ++ worldName w
        , "generation " ++ show (worldGeneration w) ++ "    " ++
          (if worldRunning w
               then "running"
               else "paused")
        , showF (1 / worldInterval w) ++ " gen/s"
        ]

showF :: Float -> String
showF x = show (fromIntegral (round (x * 10) :: Int) / 10 :: Float)

-- * Fitting the layout to the window

-- | The coordinate space everything above is laid out in, the window this
-- demo opens at, and the scale between them.
--
-- See the note on @Automata.Ant.fitTo@: the layout is written against a fixed
-- space and the picture is scaled, because a hardcoded window can be taller
-- than the screen and gloss's @getScreenSize@ throws rather than saying it
-- does not know. sized-grid-23y3.
designW, designH :: Float
designW = 900
designH = 1080

defaultWindow :: (Int, Int)
defaultWindow = (690, 820)

fitTo :: (Int, Int) -> Picture -> Picture
fitTo (w, h) = scale k k
  where
    k = min (fromIntegral w / designW) (fromIntegral h / designH)
