{-# LANGUAGE DataKinds #-}

-- | The window: fold the move stream into a picture, a few moves per frame.
module Maze.Render
  ( animate
  ) where

import           Maze.Generate
import           Maze.Model
import           Maze.Solve

import           Data.Grid.Sized                    hiding (Compose)

import           Control.Lens                       ((&), (.~))
import           Graphics.Gloss.Interface.Pure.Game
import           System.Random                      (StdGen, split)

-- | What a cell looks like now, which is more than what it /is/: the search
-- paints over the maze rather than changing it.
data Paint
    = Rock
    | Open
    | Flooded !Int
    | OnRoute
    deriving (Eq)

data Phase
    = Carving
    | Searching
    | Finished !Int
    -- ^ Route length.
    | NoRoute
    deriving (Eq)

data View = View
    { viewPaint  :: Grid Cs Paint
    , viewHead   :: Maybe (Coord Cs)
    , viewRest   :: [Move]
    , viewPhase  :: Phase
    , viewSteps  :: !Int
    , viewSeed   :: StdGen
    -- ^ The generator this maze was built from, kept so @r@ can rebuild the
    -- same maze and @n@ can take a fresh one.
    , viewRunning :: Bool
    , viewRate   :: Float
    , viewOwed   :: Float
    }

-- | The two phases as one stream. 'carve' hands back the finished maze
-- alongside its moves, and both are lazy, so this appends the search to the
-- carving without either being computed before it is watched.
allMoves :: StdGen -> [Move]
allMoves g = let (evs, maze) = carve g in evs ++ solve maze

startView :: Float -> StdGen -> View
startView rate g =
    View
    { viewPaint = tabulateGrid (const Rock)
    , viewHead = Nothing
    , viewRest = allMoves g
    , viewPhase = Carving
    , viewSteps = 0
    , viewSeed = g
    , viewRunning = True
    , viewRate = rate
    , viewOwed = 0
    }

-- | Run the demo at the given events per second. Blocks until the window is
-- closed.
animate :: Float -> StdGen -> IO ()
animate rate g =
    play
        (InWindow "Maze -- grid-sized" (760, 900) (1, 1))
        (greyN 0.15)
        60
        (startView rate g)
        draw
        onGlossEvent
        onTick

-- * Stepping

step1 :: View -> View
step1 v =
    case viewRest v of
        [] -> v {viewRunning = False}
        (e:es) ->
            let v' = v {viewRest = es, viewSteps = viewSteps v + 1}
            in case e of
                   Opened c -> v' {viewPaint = paint c Open v}
                   Moved c -> v' {viewHead = Just c}
                   Reached c d ->
                       v' { viewPaint = paint c (Flooded d) v
                          , viewHead = Nothing
                          , viewPhase = Searching
                          }
                   Solved route ->
                       v' { viewPaint =
                                foldl
                                    (\g c -> g & gridIndex c .~ OnRoute)
                                    (viewPaint v)
                                    route
                          , viewPhase = Finished (length route)
                          , viewRunning = False
                          }
                   Unreachable -> v' {viewPhase = NoRoute, viewRunning = False}
  where
    paint c p w = viewPaint w & gridIndex c .~ p

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

-- | gloss's @Event@ is a keypress; this demo's 'Move' is a step of the
-- algorithm. Renaming the latter is what keeps both spellings honest.
onGlossEvent :: Event -> View -> View
onGlossEvent (EventKey (Char c) Down _ _) v = onKey c v
onGlossEvent (EventKey (SpecialKey KeySpace) Down _ _) v =
    v {viewRunning = not (viewRunning v)}
onGlossEvent _ v = v

onKey :: Char -> View -> View
onKey 't' v = v {viewRunning = not (viewRunning v)}
onKey 'r' v = startView (viewRate v) (viewSeed v)
onKey 'n' v = startView (viewRate v) (snd (split (viewSeed v)))
onKey k v
    | k `elem` ("+=" :: String) = v {viewRate = min 20000 (viewRate v * 2)}
    | k `elem` ("-_" :: String) = v {viewRate = max 1 (viewRate v / 2)}
    | otherwise = v

-- * Drawing

tileSize :: Float
tileSize = 11

cellCentre :: Coord Cs -> (Float, Float)
cellCentre c =
    let (i, j) = coordXY c
    in ( tileSize * (fromIntegral i - fromIntegral (side - 1) / 2)
       , tileSize * (fromIntegral j - fromIntegral (side - 1) / 2) - 60
       )

paintColour :: Paint -> Color
paintColour Rock = greyN 0.22
paintColour Open = greyN 0.93
paintColour (Flooded d) =
    -- Cycled rather than scaled to the longest distance, which is not known
    -- until the search ends: the bands make the frontier's shape readable
    -- while it is still moving.
    let t = fromIntegral (d `mod` 40) / 40
    in makeColor (0.2 + 0.3 * t) (0.55 - 0.2 * t) (0.9 - 0.3 * t) 1
paintColour OnRoute = makeColor 0.35 0.9 0.4 1

draw :: View -> Picture
draw v = pictures (cells ++ [marker startCell, marker goalCell] ++ carveHead ++ [hud v])
  where
    cells =
        [ translate x y (color (paintColour p) (rectangleSolid tileSize tileSize))
        | c <- allCoord
        , let p = indexGrid (viewPaint v) c
        , p /= Rock
        , let (x, y) = cellCentre c
        ]
    marker c =
        let (x, y) = cellCentre c
        in translate x y $
           color (makeColor 1 0.75 0.2 1) $ rectangleWire (tileSize + 4) (tileSize + 4)
    carveHead =
        [ translate x y (color (makeColor 1 0.55 0.1 1) (rectangleSolid tileSize tileSize))
        | Just c <- [viewHead v]
        , let (x, y) = cellCentre c
        ]

hud :: View -> Picture
hud v =
    pictures
        (zipWith bodyLine [0 :: Int ..] body ++
         zipWith keyLine [0 :: Int ..] legend)
  where
    bodyLine i s =
        translate (-360) (410 - 26 * fromIntegral i) $
        scale 0.13 0.13 $ color (greyN 0.9) $ text s
    keyLine i s =
        translate (-360) (410 - 26 * 3 - 20 * fromIntegral i) $
        scale 0.1 0.1 $ color (greyN 0.6) $ text s
    legend =
        [ "keys:  t / space run/pause   r same maze again   n new maze   +/- faster/slower"
        , "       the head carves; the flood is breadth-first search; green is the route"
        ]
    body =
        [ "Maze on Clamped 61 x Clamped 61"
        , phase
        , "event " ++ show (viewSteps v) ++ "    " ++
          show (round (viewRate v) :: Int) ++ "/s    " ++
          (if viewRunning v
               then "running"
               else "paused")
        ]
    phase =
        case viewPhase v of
            Carving    -> "carving (randomised depth-first, head is the grid's focus)"
            Searching  -> "searching (breadth-first from the top-left cell)"
            Finished n -> "solved: a route of " ++ show n ++ " cells"
            NoRoute    -> "no route"
