{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Main (main) where

import           Data.Grid.Sized

import           Control.Comonad
import           Control.Comonad.Store
import           Control.Lens
import           Data.AffineSpace
import           Generics.SOP                       hiding (S, Z)
import           GHC.TypeLits
import           Graphics.Gloss.Interface.Pure.Game

data TileState
    = Alive
    | Dead
    deriving (Eq, Show)

flipTileState :: TileState -> TileState
flipTileState Alive = Dead
flipTileState Dead  = Alive

newtype Rule (n :: Nat) = Rule
    { runRule :: TileState -> [TileState] -> TileState
    }

gameOfLife :: Rule 2
gameOfLife = Rule $ \here neigh ->
    let aliveNeigh = length $ filter (== Alive) neigh
    in if | here == Alive && aliveNeigh `elem` [2,3] -> Alive
          | here == Dead && aliveNeigh == 3 -> Alive
          | otherwise -> Dead

applyRule ::
       ( IsCoordList cs
       , AllSizedKnown cs
       )
    => Rule n
    -> FocusedGrid cs TileState
    -> FocusedGrid cs TileState
applyRule rule =
    extend $ \fg ->
        runRule rule (extract fg) $
        map (`peek` fg) $ neighbours $ pos fg

data DisplayInfo = DisplayInfo {
  tileSize, offset :: Float
}

data WorldState cs = WorldState
    { _grid                     :: Grid cs TileState
    , _timeElapsedSinceLastTick :: Float
    , _rule                     :: Rule (Length cs)
    , _isTicking                :: Bool
    }
makeLenses ''WorldState

gridPositionFromScreenCoord ::
       ( IsCoordLifted x
       , IsCoordLifted y
       )
    => DisplayInfo
    -> Float
    -> Float
    -> Maybe (Coord '[ x, y])
gridPositionFromScreenCoord DisplayInfo{..} x y =
    let x' :: Integer = floor ((x + 0.5*tileSize + offset ) / tileSize)
        y' :: Int = floor ((y + 0.5 * tileSize + offset ) / tileSize)
    in (\a b ->
            Coord
                (I (view (re asOrdinal) a) :* I (view (re asOrdinal) b) :* Nil)) <$>
       numToOrdinal x' <*>
       numToOrdinal y'

drawWorld ::
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
        image Dead  = color black $ rectangleWire tileSize tileSize
    in ifoldMapOf
           (grid . itraversed)
           (\p a ->
                let (x :| y :| EmptyCoord) = p .-. mempty
                in translate (tileSize * fromIntegral x) (tileSize * fromIntegral y) $
                   image a)
           ws

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
        Just p  -> world & grid . gridIndex p %~ flipTileState
        Nothing -> world
updateWorld _ (EventKey (Char 't') Up _ _) world = world & isTicking %~ not
updateWorld _ _ world = world

tickWorld ::
       ( IsCoordList cs
       , AllSizedKnown cs
       )
    =>Float
    -> WorldState cs
    -> WorldState cs
tickWorld dt world
    | world ^. timeElapsedSinceLastTick + dt >= 0.1 && world ^. isTicking  =
        world & grid . asFocusedGrid %~ applyRule (world ^. rule)
              & timeElapsedSinceLastTick +~ dt - 0.1
    | world ^. isTicking = world & timeElapsedSinceLastTick +~ dt
    | otherwise = world

main :: IO ()
main =
    let startGame :: WorldState '[ Periodic 60, Periodic 60] =
            WorldState
            { _grid = pure Dead
            , _timeElapsedSinceLastTick = 0
            , _rule = gameOfLife
            , _isTicking = False
            }
        di = DisplayInfo {tileSize = 16, offset = 500}
    in play
           (InWindow "floatMe" (1100, 1100) (1, 1))
           white
           60
           startGame
           (translate (negate $ offset di) (negate $ offset di) . drawWorld di)
           (updateWorld di)
           tickWorld
