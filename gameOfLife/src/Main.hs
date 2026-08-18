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
import qualified Data.Vector.Generic                as VG
import qualified Data.Vector.Generic.Mutable        as VGM
import qualified Data.Vector.Unboxed                as VU
import           Data.Word                          (Word8)
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

newtype Rule (n :: Nat) = Rule
    { runRule :: TileState -> [TileState] -> TileState
    }

gameOfLife :: Rule 2
gameOfLife = Rule $ \here neigh ->
    let aliveNeigh = length $ filter (== Alive) neigh
    in if | here == Alive && aliveNeigh `elem` [2,3] -> Alive
          | here == Dead && aliveNeigh == 3 -> Alive
          | otherwise -> Dead

-- | One tick, as a bulk pass over the unboxed grid: for every coordinate,
-- read its neighbours back out of the old grid and decide the new tile.
-- The neighbourhood comes in as a precomputed 'Stencil' rather than being
-- enumerated per cell, since the same positions would otherwise be rebuilt
-- sixty times a second for no reason.
applyRule ::
       Rule n
    -> Stencil cs
    -> UGrid cs TileState
    -> UGrid cs TileState
applyRule rule s = stencilGrid s (runRule rule)

data DisplayInfo = DisplayInfo {
  tileSize, offset :: Float
}

data WorldState cs = WorldState
    { _grid                     :: UGrid cs TileState
    , _timeElapsedSinceLastTick :: Float
    , _rule                     :: Rule (Length cs)
    -- | The board's neighbourhood, built once at start-up and held for the
    -- lifetime of the game. It is a field rather than something 'tickWorld'
    -- computes because that is the whole trade a 'Stencil' offers: building one
    -- costs a tick's worth of work, and it then makes every subsequent tick an
    -- order of magnitude cheaper.
    , _neighbourhood            :: Stencil cs
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
        image Dead  = color black $ rectangleWire tileSize tileSize
        tile :: Coord cs -> TileState -> Picture
        tile p a =
            let (x :| y :| EmptyCoord) = p .-. mempty
            in translate (tileSize * fromIntegral x) (tileSize * fromIntegral y) $
               image a
    in pictures $
       zipWith tile (allCoord @cs) (VG.toList (gridVector (ws ^. grid)))

updateWorld :: forall x y .
       ( IsCoordLifted x
       , IsCoordLifted y
       , Eq x
       , Eq y
       )
    => DisplayInfo
    -> Event
    -> WorldState '[ x, y]
    -> WorldState '[ x, y]
updateWorld di (EventKey (MouseButton LeftButton) Up _ (x, y)) world =
    case gridPositionFromScreenCoord di x y of
        Just p  -> world & grid %~ imapGrid (\c a -> if c == p then flipTileState a else a)
        Nothing -> world
updateWorld _ (EventKey (Char 't') Up _ _) world = world & isTicking %~ not
updateWorld _ _ world = world

tickWorld ::
       Float
    -> WorldState cs
    -> WorldState cs
tickWorld dt world
    | world ^. timeElapsedSinceLastTick + dt >= 0.1 && world ^. isTicking  =
        world & grid %~ applyRule (world ^. rule) (world ^. neighbourhood)
              & timeElapsedSinceLastTick +~ dt - 0.1
    | world ^. isTicking = world & timeElapsedSinceLastTick +~ dt
    | otherwise = world

main :: IO ()
main =
    let startGame :: WorldState '[ Periodic 60, Periodic 60] =
            WorldState
            { _grid = tabulateGrid (const Dead)
            , _timeElapsedSinceLastTick = 0
            , _rule = gameOfLife
            , _neighbourhood = mooreStencil 1
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
