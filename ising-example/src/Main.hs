{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main (main) where

import           Data.Grid.Sized

-- No Control.Comonad here any more. 'singleEnergy' was the only user of it,
-- through a 'FocusedGrid' it seeked to a coordinate the caller already held and
-- then only ever asked 'peek' and 'extract' -- which is 'indexGrid' either way.
-- The comonadic interface is the right one when the focus is carried across a
-- computation; a single-site Metropolis update does not carry one.
import           Control.Lens
import           Control.Monad
import           Control.Monad.Random
import           Data.AffineSpace
import           Data.Maybe            (fromMaybe)
import           Data.Proxy
import qualified GHC.TypeLits          as GHC
import           Graphics.Gloss
import           Pipes                 hiding (Proxy, each)
import qualified Pipes.Prelude         as P

data Spin = Up | Down deriving (Eq,Show,Enum,Bounded)

flipSpin :: Spin -> Spin
flipSpin Up   = Down
flipSpin Down = Up

spinNumber :: Num a => Spin -> a
spinNumber Up   = 1
spinNumber Down = -1

newtype PhysicalOptions = PhysicalOptions
  { coupling :: Double
  } deriving (Eq, Show)

instance Random Spin where
  random g =
    let (a, g') = random g
    in if a
         then (Up, g')
         else (Down, g')
  randomR (mi, ma) g =
    let toBool Up   = True
        toBool Down = False
        (a,g') = randomR (toBool mi, toBool ma) g
    in if a
         then (Up, g')
         else (Down, g')

type GridType = '[Periodic 60, Periodic 60]

-- | The four orthogonal neighbours of every site, worked out once for the
-- lattice /type/ rather than per site per sweep.
--
-- A top-level binding at a concrete type, so it is built the first time a site
-- is looked at and shared by every one after --- and a sweep is 3,600 sites,
-- with 'runSimulation' doing two energy evaluations per attempted flip. What
-- 'vonNeumannNeighbours' would otherwise redo on each of those is fixed by
-- 'GridType' alone: the axis sizes fix the strides and 'Periodic' fixes the
-- wrapping, so the answer cannot depend on the spins.
neighbourhood :: Stencil GridType
neighbourhood = vonNeumannStencil 1

gridSize :: Integer
gridSize = GHC.natVal (Proxy :: Proxy (MaxCoordSize GridType))

randomGrid ::
     (MonadRandom m, AllSizedKnown cs)
  => m (Grid cs Spin)
randomGrid = sequence $ pure getRandom

-- | The interaction energy of one site with its neighbours.
--
-- Takes the grid and the coordinate rather than a 'FocusedGrid' seeked to that
-- coordinate, which is what it used to do:
--
-- > singleEnergy PhysicalOptions{..} fg =
-- >   (-0.5) * coupling *
-- >   sum (map (\p -> spinNumber (peek p fg) * spinNumber (extract fg)) $
-- >        vonNeumannNeighbours 1 (pos fg))
--
-- Two things went with the focus. The neighbourhood is now read out of
-- 'neighbourhood', a precomputed table, instead of being enumerated through
-- 'Periodic''s wrapping arithmetic on every call. And @spinNumber (extract fg)@
-- --- the same site's own spin, constant across the sum --- is factored out of
-- it rather than recomputed per neighbour.
--
-- The 'FocusedGrid' was never doing any work here: 'peek' and 'extract' are
-- 'indexGrid' at a coordinate the caller already had, and 'seek' at every call was
-- rebuilding a focus that was then only asked what it was focused on.
singleEnergy ::
     PhysicalOptions
  -> Grid GridType Spin
  -> Coord GridType
  -> Double
singleEnergy PhysicalOptions {..} g c =
  (-0.5) * coupling * spinNumber (indexGrid g c) *
  sum (map spinNumber (stencilAt neighbourhood g c))

energyAtPoint ::
     IsGrid GridType (grid GridType)
  => PhysicalOptions
  -> grid GridType Spin
  -> Coord GridType
  -> Double
energyAtPoint po g c = singleEnergy po (g ^. asGrid) c

attempFlip ::
     (IsGrid GridType (grid GridType), MonadRandom m)
  => PhysicalOptions
  -> grid GridType Spin
  -> Coord GridType
  -> m (grid GridType Spin)
attempFlip po start c = do
  let startEnergy = energyAtPoint po start c
      newGrid = start & gridIndex c %~ flipSpin
      newEnergy = energyAtPoint po newGrid c
      acceptProb = min 1 $ exp (startEnergy - newEnergy)
  a :: Double <- getRandom
  return
    (if newEnergy >= startEnergy && a >= acceptProb
       then start
       else newGrid)

runSimulation ::
     forall m. MonadRandom m
  => PhysicalOptions
  -> Int
  -> Producer' (Grid GridType Spin) m ()
runSimulation po n =
  P.replicateM (n * fromIntegral gridSize) (getRandom :: m (Coord GridType)) >->
  P.scanM (attempFlip po) randomGrid pure >-> takeOneIn 100

takeOneIn :: Monad m => Int -> Pipe a a m ()
takeOneIn n = forever $ do
  a <- await
  replicateM_ (n - 1) await
  yield a

data SimulationState = SimulationState
    { _current              :: Grid GridType Spin
    , _stepPerTime          :: Float
    , _elapsedSinceLastStep :: Float
    , _gen                  :: StdGen
    } deriving (Show)
makeLenses ''SimulationState

displaySimulation :: PhysicalOptions -> SimulationState -> IO ()
displaySimulation po startSimulationState =
    let draw = ifoldMapOf (current . itraversed) drawHelper
        drawHelper p a =
            let c =
                    if a == Up
                        then red
                        else blue
                (x :^ y :^ NoDelta) = p .-. mempty
            in translate
                   (8 * fromIntegral x)
                   (8 * fromIntegral y)
                    $ color c (translate 1 1 $ rectangleSolid 8 8)
        update vp dt old
            | old ^. elapsedSinceLastStep + dt >= old ^. stepPerTime =
                -- P.last is Nothing only for an empty producer, which cannot
                -- happen here, but pattern-matching on Just would make that an
                -- unexplained crash rather than a dropped frame.
                let (newGrid, g') = runRand (P.last (runSimulation po 1)) (old ^. gen)
                in update vp (dt - old ^. stepPerTime) $
                   old & (current %~ \c -> fromMaybe c newGrid) &
                   (elapsedSinceLastStep -~ old ^. stepPerTime) &
                   (gen .~ g')
            | otherwise = old & elapsedSinceLastStep +~ dt
    in simulate
           (InWindow "floatMe" (800, 800) (1, 1))
           white
           60
           startSimulationState
           (translate (-350) (-350) . draw)
           update

main :: IO ()
main =
    let po = PhysicalOptions 10
    in do g <- newStdGen
          startGrid <- randomGrid
          displaySimulation po $
              SimulationState
              { _current = startGrid
              , _stepPerTime =0.5
              , _elapsedSinceLastStep = 0
              , _gen = g
              }
