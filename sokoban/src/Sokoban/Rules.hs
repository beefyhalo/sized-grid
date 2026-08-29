{-# LANGUAGE DataKinds #-}

-- | Sokoban's rules, on a surface that does not lie flat. No gloss in here,
-- and no drawing: this module is the game, and everything else is a way of
-- looking at it.
--
-- The rules themselves are the ones everybody knows. Walk onto floor. Walk
-- into a crate with an empty cell behind it and both of you move one on.
-- Every goal covered wins. What the strip changes is not any of those
-- sentences --- it is what \"one on\" means, and that is 'stepSpot's business
-- and not this module's.
--
-- == The push, which is the one thing worth reading closely
--
-- A push is two steps, and they are not the same step twice. The crate is one
-- cell ahead of the player, so when the pair is straddling the seam the crate
-- is already on the far side of it: the player's row and the crate's row are
-- mirror images, and no displacement added to the player's coordinate names
-- the cell the crate is going to.
--
-- What does name it is the heading that comes back /out/ of the player's own
-- step. 'stepSpot' returns the landing cell and the heading in the frame of
-- that cell, so
--
-- > (ahead,  ahead')  <- stepSpot here  heading
-- > (beyond, _)       <- stepSpot ahead ahead'
--
-- reads the surface twice in the frame it is in at the time, and the seam
-- lands where it lands. This is the entire accommodation the game makes for
-- the twist. There is no case for it.
module Sokoban.Rules
  ( -- * A level
    Level(..)
  , levelPlay
    -- * A game in progress
  , Play(..)
  , Game(..)
  , newGame
  , restart
    -- * Playing
  , Outcome(..)
  , outcomeMoved
  , outcomeName
  , move
  , replay
  , undo
  , solved
    -- * Asking about the board
  , tileAt
  , crateAt
  , occupied
  , goalsLeft
  ) where

import           Sokoban.Board

import           Data.Grid.Sized

import           Data.Set               (Set)
import qualified Data.Set               as Set

-- | A level: its terrain, its goals, and where everything starts.
--
-- The goals are held twice --- as 'Goal' tiles in the board and as a set here
-- --- because the two are asked different questions. The board is asked what
-- is under this one cell, for drawing; the set is asked whether every goal is
-- covered, for winning, and that is a subset test rather than a sweep of the
-- grid.
data Level w h = Level
    { levelName   :: String
    , levelNote   :: String
    -- ^ The one thing this level is teaching. Empty for a level that is
    -- teaching nothing, which is a level that should probably not ship.
    , levelBoard  :: Board w h
    , levelGoals  :: Set (Coord (Strip w h))
    , levelStart  :: Play w h
    }

-- | The part of a game that changes: everything a move rewrites and an undo
-- puts back.
--
-- The counters are in here rather than beside it precisely so that undo
-- restores them too. A move count that survives its own move being taken back
-- is a different kind of number from the one the player is trying to make
-- small.
data Play w h = Play
    { playPlayer  :: !(Spot w h)
    , playFlipped :: !Bool
    -- ^ Whether the player has crossed the seam an odd number of times, and
    -- so is upside down with respect to the chart. Only 'PlayerFrame' reads
    -- it, but it is state of the game and not of the view: it is a fact about
    -- where the player has been, and undo has to put it back.
    , playFacing  :: !Heading
    -- ^ Which way the last move pointed, for drawing. Not consulted by any
    -- rule --- a Sokoban pushes by walking into a crate, so facing is never
    -- an input --- but on this surface the player needs to see which way they
    -- are about to go, because it decides whether the next step is through
    -- the seam.
    --
    -- A 'Heading' and not the 'Dir' that was pressed, because a heading names
    -- an axis of the chart and a key press does not: the same key means
    -- different headings on the two sides of the seam. A view that wants the
    -- key back asks 'dirOf' for it, in the frame it is drawing in.
    , playCrates  :: !(Set (Coord (Strip w h)))
    , playMoves   :: !Int
    , playPushes  :: !Int
    } deriving (Eq)

-- | Standalone, because 'Show' for a coordinate wants to know the axis sizes
-- and 'Eq' does not: a coordinate is its row-major position, so equality is
-- an 'Int' comparison, and printing one is not.
deriving instance KnownStrip w h => Show (Play w h)

-- | A level being played: the level itself, the current state, and every
-- state before it.
--
-- Undo is a stack of past states rather than an inverse move, and on this
-- surface that is not laziness. Inverting a move means inverting a seam
-- crossing, which means getting the mirror the right way round in the
-- direction nothing ever walks. Keeping the old state costs a few words and
-- cannot be wrong.
data Game w h = Game
    { gameLevel :: Level w h
    , gamePlay  :: !(Play w h)
    , gamePast  :: [Play w h]
    }

levelPlay :: Level w h -> Play w h
levelPlay = levelStart

newGame :: Level w h -> Game w h
newGame lvl = Game {gameLevel = lvl, gamePlay = levelStart lvl, gamePast = []}

-- | Back to the first move, keeping nothing. Not an undo of every move: the
-- history goes too, so a restart cannot be walked back into.
restart :: Game w h -> Game w h
restart = newGame . gameLevel

-- | What a key press did.
--
-- The two refusals are kept apart on purpose. 'OffTheStrip' is the surface
-- running out --- the 'Straight' axis, the one edge a Mobius strip genuinely
-- has --- and 'BlockedByWall' is terrain. They look identical to a player
-- staring at the boundary of a rectangle, and telling them apart is most of
-- what makes the strip legible: the left and right edges are not edges at
-- all, and the top and bottom are.
data Outcome
    = Walked
    | Pushed
    | BlockedByWall
    | BlockedByCrate
    | OffTheStrip
    deriving (Eq, Show)

-- | Did this outcome change the game?
outcomeMoved :: Outcome -> Bool
outcomeMoved Walked = True
outcomeMoved Pushed = True
outcomeMoved _      = False

outcomeName :: Outcome -> String
outcomeName Walked         = "walked"
outcomeName Pushed         = "pushed"
outcomeName BlockedByWall  = "a wall"
outcomeName BlockedByCrate = "a crate with no room behind it"
outcomeName OffTheStrip    = "the edge of the strip"

tileAt :: KnownStrip w h => Game w h -> Spot w h -> Tile
tileAt g = boardTile (levelBoard (gameLevel g))

crateAt :: Play w h -> Spot w h -> Bool
crateAt p s = Set.member (spotCoord s) (playCrates p)

-- | Is there anything in this cell that stops a crate entering it?
occupied :: KnownStrip w h => Game w h -> Play w h -> Spot w h -> Bool
occupied g p s = not (walkable (tileAt g s)) || crateAt p s

-- | Goals with no crate on them.
goalsLeft :: Game w h -> Int
goalsLeft g =
    Set.size (levelGoals (gameLevel g) `Set.difference` playCrates (gamePlay g))

solved :: Game w h -> Bool
solved g = levelGoals (gameLevel g) `Set.isSubsetOf` playCrates (gamePlay g)

-- | Press a direction key.
--
-- The game comes back changed only if the move happened; the outcome says
-- what happened either way, so a refusal can be shown as the reason it was
-- refused rather than as nothing at all.
move :: KnownStrip w h => Frame -> Dir -> Game w h -> (Game w h, Outcome)
move frame dir g =
    case stepSpot here heading of
        Nothing -> (g, OffTheStrip)
        Just (ahead, ahead')
            | not (walkable (tileAt g ahead)) -> (g, BlockedByWall)
            | crateAt play ahead ->
                case stepSpot ahead ahead' of
                    Nothing -> (g, BlockedByCrate)
                    Just (beyond, _)
                        | occupied g play beyond -> (g, BlockedByCrate)
                        | otherwise ->
                            ( commit
                                  play
                                  { playCrates =
                                        Set.insert (spotCoord beyond)
                                            (Set.delete (spotCoord ahead) (playCrates play))
                                  , playPushes = playPushes play + 1
                                  }
                                  ahead
                            , Pushed)
            | otherwise -> (commit play ahead, Walked)
  where
    play = gamePlay g
    here = playPlayer play
    heading = headingFor frame (playFlipped play) dir
    -- The parity is flipped from the step that was about to be taken, not
    -- from the one that was: 'crossesSeam' asks about a heading leaving a
    -- cell. See its note --- this bit is one 'mobiusStep' already computed
    -- and did not hand back.
    crossed = crossesSeam here heading
    commit p landed =
        g { gamePlay =
                p { playPlayer = landed
                  , playFlipped = playFlipped p /= crossed
                  , playFacing = heading
                  , playMoves = playMoves p + 1
                  }
          , gamePast = play : gamePast g
          }

-- | Press a run of keys, and say what each one did. What a solver's answer
-- is checked with, and what a recorded solution is played back through.
replay ::
       KnownStrip w h
    => Frame
    -> [Dir]
    -> Game w h
    -> (Game w h, [Outcome])
replay frame dirs g0 = fmap reverse (foldl' one (g0, []) dirs)
  where
    one (g, outs) dir =
        let (g', o) = move frame dir g
        in (g', o : outs)

-- | Take back one move. 'Nothing' at the start of the level, so a caller can
-- say so rather than silently doing nothing.
undo :: Game w h -> Maybe (Game w h)
undo g =
    case gamePast g of
        []     -> Nothing
        (p:ps) -> Just g {gamePlay = p, gamePast = ps}
