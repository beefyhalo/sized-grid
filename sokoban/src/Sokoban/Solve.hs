{-# LANGUAGE DataKinds #-}

-- | Breadth-first search over game states, which is enough at this size and
-- earns its place twice over.
--
-- It is what says a level is solvable, so a level that ships is a level
-- somebody has finished; and because it plays the game through
-- "Sokoban.Rules"\'s own 'move' rather than a second copy of the rules, a
-- change to the rules that breaks a level shows up as that level no longer
-- having a solution. There is no separate model of the surface here to drift
-- out of step with the real one.
--
-- Searched in 'ChartFrame'. The frame only renames the keys --- the same four
-- headings are available from every cell either way --- so a level solvable in
-- one frame is solvable in the other, by a sequence with some of its ups and
-- downs swapped.
module Sokoban.Solve
  ( Solution(..)
  , solveLevel
  , solveFrom
  ) where

import           Sokoban.Board
import           Sokoban.Rules

import           Data.Grid.Sized (Coord)

import           Data.Set        (Set)
import qualified Data.Set        as Set

-- | A solution, and what it cost.
data Solution = Solution
    { solutionMoves  :: [Dir]
    , solutionPushes :: !Int
    , solutionSeen   :: !Int
    -- ^ States expanded getting here, which is the number to look at when a
    -- level turns out to need a bigger budget than it should.
    } deriving (Eq, Show)

-- | What makes two states the same state. The player's parity is not in it:
-- in 'ChartFrame' nothing consults it, so two states that differ only there
-- have the same moves available and the same crates to push.
type Key w h = (Coord (Strip w h), Set (Coord (Strip w h)))

key :: Play w h -> Key w h
key p = (spotCoord (playPlayer p), playCrates p)

-- | The shortest sequence of key presses that finishes a level, or 'Nothing'
-- if there is none within the budget.
--
-- The budget is a number of states, not of moves: it bounds the search rather
-- than the answer, so exceeding it means \"not found in this much work\" and
-- not \"unsolvable\". A level whose answer needs a bigger budget than a
-- player would ever explore is a level to cut.
solveLevel :: KnownStrip w h => Int -> Level w h -> Maybe Solution
solveLevel budget lvl = solveFrom budget (newGame lvl)

-- | As 'solveLevel', from wherever a game has got to. What a hint would call.
solveFrom :: forall w h. KnownStrip w h => Int -> Game w h -> Maybe Solution
solveFrom budget game
    | solved game = Just (Solution [] (playPushes start) 0)
    | otherwise = bfs budget 0 (Set.singleton (key start)) [(start, [])]
  where
    start = gamePlay game
    -- One game to hang states off, its history dropped so the search does not
    -- carry every path it has ever taken.
    blank = game {gamePast = []}
    won p = solved blank {gamePlay = p}
    bfs :: Int -> Int -> Set (Key w h) -> [(Play w h, [Dir])] -> Maybe Solution
    bfs left expanded seen frontier
        | null frontier = Nothing
        | left <= 0 = Nothing
        | otherwise =
            case found of
                Just s -> Just s
                Nothing ->
                    bfs
                        (left - length frontier)
                        expanded'
                        seen'
                        (reverse next)
      where
        expanded' = expanded + length frontier
        (seen', next, found) = foldl' fromState (seen, [], Nothing) frontier
        fromState acc (p, path) = foldl' (try p path) acc allDirs
        try p path acc@(s, acc', hit) dir
            | not (outcomeMoved outcome) = acc
            | Set.member k s = acc
            | won p' =
                ( Set.insert k s
                , acc'
                , maybe
                      (Just
                           (Solution
                                (reverse path')
                                (playPushes p')
                                (expanded' + length acc')))
                      Just
                      hit)
            | otherwise = (Set.insert k s, (p', path') : acc', hit)
          where
            (stepped, outcome) = move ChartFrame dir blank {gamePlay = p}
            p' = gamePlay stepped
            path' = dir : path
            k = key p'
