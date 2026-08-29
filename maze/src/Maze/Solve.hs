{-# LANGUAGE DataKinds #-}

-- | Breadth-first search from one corner of the maze to the other, emitted a
-- layer at a time.
--
-- The walking is the same as the generator's and for the same reason: a
-- neighbour is 'offsetCoord' in one of the four 'directions', and off the
-- board is 'Nothing' rather than a coordinate clamped back onto the edge. So
-- the only test in the expansion is \"is that a floor cell I have not seen\",
-- and nothing here compares an index against 61.
module Maze.Solve
  ( solve
  ) where

import           Maze.Model

import           Data.Grid.Sized

import           Control.Lens    ((&), (.~))
import           Data.Maybe      (isNothing)

-- | How a cell was first reached, which is what turns the search into a route.
data Prev
    = Root
    | From (Coord Cs)

-- | The search, as the layers it reaches and the route it ends with.
--
-- Lazy layer by layer, like the generator, so the viewer can draw the flood
-- spreading rather than waiting for an answer.
solve :: Grid Cs Tile -> [Move]
solve maze
    | indexGrid maze startCell == Wall || indexGrid maze goalCell == Wall =
        [Unreachable]
    | otherwise = go seen0 [startCell] 0
  where
    seen0 :: Grid Cs (Maybe Prev)
    seen0 = tabulateGrid (const Nothing) & gridIndex startCell .~ Just Root
    go :: Grid Cs (Maybe Prev) -> [Coord Cs] -> Int -> [Move]
    go seen level depth
        | null level = [Unreachable]
        | goalCell `elem` level =
            map (`Reached` depth) level ++ [Solved (routeTo seen goalCell)]
        | otherwise =
            let (seen', next) = expand seen level
            in map (`Reached` depth) level ++ go seen' next (depth + 1)
    expand :: Grid Cs (Maybe Prev) -> [Coord Cs] -> (Grid Cs (Maybe Prev), [Coord Cs])
    expand seen level = foldl step (seen, []) level
      where
        step (s, acc) c =
            let fresh =
                    [ n
                    | d <- directions
                    , Just n <- [offsetCoord c d]
                    , indexGrid maze n == Floor
                    , isNothing (indexGrid s n)
                    ]
            -- 'fresh' is recomputed against the grid as it is written, so a
            -- cell two cells of this level both border is claimed by the
            -- first of them and not queued twice.
            in foldl
                   (\(s', acc') n -> (s' & gridIndex n .~ Just (From c), n : acc'))
                   (s, acc)
                   fresh
    routeTo :: Grid Cs (Maybe Prev) -> Coord Cs -> [Coord Cs]
    routeTo seen goal = reverse (walk goal)
      where
        walk c =
            case indexGrid seen c of
                Just Root       -> [c]
                Just (From p)   -> c : walk p
                -- Unreachable: 'routeTo' is only called on a cell the search
                -- has just put in a layer, and every such cell has a 'Prev'.
                Nothing         -> [c]
