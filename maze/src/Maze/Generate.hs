{-# LANGUAGE DataKinds #-}

-- | Maze generation by randomised depth-first search, with the carving head
-- as the /focus/ of a 'FocusedGrid'.
--
-- That is the whole point of doing it this way. The head is not a coordinate
-- the algorithm carries alongside a grid and remembers to keep in step; it is
-- the grid's own focus, so \"look two cells that way\" is 'traceTo', \"where
-- am I\" is 'pos', \"write here\" is @gridIndex (pos fg)@, and \"advance\" is
-- 'seek'. There is no index arithmetic in this module and no bounds test: a
-- direction is available exactly when a two-step walk from the focus lands
-- somewhere that is still rock, and on a 'Clamped' axis a walk that would
-- leave the board is 'Nothing' rather than a clamped lie.
--
-- The visited set is the maze. A cell is unvisited exactly when it is still
-- 'Wall', so nothing is tracked twice and the two cannot disagree.
module Maze.Generate
  ( carve,
  )
where

import Control.Comonad.Store (peek, pos, seek)
import Control.Lens ((&), (.~))
import Data.Grid.Sized
import Data.Maybe (listToMaybe)
import Maze.Model
import System.Random (StdGen, randomR)

-- | Carve a maze, as the moves it makes and the maze they leave behind.
--
-- The event list is produced lazily and the finished grid comes out at the
-- end of it, so a viewer can start drawing on the first move and a solver can
-- have the maze the moment the last one is consumed.
carve :: StdGen -> ([Move], Grid Cs Tile)
carve gen =
  let fg0 = FocusedGrid solidGrid startCell & gridIndex startCell .~ Floor
      (evs, final) = go gen fg0 []
   in (Opened startCell : Moved startCell : evs, final)
  where
    go g fg stack =
      case advance g fg of
        (g', Just (mid, dst)) ->
          let fg' =
                fg
                  & gridIndex mid .~ Floor
                  & gridIndex dst .~ Floor
                  & seek dst
              (evs, final) = go g' fg' (pos fg : stack)
           in (Opened mid : Opened dst : Moved dst : evs, final)
        (g', Nothing) ->
          case stack of
            [] -> ([], focusedGrid fg)
            (c : cs) ->
              let (evs, final) = go g' (seek c fg) cs
               in (Moved c : evs, final)

-- | Pick a direction to carve in: the wall to open and the cell beyond it, or
-- 'Nothing' if every direction is off the board or already carved.
advance ::
  StdGen ->
  FocusedGrid Cs Tile ->
  (StdGen, Maybe (Coord Cs, Coord Cs))
advance g fg =
  let (order, g') = shuffle g directions
      candidates =
        [ (mid, dst)
        | d <- order,
          -- Two steps that way is on the board and still rock.
          Just (dst, Wall) <- [traceTo (twoSteps d) fg],
          -- One step that way is the wall between, which the two-step walk
          -- above has already proved is on the board.
          Just mid <- [offsetCoord (pos fg) d],
          -- ...and it is rock too, which it must be: a carved wall means
          -- the cell beyond it was carved, and that one is Wall.
          peek mid fg == Wall
        ]
   in (g', listToMaybe candidates)

-- | Fisher-Yates on a four-element list.
shuffle :: StdGen -> [a] -> ([a], StdGen)
shuffle gen xs0 = go gen xs0 (length xs0)
  where
    go g _ 0 = ([], g)
    go g ys n =
      case randomR (0, n - 1) g of
        (i, g') ->
          case splitAt i ys of
            (before, y : after) ->
              case go g' (before ++ after) (n - 1) of
                (rest, g'') -> (y : rest, g'')
            -- @i@ is below @n@ and @n@ is the length of @ys@, so the
            -- right half of the split is never empty. GHC cannot see
            -- that, and an incomplete pattern here would be a crash
            -- rather than a warning.
            (_, []) -> ([], g')
