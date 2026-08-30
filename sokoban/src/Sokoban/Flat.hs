-- | The same level, played on a surface that lies flat.
--
-- This is the test the epic asks every level to pass: would it still be
-- interesting on a flat board? If it would, the topology is decoration.
-- Answering that by eye does not work --- a level can /look/ as though it
-- needs the seam and be solvable by an ordinary route nobody noticed --- so it
-- is answered by solving the same layout twice more, on the two flat surfaces
-- of the same shape:
--
--   * 'Rectangle', with walls where the strip has its seam. If a level is
--     solvable here, neither the wrap nor the twist is doing anything.
--
--   * 'Cylinder', wrapped the same way around but /without/ the half turn. If
--     a level is solvable here but not on a rectangle, the wrap is load
--     bearing and the twist is not --- and the level belongs in a game about
--     cylinders.
--
-- A level that is solvable on the strip and on neither of these is a level
-- whose answer is the twist. That is the only kind worth shipping as a
-- headline, and 'verdict' is what says which kind a level is.
--
-- == Why this is written out again rather than reusing the rules
--
-- "Sokoban.Solve" plays through "Sokoban.Rules" on purpose, so that a change
-- to the rules shows up as a level that stops having a solution. Here the
-- opposite is wanted: an /independent/ implementation, so that agreement
-- between the two is evidence rather than a tautology. The stepping below is
-- forty lines of @Int@ arithmetic that knows nothing about grid-sized, and if
-- it and the strip ever disagree about a level that should be flat-solvable,
-- one of them is wrong and it is worth finding out which.
module Sokoban.Flat
  ( Surface (..),
    surfaceName,
    solvableOn,
    Verdict (..),
    verdict,
    verdictLine,
  )
where

import Data.Set qualified as Set
import Sokoban.Level (Layout (..))

-- | A surface of the same shape as the level's strip that is not a Mobius
-- strip.
data Surface
  = -- | No wrap at all: the left and right columns have walls beyond them.
    Rectangle
  | -- | Wrapped around, no half turn: leaving the right of row @y@ arrives at
    -- the left of row @y@.
    Cylinder
  deriving (Eq, Show, Enum, Bounded)

surfaceName :: Surface -> String
surfaceName Rectangle = "a plain rectangle"
surfaceName Cylinder = "a cylinder"

-- | Where a step from @(x, y)@ lands, or 'Nothing' if it leaves the surface.
step :: Surface -> Int -> Int -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
step surface w h (dx, dy) (x, y)
  | y' < 0 || y' >= h = Nothing
  | dx == 0 = Just (x, y')
  | otherwise =
      case surface of
        Rectangle
          | x' < 0 || x' >= w -> Nothing
          | otherwise -> Just (x', y')
        Cylinder -> Just (x' `mod` w, y')
  where
    x' = x + dx
    y' = y + dy

-- | The fewest pushes that finish this layout on the given surface, or
-- 'Nothing' if there is no solution within the budget.
--
-- Counted in pushes rather than moves so the number is comparable with
-- 'Sokoban.Solve.solutionPushes': the two searches walk different graphs, and
-- the move counts are not the same question.
solvableOn :: Surface -> Int -> Layout -> Maybe Int
solvableOn surface budget lay
  | won crates0 = Just 0
  | otherwise = bfs budget (Set.singleton (player0, crates0)) [(player0, crates0, 0)]
  where
    w = layoutWidth lay
    h = layoutHeight lay
    walls = layoutWalls lay
    goals = layoutGoals lay
    player0 = layoutPlayer lay
    crates0 = layoutCrates lay
    won crates = goals `Set.isSubsetOf` crates
    free crates c = not (Set.member c walls) && not (Set.member c crates)
    dirs = [(1, 0), (-1, 0), (0, 1), (0, -1)]
    bfs left seen frontier
      | left <= 0 || null frontier = Nothing
      | otherwise =
          case found of
            Just n -> Just n
            Nothing -> bfs (left - length frontier) seen' (reverse next)
      where
        (seen', next, found) = foldl' fromState (seen, [], Nothing) frontier
        fromState acc st = foldl' (try st) acc dirs
        try (p, crates, pushes) acc d =
          case step surface w h d p of
            Nothing -> acc
            Just ahead
              | Set.member ahead walls -> acc
              | Set.member ahead crates ->
                  case step surface w h d ahead of
                    Nothing -> acc
                    Just beyond
                      | not (free crates beyond) -> acc
                      | otherwise ->
                          visit
                            acc
                            ( ahead,
                              Set.insert beyond (Set.delete ahead crates),
                              pushes + 1
                            )
              | otherwise -> visit acc (ahead, crates, pushes)
          where
            visit (s0, acc0, hit0) st@(p', crates', pushes')
              | Set.member (p', crates') s0 = (s0, acc0, hit0)
              | won crates' =
                  (Set.insert (p', crates') s0, acc0, maybe (Just pushes') Just hit0)
              | otherwise = (Set.insert (p', crates') s0, st : acc0, hit0)

-- | What kind of level this is: which of the flat surfaces of the same shape
-- can also solve it.
data Verdict = Verdict
  { verdictOnRectangle :: Maybe Int,
    verdictOnCylinder :: Maybe Int
  }
  deriving (Eq, Show)

verdict :: Int -> Layout -> Verdict
verdict budget lay =
  Verdict
    { verdictOnRectangle = solvableOn Rectangle budget lay,
      verdictOnCylinder = solvableOn Cylinder budget lay
    }

-- | The verdict as a sentence, strongest claim first.
verdictLine :: Verdict -> String
verdictLine (Verdict rect cyl) =
  case (rect, cyl) of
    (Just n, _) ->
      "flat: solvable on a plain rectangle in "
        ++ show n
        ++ " pushes -- neither the wrap nor the twist is doing anything"
    (Nothing, Just n) ->
      "cylinder: solvable wrapped without the twist in "
        ++ show n
        ++ " pushes -- the wrap is load bearing, the twist is not"
    (Nothing, Nothing) -> "MOBIUS: no solution on a rectangle or a cylinder"
