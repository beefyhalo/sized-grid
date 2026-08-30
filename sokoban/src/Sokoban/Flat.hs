-- | The same level, played on a surface that lies flat.
--
-- This is the test the epic asks every level to pass: would it still be
-- interesting on a flat board? If it would, the topology is decoration.
-- Answering that by eye does not work --- a level can /look/ as though it
-- needs the seam and be solvable by an ordinary route nobody noticed --- so it
-- is answered by solving the same layout again on each of the flat surfaces
-- of the same shape:
--
--   * 'Rectangle', with walls where the surface has its seams. If a level is
--     solvable here, none of the gluing is doing anything.
--
--   * 'Cylinder', wrapped sideways but /without/ the half turn. If a level is
--     solvable here but not on a rectangle, the wrap is load bearing and the
--     twist is not --- and the level belongs in a game about cylinders.
--
--   * 'Torus', wrapped both ways and still without a turn anywhere.
--
-- A level solvable on its own surface and on none of these is a level whose
-- answer is the gluing. That is the only kind worth shipping as a headline,
-- and 'verdict' is what says which kind a level is.
--
-- == Only fair comparisons
--
-- Which of the three a level is compared against depends on the surface it is
-- played on, and getting that wrong is not a detail. A torus joins its top to
-- its bottom; a Mobius strip does not join anything to anything there, because
-- that is where its edge is. Offering a Mobius level a torus hands the solver
-- a route the level's own surface has never had, and it will take it: the
-- third built-in level is solvable on a torus in two pushes and needs a lap of
-- the seam on the strip it is actually played on.
--
-- So a comparison is only offered when it glues no more than the level's own
-- surface does, which 'surfaceEdged' is enough to decide --- an edge is a pair
-- of sides that is joined to nothing, and a torus joins every pair.
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
  ( Comparison (..),
    comparisonName,
    comparisonsFor,
    solvableOn,
    Verdict (..),
    verdict,
    verdictLine,
  )
where

import Control.Applicative ((<|>))
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set
import Sokoban.Board (Surface, surfaceEdged)
import Sokoban.Level (Layout (..))

-- | A flat surface of the same shape as the level's own, in order of how
-- little topology it needs.
--
-- The order is load bearing: every move available on a rectangle is available
-- on a cylinder, and every move on a cylinder is available on a torus, so a
-- layout solvable on one is solvable on all the later ones and the /first/ one
-- that solves it is the weakest claim that can be made about the level.
data Comparison
  = -- | No wrap at all: every side has a wall beyond it.
    Rectangle
  | -- | Wrapped sideways, no half turn: leaving the right of row @y@ arrives
    -- at the left of row @y@.
    Cylinder
  | -- | Wrapped both ways, and still no half turn anywhere.
    Torus
  deriving (Eq, Show, Enum, Bounded)

comparisonName :: Comparison -> String
comparisonName Rectangle = "a plain rectangle"
comparisonName Cylinder = "a cylinder"
comparisonName Torus = "a torus"

-- | The flat surfaces it is fair to judge a level on this surface against: the
-- ones that glue no more than it does. See the module header.
comparisonsFor :: Surface -> [Comparison]
comparisonsFor surface
  | surfaceEdged surface = [Rectangle, Cylinder]
  | otherwise = [Rectangle, Cylinder, Torus]

-- | Where a step from @(x, y)@ lands, or 'Nothing' if it leaves the surface.
step :: Comparison -> Int -> Int -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
step surface w h (dx, dy) (x, y) =
  case surface of
    Rectangle -> (,) <$> inside w x' <*> inside h y'
    Cylinder -> (,) (x' `mod` w) <$> inside h y'
    Torus -> Just (x' `mod` w, y' `mod` h)
  where
    x' = x + dx
    y' = y + dy
    inside n i
      | i < 0 || i >= n = Nothing
      | otherwise = Just i

-- | The fewest pushes that finish this layout on the given surface, or
-- 'Nothing' if there is no solution within the budget.
--
-- Counted in pushes rather than moves so the number is comparable with
-- 'Sokoban.Solve.solutionPushes': the two searches walk different graphs, and
-- the move counts are not the same question.
solvableOn :: Comparison -> Int -> Layout -> Maybe Int
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
                  (Set.insert (p', crates') s0, acc0, hit0 <|> Just pushes')
              | otherwise = (Set.insert (p', crates') s0, st : acc0, hit0)

-- | What kind of level this is: the weakest flat surface of the same shape
-- that can also solve it, and in how many pushes.
--
-- 'Nothing' is the answer a level wants. It says the level cannot be finished
-- without a half turn somewhere, which is the whole of what makes it a level
-- about a surface rather than a level that happens to be drawn on one.
newtype Verdict = Verdict (Maybe (Comparison, Int))
  deriving (Eq, Show)

-- | The surface is passed in rather than read off the 'Layout' because a
-- caller with a level in hand already has it, and a layout re-read from a
-- level's own picture has lost it: 'Sokoban.Level.levelPicture' writes the
-- cells and not the header.
verdict :: Surface -> Int -> Layout -> Verdict
verdict surface budget lay =
  Verdict $
    listToMaybe
      [ (c, n)
      | c <- comparisonsFor surface,
        Just n <- [solvableOn c budget lay]
      ]

-- | The verdict as a sentence.
verdictLine :: Surface -> Verdict -> String
verdictLine surface (Verdict Nothing) =
  "NEEDS THE TURN: no solution on "
    ++ list (map comparisonName (comparisonsFor surface))
  where
    list [] = "any flat surface"
    list [one] = one
    list xs = intercalate ", " (init xs) ++ " or " ++ last xs
verdictLine
  _
  (Verdict (Just (c, n))) =
    case c of
      Rectangle ->
        "flat: solvable on a plain rectangle in "
          ++ show n
          ++ " pushes -- none of the gluing is doing anything"
      Cylinder ->
        "cylinder: solvable wrapped sideways without a turn in "
          ++ show n
          ++ " pushes -- the wrap is load bearing, the turn is not"
      Torus ->
        "torus: solvable wrapped both ways without a turn in "
          ++ show n
          ++ " pushes -- the wraps are load bearing, the turns are not"
