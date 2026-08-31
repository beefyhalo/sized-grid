{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

-- | The surface the game is played on, and the vocabulary the rules are
-- written in: what a cell holds, which way a key points, and where a step from
-- here lands.
--
-- A surface is one chart of @'Clamped' w x 'Clamped' h@ glued to itself, and
-- which gluing is a 'Surface', chosen by the level. Every step goes through
-- 'stepSpot', so the gluing is never something this game computes. It is
-- consulted.
--
-- Three of them, and grid-atlas carries all three (sized-grid-lopy.7):
--
--   * a 'mobius' strip, glued along its width with a half twist. Its other
--     axis is a genuine edge, and it is the only one of the three with one.
--
--   * a 'klein' bottle, glued along both axes, one of them with the twist.
--     Nothing has an edge, so a step is never refused by the surface.
--
--   * a 'projective' plane, glued along both axes with a twist on each.
--
-- Nothing above this module knows which. The rules, the solver, the level
-- format and every view are written against 'Surface' rather than against a
-- strip, and the only thing that ever names a particular one is a level saying
-- which it wants.
--
-- == The two things that follow from a twist
--
--   * A lap does not return you to where you started. Crossing a mirrored
--     seam reflects the /other/ axis --- leaving sideways through a Mobius
--     strip's seam sends @(u, v)@ to @(u, h-1-v)@ --- so one lap lands on the
--     mirrored row and it takes two to close.
--
--   * That reflection turns the walker over. 'PlayerFrame' says so;
--     'ChartFrame' does not, and the difference between them is the whole of
--     what 'Frame' is for.
--
-- The second is a bit the game has to carry, and it comes out of the step:
-- 'stepSpot' returns a 'Crossing', and 'reversedFrame' is 'True' for exactly
-- the crossings that turn the walker over. It did not always. This module used
-- to reconstruct it with a bounds test against the strip's width, which was
-- wrong-headed twice over --- it is arithmetic the library exists to remove,
-- and it only worked because on that one surface every seam crossing happens
-- to mirror. sized-grid-lopy.5.
--
-- == Why the walker's frame is two bits and not one
--
-- 'reversedFrame' is one bit, and it is the right one: it says whether the
-- walker's left hand has changed sides, which is all a question about
-- /orientation/ needs. It is not enough to read direction keys with.
--
-- A mirrored crossing along one axis reverses the walker's view of the
-- other. On a Mobius strip and on a Klein bottle only the sideways seam ever
-- mirrors, so only up and down are ever reversed and one bit says everything.
-- On a projective plane both seams mirror, so a sideways crossing swaps up
-- with down and an up-or-down crossing swaps left with right --- and which of
-- those happened is not in 'reversedFrame'. It /is/ recoverable, from the axis
-- the step was made along, and 'turnAfter' is where that is done. See 'Turn'.
module Sokoban.Board
  ( -- * The chart
    Strip,
    KnownStrip,
    Spot,
    spotAt,
    spotXY,
    spotCoord,
    stripSpots,
    stripSize,

    -- * Which surface it is glued into
    Surface (..),
    surfaces,
    surfaceNamed,
    mobius,
    klein,
    projective,

    -- * What a cell holds
    Tile (..),
    walkable,
    Board,
    boardTile,
    boardFromGrid,

    -- * Pointing
    Dir (..),
    allDirs,
    dirName,
    Frame (..),
    Turn (..),
    square,
    turnAfter,
    turnNote,
    Heading (..),
    Axis (..),
    headingFor,
    dirOf,

    -- * Stepping
    Crossing (..),
    crossedSeam,
    reversedFrame,
    stepSpot,
    stepDir,
    walkFrom,
    cellAround,
    spotBeyond,
  )
where

import Control.Monad (foldM)
import Data.Grid.Atlas
import Data.Grid.Atlas.Klein (kleinAtlas, kleinStep)
import Data.Grid.Atlas.Mobius (mobiusAtlas, mobiusStep)
import Data.Grid.Atlas.Projective (projectiveAtlas, projectiveStep)
import Data.Grid.Atlas.Rect
-- 'Frame' hidden: this module has its own 'Frame' (chart vs. player), older
-- than the library's accumulated-reflection 'Frame' (sized-grid-dse0).
-- sized-grid-t8rw folds this hand-rolled 'Turn' \/ 'Frame' into that type.
import Data.Grid.Sized hiding (Frame)
import Data.List (find)
import GHC.TypeLits (KnownNat, type (<=))

-- | The chart: a rectangle @w@ cells one way and @h@ cells the other.
--
-- 'Clamped' on both axes, even for the axes that wrap. A wrap is not a
-- property of an axis here: it is a gluing between the chart's own ends that
-- may reverse the other axis on the way through, and no separable per-axis
-- policy can say that. Saying it is what the atlas layer is for. So each axis
-- is clamped --- a step off either end of the chart is a step out of the chart
-- --- and the 'Surface' decides which of those ends has something on the far
-- side.
type Strip w h = '[Clamped w, Clamped h]

-- | What every function here needs to know about a board's size. Both bounds
-- come from 'Clamped' itself, which has no zero-length axis.
type KnownStrip w h = (KnownNat w, KnownNat h, 1 <= w, 1 <= h)

-- | A cell of the surface.
--
-- An 'AtlasCoord' at @k = 1@, so the chart component is always the single
-- 'Ordinal' @1@ and carries no information --- see 'spotAt'. It is kept rather
-- than projected away because it is what the surface steppers take and return,
-- and unwrapping at every call site to rewrap at every next one would be a
-- translation layer that buys nothing.
type Spot w h = AtlasCoord (Strip w h) 1

-- | The cell at column @x@ and row @y@, or 'Nothing' if either is off the
-- chart.
spotAt :: forall w h. (KnownStrip w h) => Int -> Int -> Maybe (Spot w h)
spotAt x y = do
  u <- numToOrdinal x
  v <- numToOrdinal y
  pure (minBound, Clamped u :| Clamped v :| EmptyCoord)

-- | A cell as @(column, row)@ --- along the picture first, up it second.
--
-- The chart component carries nothing at @k = 1@, so this is 'coordIndices2'
-- of the chart-local coordinate: no unwrapping of 'Clamped' to
-- 'Data.Grid.Sized.Ordinal.Ordinal' to 'Int' to ask where a cell is
-- (sized-grid-bzzy).
spotXY :: (KnownStrip w h) => Spot w h -> (Int, Int)
spotXY = coordIndices2 . spotCoord

-- | The chart-local coordinate, which is all of a 'Spot' that distinguishes
-- one cell from another at @k = 1@. What crate sets and goal sets are keyed
-- by.
spotCoord :: Spot w h -> Coord (Strip w h)
spotCoord = snd

-- | The board's size, as @(across the picture, up it)@. Named with a type
-- application --- @stripSize \@w \@h@ --- since there is no value to read it
-- off.
stripSize :: forall w h. (KnownStrip w h) => (Int, Int)
stripSize = (ordinalSize @w, ordinalSize @h)

-- | Every cell of the surface, once each.
stripSpots :: forall w h. (KnownStrip w h) => [Spot w h]
stripSpots = [(minBound, c) | c <- allCoord]

-- * Surfaces

-- | How the chart's four edges are glued, and the little a caller needs to
-- know about the result.
--
-- Four things, which is what sized-grid-lopy.7 predicted it would take: a
-- name, a sentence, whether the surface has an edge, and the step. Everything
-- else in the game --- move, push, undo, win, the solver, the level format,
-- every view --- was already written without saying anything about the seam,
-- so nothing else had to change to gain three topologies.
--
-- 'surfaceAtlas' is a fourth field that earns its place by being honest rather
-- than by being different: all three build the same one-chart atlas, because
-- an 'Atlas' at @k = 1@ carries storage and not gluing. Handing each surface
-- its own is what keeps that a fact about today rather than an assumption.
data Surface = Surface
  { -- | What a level writes to ask for this surface. One word, lower case,
    -- and part of the level format rather than prose --- 'surfaceTitle' is
    -- what a sentence uses.
    surfaceName :: String,
    -- | The surface's name in a sentence, article and all.
    surfaceTitle :: String,
    -- | What the gluing is, in a sentence, for a level that wants to say.
    surfaceNote :: String,
    -- | Whether any step can leave the surface. Only a Mobius strip has an
    -- edge; on the other two 'stepSpot' never answers 'Nothing', and a rule
    -- written for the edge is dead code rather than a wrong answer.
    surfaceEdged :: Bool,
    -- | Whether "Sokoban.Band"'s twisted band is a picture of this surface.
    --
    -- Only one of the three, and it is not a rendering detail dressed up as a
    -- fact about the surface: a Mobius strip embeds in space, and a Klein
    -- bottle and a projective plane do not --- the best either has is an
    -- immersion that passes through itself, which is a different picture and a
    -- different piece of geometry.
    surfaceIsBand :: Bool,
    surfaceStep ::
      forall w h.
      (KnownStrip w h) =>
      Spot w h ->
      Heading ->
      Maybe (Spot w h, Heading, Crossing),
    surfaceAtlas :: forall w h a. Grid (Strip w h) a -> Atlas (Strip w h) 1 a
  }

-- | By name, since the rest of a 'Surface' is functions and there are only
-- ever the three in 'surfaces'. Both instances exist so that a 'Layout', which
-- carries the surface a level asked for, can keep its derived ones.
instance Eq Surface where
  a == b = surfaceName a == surfaceName b

-- | The name, which is also what a level writes to ask for one. Not a valid
-- expression for the value, and could not be.
instance Show Surface where
  showsPrec _ = showString . surfaceName

-- | Every surface a level may ask for, in the order a player should meet
-- them.
surfaces :: [Surface]
surfaces = [mobius, klein, projective]

surfaceNamed :: String -> Maybe Surface
surfaceNamed n = find ((== n) . surfaceName) surfaces

-- | Glued along its width with a half turn, and along nothing else.
mobius :: Surface
mobius =
  Surface
    { surfaceName = "mobius",
      surfaceTitle = "a Mobius strip",
      surfaceNote =
        "The left and right edges of the picture are the same edge, joined\
        \ with a half turn. Leave one and you arrive at the other in the row\
        \ on the far side of the middle, and your own up is now down. The top\
        \ and bottom are real edges and they stop you.",
      surfaceEdged = True,
      surfaceIsBand = True,
      surfaceStep = mobiusStep,
      surfaceAtlas = mobiusAtlas
    }

-- | Glued along both axes, one of them with a half turn. No edges at all.
klein :: Surface
klein =
  Surface
    { surfaceName = "klein",
      surfaceTitle = "a Klein bottle",
      surfaceNote =
        "Every edge is glued to another one, so there is nowhere to fall off.\
        \ Left and right join with a half turn, the way a strip does. Top and\
        \ bottom join straight through, the way a cylinder does.",
      surfaceEdged = False,
      surfaceIsBand = False,
      surfaceStep = total kleinStep,
      surfaceAtlas = kleinAtlas
    }

-- | Glued along both axes, each with a half turn. No edges either, and no way
-- to cross a seam without being turned over.
projective :: Surface
projective =
  Surface
    { surfaceName = "projective",
      surfaceTitle = "a projective plane",
      surfaceNote =
        "Both pairs of edges join with a half turn, and there is no edge\
        \ anywhere. Leaving sideways swaps your up with your down; leaving\
        \ upwards swaps your left with your right.",
      surfaceEdged = False,
      surfaceIsBand = False,
      surfaceStep = total projectiveStep,
      surfaceAtlas = projectiveAtlas
    }

-- | A surface with no edge steps in no 'Applicative' at all, and this is where
-- that is given up. 'stepSpot' answers in 'Maybe' for every surface, because
-- the rules have to be written once and one of the three genuinely can refuse.
total ::
  (Spot w h -> Heading -> (Spot w h, Heading, Crossing)) ->
  Spot w h ->
  Heading ->
  Maybe (Spot w h, Heading, Crossing)
total f s hd = Just (f s hd)

-- * The board

-- | What is built into a cell. Crates are not here: they move, and what moves
-- is state rather than terrain.
data Tile
  = Wall
  | Floor
  | Goal
  deriving (Eq, Show)

-- | A cell a player or a crate may occupy, crates aside.
walkable :: Tile -> Bool
walkable Wall = False
walkable Floor = True
walkable Goal = True

-- | The terrain, as the atlas it is played on.
--
-- One chart, so this is a 'Grid' in a wrapper that adds nothing to the storage
-- --- but a step answers in 'AtlasCoord', and reading a cell with 'atlasIndex'
-- at the same coordinate is what keeps the two halves in one vocabulary.
type Board w h = Atlas (Strip w h) 1 Tile

-- | Applied rather than point-free, here and at 'stepSpot', and it has to be:
-- the field is polymorphic, so the selector must be given its record for the
-- field's own foralls to instantiate. hlint sees only the shape.
{-# ANN boardFromGrid ("HLint: ignore Eta reduce" :: String) #-}
boardFromGrid :: Surface -> Grid (Strip w h) Tile -> Board w h
boardFromGrid surface grid = surfaceAtlas surface grid

boardTile :: (KnownStrip w h) => Board w h -> Spot w h -> Tile
boardTile = atlasIndex

-- * Pointing

-- | A key press, in whichever frame it is being read in. Not a 'Heading': a
-- heading names an axis of the chart, and up is not an axis of the chart once
-- the player has crossed a mirrored seam an odd number of times.
data Dir
  = DirLeft
  | DirRight
  | DirUp
  | DirDown
  deriving (Eq, Show, Enum, Bounded)

allDirs :: [Dir]
allDirs = [minBound .. maxBound]

dirName :: Dir -> String
dirName DirLeft = "left"
dirName DirRight = "right"
dirName DirUp = "up"
dirName DirDown = "down"

-- | Which frame the direction keys are read in.
--
-- The choice only exists because these surfaces are not orientable, and it is
-- a real choice rather than a preference:
--
--   * 'ChartFrame' reads up as the chart's up, always. The board is then a
--     rectangle whose edges are stitched with known mirrors, and the player
--     can plan a seam crossing by looking at it.
--
--   * 'PlayerFrame' reads up as the /player's/ up, which a mirrored seam
--     reverses. This is what living on the surface is actually like, and it is
--     only playable with a view that turns over with the player: in a fixed
--     view the same key press walks the opposite way after a lap, for a reason
--     nothing on screen shows.
data Frame
  = ChartFrame
  | PlayerFrame
  deriving (Eq, Show)

-- | How the player's own axes sit against the chart's: for each, whether the
-- player's sense of it runs backwards.
--
-- Two bits, and the module header says why one will not do. On a Mobius strip
-- and a Klein bottle 'turnU' is never set and this is the single parity bit
-- the game used to carry; on a projective plane both are live.
--
-- Not a 'Bool' pair by accident: these are the four symmetries of the square
-- that axis-aligned seams can generate, and composing two of them is @xor@
-- componentwise, which is what 'turnAfter' does.
data Turn = Turn
  { turnU :: !Bool,
    turnV :: !Bool
  }
  deriving (Eq, Show)

-- | The player as the chart sees them: nothing reversed. Where every level
-- starts.
square :: Turn
square = Turn False False

-- | The player's frame after a step, given the heading it was made along and
-- what the step did.
--
-- A mirrored crossing reflects the coordinate that runs /along/ the seam,
-- which is the axis the step was not moving along. That is the whole content
-- of this function, and it is the piece 'reversedFrame' cannot carry on its
-- own: the bit says a reflection happened, the heading says in which axis.
turnAfter :: Heading -> Crossing -> Turn -> Turn
turnAfter (Heading along _) crossing t
  | not (reversedFrame crossing) = t
  | along == U = t {turnV = not (turnV t)}
  | otherwise = t {turnU = not (turnU t)}

-- | How the player is standing, for a readout. Empty when they are standing
-- the way the chart does.
turnNote :: Turn -> String
turnNote (Turn False False) = ""
turnNote (Turn False True) = "upside down"
turnNote (Turn True False) = "left and right swapped"
turnNote (Turn True True) = "turned all the way round"

-- | A heading read the player's way round, or a player's heading read the
-- chart's way round.
--
-- Its own inverse, since reversing an axis twice is not reversing it, and that
-- is why 'headingFor' and 'dirOf' are the same function with the ends swapped.
throughTurn :: Turn -> Heading -> Heading
throughTurn t (Heading U side) = Heading U (if turnU t then flipSide side else side)
throughTurn t (Heading V side) = Heading V (if turnV t then flipSide side else side)

-- | The chart heading a key press means.
--
-- In 'ChartFrame' the player's own turn is not consulted at all, which is what
-- makes that frame plannable from a fixed picture. In 'PlayerFrame' each axis
-- is read the player's way round.
headingFor :: Frame -> Turn -> Dir -> Heading
headingFor ChartFrame _ = chartHeading
headingFor PlayerFrame t = throughTurn t . chartHeading

-- | The heading a key press means to the chart itself.
chartHeading :: Dir -> Heading
chartHeading DirRight = Heading U AtMax
chartHeading DirLeft = Heading U AtMin
chartHeading DirUp = Heading V AtMax
chartHeading DirDown = Heading V AtMin

flipSide :: Extremum -> Extremum
flipSide AtMin = AtMax
flipSide AtMax = AtMin

-- | The key press that means a heading, in a frame: the inverse of
-- 'headingFor', for a caller that has a heading in hand and wants to draw or
-- name it the way the player would.
--
-- Its own inverse, since reversing an axis twice is reversing it not at all,
-- which is why one function serves both ways round.
dirOf :: Frame -> Turn -> Heading -> Dir
dirOf ChartFrame _ = chartDir
dirOf PlayerFrame t = chartDir . throughTurn t

-- | Which key a chart heading is, read in the chart's own frame.
chartDir :: Heading -> Dir
chartDir (Heading U AtMax) = DirRight
chartDir (Heading U AtMin) = DirLeft
chartDir (Heading V AtMax) = DirUp
chartDir (Heading V AtMin) = DirDown

-- * Stepping

-- | One cell along a heading: where it lands, the heading it arrives with, and
-- what it did on the way.
--
-- 'Nothing' where the surface runs out, which only a Mobius strip's straight
-- axis does. It is not a wall --- a wall is terrain, and this is the surface
-- ending --- and "Sokoban.Rules" keeps the two apart because a player who
-- cannot tell them apart cannot tell a mistake from a boundary.
{-# ANN stepSpot ("HLint: ignore Eta reduce" :: String) #-}
stepSpot ::
  (KnownStrip w h) =>
  Surface ->
  Spot w h ->
  Heading ->
  Maybe (Spot w h, Heading, Crossing)
stepSpot surface here heading = surfaceStep surface here heading

-- | One key press: where it lands, and the walker's frame on arrival.
stepDir ::
  (KnownStrip w h) =>
  Surface ->
  Frame ->
  (Spot w h, Turn) ->
  Dir ->
  Maybe (Spot w h, Turn)
stepDir surface frame (here, t) dir = do
  let heading = headingFor frame t dir
  (there, _, crossing) <- stepSpot surface here heading
  pure (there, turnAfter heading crossing t)

-- | A run of key presses, from a cell and a frame. 'Nothing' the moment one of
-- them leaves the surface.
walkFrom ::
  (KnownStrip w h) =>
  Surface ->
  Frame ->
  (Spot w h, Turn) ->
  [Dir] ->
  Maybe (Spot w h, Turn)
walkFrom surface frame = foldM (stepDir surface frame)

-- | The cell @(dx, dy)@ away, as the walker at this cell would count it: up
-- the surface first, then along it.
--
-- The order does not matter, and that it does not is a fact about the surface
-- rather than a convenience. Going along first mirrors the axis @dy@ counts
-- down, and the walker's own up is mirrored with it, so the two arrive at the
-- same cell. It is only in the /chart's/ frame that they come apart --- which
-- is why a player-centred view has a well defined neighbourhood at all.
cellAround ::
  (KnownStrip w h) =>
  Surface ->
  (Spot w h, Turn) ->
  (Int, Int) ->
  Maybe (Spot w h, Turn)
cellAround surface start (dx, dy) =
  walkFrom
    surface
    PlayerFrame
    start
    ( replicate (abs dy) (if dy >= 0 then DirUp else DirDown)
        ++ replicate (abs dx) (if dx >= 0 then DirRight else DirLeft)
    )

-- | What is at picture position @(x, y)@, when the chart is drawn as a
-- rectangle with the surface continued past each side.
--
-- Inside the chart it is that cell. Past one side it is what you reach by
-- stepping off that side and carrying on, which is what the ghost strips of
-- both flat views draw --- and it is the whole of what a view needs to know
-- about gluing. It used to be @across - 1 - y@ written out in the drawing
-- code, which is the Mobius mirror and nothing else's.
--
-- 'Nothing' past a corner, and that is not laziness. Two walks reach a corner
-- --- along then up, or up then along --- and in the /chart's/ frame a turn
-- between them makes the two disagree. There is no cell to name, so no view
-- draws one. It is the same fact 'cellAround' turns the other way up: in the
-- walker's own frame the two walks do agree, because the walker is mirrored
-- along with the axis.
spotBeyond :: forall w h. (KnownStrip w h) => Surface -> (Int, Int) -> Maybe (Spot w h)
spotBeyond surface (x, y)
  | insideX && insideY = spotAt @w @h x y
  | insideY = walk (edgeX, y) (if x < 0 then DirLeft else DirRight) outX
  | insideX = walk (x, edgeY) (if y < 0 then DirDown else DirUp) outY
  | otherwise = Nothing
  where
    (around, across) = stripSize @w @h
    insideX = x >= 0 && x < around
    insideY = y >= 0 && y < across
    (edgeX, outX) = if x < 0 then (0, -x) else (around - 1, x - around + 1)
    (edgeY, outY) = if y < 0 then (0, -y) else (across - 1, y - across + 1)
    walk from dir n = do
      start <- uncurry (spotAt @w @h) from
      -- In 'ChartFrame': the chart's own right stays the chart's own right
      -- across a seam, so carrying on that way carries on the picture.
      (there, _) <- walkFrom surface ChartFrame (start, square) (replicate n dir)
      pure there
