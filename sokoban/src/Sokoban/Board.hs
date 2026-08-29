{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}

-- | The surface the game is played on, and the vocabulary the rules are
-- written in: what a cell holds, which way a key points, and where a step
-- from here lands.
--
-- The surface is a Mobius strip --- one chart of @'Clamped' w x 'Clamped' h@
-- glued to itself along its width, with a half twist. Every step goes through
-- 'Data.Grid.Atlas.Mobius.mobiusStep', so the twist is never something this
-- game computes. It is consulted.
--
-- Two things follow from the twist that a flat board does not have, and both
-- live here rather than in "Sokoban.Rules", because they are facts about the
-- surface and not about Sokoban:
--
--   * A lap of the strip does not return you to where you started. Crossing
--     the seam mirrors the /other/ axis --- @(u, v)@ becomes @(u, h-1-v)@ ---
--     so one lap lands you on the mirrored row and it takes two to close.
--     Only the middle row of an odd-height strip is a loop on its own.
--
--   * That mirror reverses the walker's own up. After an odd number of
--     crossings, the direction the player thinks of as up is the strip's
--     down. 'PlayerFrame' says so; 'ChartFrame' does not, and the difference
--     between them is the whole of what 'Frame' is for.
module Sokoban.Board
  ( -- * The surface
    Strip
  , KnownStrip
  , Spot
  , spotAt
  , spotXY
  , spotCoord
  , stripSpots
  , stripSize
    -- * What a cell holds
  , Tile(..)
  , walkable
  , Board
  , boardTile
  , boardFromGrid
    -- * Pointing
  , Dir(..)
  , allDirs
  , dirName
  , Frame(..)
  , headingFor
    -- * Stepping
  , stepSpot
  , crossesSeam
  ) where

import           Data.Grid.Atlas
import           Data.Grid.Atlas.Mobius
import           Data.Grid.Sized

import           GHC.TypeLits           (KnownNat, type (<=))

-- | The chart: a rectangle @w@ cells around the strip and @h@ cells across
-- it.
--
-- 'Clamped' on both axes and not 'Periodic' on the first, even though the
-- first axis is the one that wraps. The wrap is not a property of the axis
-- here: it is a gluing between the chart's two ends that reverses the other
-- axis on the way through, and no separable per-axis policy can say that.
-- Saying it is what the atlas layer is for. So each axis is clamped --- a
-- step off either end of the chart is a step out of the chart --- and
-- 'mobiusStep' decides which of those two ends has something on the far side.
type Strip w h = '[ Clamped w, Clamped h]

-- | What every function here needs to know about a board's size. Both bounds
-- come from 'Clamped' itself, which has no zero-length axis.
type KnownStrip w h = (KnownNat w, KnownNat h, 1 <= w, 1 <= h)

-- | A cell of the surface.
--
-- An 'AtlasCoord' at @k = 1@, so the chart component is always the single
-- 'Ordinal' @1@ and carries no information --- see 'spotAt'. It is kept
-- rather than projected away because it is what 'mobiusStep' takes and
-- returns, and unwrapping at every call site to rewrap at every next one
-- would be a translation layer that buys nothing.
type Spot w h = AtlasCoord (Strip w h) 1

-- | The cell at column @x@ (around the strip) and row @y@ (across it), or
-- 'Nothing' if either is off the chart.
spotAt :: forall w h. KnownStrip w h => Int -> Int -> Maybe (Spot w h)
spotAt x y = do
    u <- numToOrdinal x
    v <- numToOrdinal y
    pure (minBound, Clamped u :| Clamped v :| EmptyCoord)

-- | A cell as @(column, row)@ --- around the strip first, across it second.
spotXY :: KnownStrip w h => Spot w h -> (Int, Int)
spotXY (_, u :| v :| EmptyCoord) = (ordinalToInt (unClamped u), ordinalToInt (unClamped v))

-- | The chart-local coordinate, which is all of a 'Spot' that distinguishes
-- one cell from another at @k = 1@. What crate sets and goal sets are keyed
-- by.
spotCoord :: Spot w h -> Coord (Strip w h)
spotCoord = snd

-- | The board's size, as @(around the strip, across it)@. Named with a type
-- application --- @stripSize \@w \@h@ --- since there is no value to read it
-- off.
stripSize :: forall w h. KnownStrip w h => (Int, Int)
stripSize = (ordinalSize @w, ordinalSize @h)

-- | Every cell of the surface, once each.
stripSpots :: forall w h. KnownStrip w h => [Spot w h]
stripSpots = [(minBound, c) | c <- allCoord]

-- | What is built into a cell. Crates are not here: they move, and what
-- moves is state rather than terrain.
data Tile
    = Wall
    | Floor
    | Goal
    deriving (Eq, Show)

-- | A cell a player or a crate may occupy, crates aside.
walkable :: Tile -> Bool
walkable Wall  = False
walkable Floor = True
walkable Goal  = True

-- | The terrain, as the atlas it is played on.
--
-- One chart, so this is a 'Grid' in a wrapper that adds nothing to the
-- storage --- but 'mobiusStep' answers in 'AtlasCoord', and reading a cell
-- with 'atlasIndex' at the same coordinate is what keeps the two halves in
-- one vocabulary.
type Board w h = Atlas (Strip w h) 1 Tile

boardFromGrid :: Grid (Strip w h) Tile -> Board w h
boardFromGrid = mobiusAtlas

boardTile :: KnownStrip w h => Board w h -> Spot w h -> Tile
boardTile = atlasIndex

-- | A key press, in whichever frame it is being read in. Not a 'Heading':
-- a heading names an axis of the chart, and up is not an axis of the chart
-- once the player has crossed the seam an odd number of times.
data Dir
    = DirLeft
    | DirRight
    | DirUp
    | DirDown
    deriving (Eq, Show, Enum, Bounded)

allDirs :: [Dir]
allDirs = [minBound .. maxBound]

dirName :: Dir -> String
dirName DirLeft  = "left"
dirName DirRight = "right"
dirName DirUp    = "up"
dirName DirDown  = "down"

-- | Which frame the direction keys are read in.
--
-- The choice only exists because the surface is non-orientable, and it is a
-- real choice rather than a preference:
--
--   * 'ChartFrame' reads up as the chart's up, always. The board is then a
--     rectangle whose left and right edges are stitched with a vertical
--     mirror, and the player can plan a seam crossing by looking at it.
--
--   * 'PlayerFrame' reads up as the /player's/ up, which the seam reverses.
--     This is what living on the surface is actually like, and it is only
--     playable with a view that turns over with the player: in a fixed view
--     the same key press walks the opposite way after a lap, for a reason
--     nothing on screen shows.
data Frame
    = ChartFrame
    | PlayerFrame
    deriving (Eq, Show)

-- | The chart heading a key press means.
--
-- The 'Bool' is the player's parity: whether an odd number of seam crossings
-- has left them upside down with respect to the chart. Left and right are
-- untouched by it --- the seam's mirror fixes the axis it wraps and reverses
-- the one it does not --- so the parity only ever swaps up with down.
headingFor :: Frame -> Bool -> Dir -> Heading
headingFor _ _ DirRight = Heading Wrapped AtMax
headingFor _ _ DirLeft = Heading Wrapped AtMin
headingFor frame flipped dir = Heading Straight (side (upIsUp == (dir == DirUp)))
  where
    upIsUp = frame == ChartFrame || not flipped
    side True  = AtMax
    side False = AtMin

-- | One cell along a heading: where it lands, and the heading it arrives
-- with.
--
-- 'Nothing' is the strip's one genuine edge, the 'Straight' axis, which is
-- glued to nothing. It is not a wall --- a wall is terrain, and this is the
-- surface running out --- and "Sokoban.Rules" keeps the two apart because a
-- player who cannot tell them apart cannot tell a mistake from a boundary.
stepSpot :: KnownStrip w h => Spot w h -> Heading -> Maybe (Spot w h, Heading)
stepSpot = mobiusStep

-- | Does a step from here along this heading go through the seam?
--
-- __This is index arithmetic against a bound, and it should not have to be.__
-- 'Data.Atlas.Topology.Seam.SeamTable' answers exactly this question --- its
-- crossing returns a 'Bool' saying whether the frame is reversed, and
-- @Data.Grid.Atlas.Mobius.crossMobiusEdge@ returns 'True' for precisely the
-- two wrapped half-edges --- but 'mobiusStep' consumes that bit and returns
-- only a position and a heading. A caller carrying a frame of its own, which
-- is any caller on a non-orientable surface, has to reconstruct it. Filed as
-- sized-grid-lopy.5.
--
-- Reconstructed from the /source/ cell rather than by comparing the two
-- positions, because on a strip one cell around @u@ does not change across
-- the seam and a comparison would report no crossing where there was one.
crossesSeam :: forall w h. KnownStrip w h => Spot w h -> Heading -> Bool
crossesSeam spot (Heading along end) =
    case (along, end) of
        (Wrapped, AtMax) -> x == ordinalSize @w - 1
        (Wrapped, AtMin) -> x == 0
        (Straight, _)    -> False
  where
    (x, _) = spotXY spot
