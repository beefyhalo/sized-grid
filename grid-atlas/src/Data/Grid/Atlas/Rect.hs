-- | Rectangular charts (sized-grid-4wn): the coordinate half of a seam
-- crossing, the half @atlas-topology@ deliberately does not have.
--
-- "Data.Atlas.Topology.Seam" splits a discrete atlas in two and keeps only
-- the combinatorics: which boundary of which chart is glued to which. The
-- other half --- deciding that a step leaves the chart at all, and turning
-- the table's answer into a landing coordinate --- needs to know what a
-- chart /is/, so it stays with whoever knows. This module is that half for
-- one chart shape: a rectangle of cells with two axes.
--
-- One shape, not all shapes. There is no class here saying what a boundary
-- of an arbitrary chart is, because the two atlases that exist are both
-- rectangular and a class fitted to two instances of one shape would be a
-- guess about the third. What the split does say is where the seam between
-- the two would go if a non-rectangular chart ever arrives: everything in
-- this module is what @Rect@ contributes, and nothing in it is cube- or
-- Mobius-specific.
--
-- == What varies between the two callers
--
-- 'rectStep' takes as arguments exactly the three things
-- "Data.Grid.Atlas.CubeMap" and "Data.Grid.Atlas.Mobius" disagree about,
-- and nothing else:
--
--   * the size of each axis --- @n@ twice for a cube face, @w@ and @h@ for
--     a Mobius strip;
--   * the gluing, as a lookup rather than a
--     'Data.Atlas.Topology.Seam.SeamTable' directly, so a caller can hand
--     over a table that does not glue every edge;
--   * how partial that lookup is, as the 'Applicative' it answers in. A
--     cube has no edge of its own, so its lookup is total and it steps in
--     @Identity@; a Mobius strip's straight axis is a genuine @Clamped@
--     edge with nothing on the other side, so it steps in 'Maybe'. The
--     step is exactly as partial as the gluing it is given.
--
-- == What a caller must guarantee
--
-- Both boundaries of a seam must have the same length. A crossing carries
-- the along-the-edge coordinate over unchanged (or mirrored), so gluing an
-- edge of @w@ cells to one of @h@ cells with @w \/= h@ has no landing cell
-- to name for the overhang. Nothing here checks it --- the caller's own
-- @unsafeOrdinal@ assertion is what fires --- because it is a property of
-- the table, fixed once when the table is written, not of a step.
module Data.Grid.Atlas.Rect
  ( Axis(..)
  , Heading(..)
  , rectStep
  ) where

import           Data.Grid.Sized (Extremum (..))

-- | Which of a rectangular chart's two axes. Not a type-level axis position
-- (as @mapAxis@ names one): both axes of a chart share a type, so a runtime
-- tag is all a caller needs to say which one a heading points along.
data Axis
    = U
    | V
    deriving (Eq, Show, Enum, Bounded)

-- | The axis a heading along the given one does /not/ move along --- the
-- axis a crossing's along-the-edge coordinate runs down.
otherAxis :: Axis -> Axis
otherAxis U = V
otherAxis V = U

-- | A unit step: which axis, and which end of it the step moves towards.
-- The same @(axis, end)@ pair a seam table uses as its boundary label, and
-- deliberately so --- a step that leaves the chart /is/ the half-edge it
-- leaves through.
data Heading = Heading
    { headingAxis :: Axis
    , headingSide :: Extremum
    } deriving (Eq, Show)

sideSign :: Extremum -> Int
sideSign AtMin = -1
sideSign AtMax = 1

-- | The end of an axis opposite the given one.
oppositeSide :: Extremum -> Extremum
oppositeSide AtMin = AtMax
oppositeSide AtMax = AtMin

-- | Move one cell in a heading, crossing a seam --- with its frame
-- transform applied to the heading itself --- if the step would leave the
-- chart.
--
-- Positions are raw @0..size-1@ pairs, in @(U, V)@ order, rather than a
-- 'Data.Grid.Sized.Coord': a chart's coordinate type carries a boundary
-- policy, and the policy this implements (leave the chart, land where the
-- seam says) is not the one the coordinate names, the same way
-- @atlasOffsetHead@ works below @headAxis@'s own policy rather than
-- through it. Wrapping the answer back up is the caller's job, and it is
-- what the caller was going to do anyway --- a cube face's @Ordinal n@ and
-- a Mobius strip's @Clamped w@ are not the same wrapper.
--
-- Only ever moves by one cell. A caller walking further composes calls;
-- nothing here needs to, since a single crossing is the whole difficulty.
rectStep ::
       Applicative f
    => (Axis -> Int)
    -> (chart -> (Axis, Extremum) -> f (chart, (Axis, Extremum), Bool))
    -> chart
    -> (Int, Int)
    -> Heading
    -> f (chart, (Int, Int), Heading)
rectStep sizeOf cross chart (u, v) heading@(Heading axis side) =
    let d = sideSign side
        (u', v') =
            case axis of
                U -> (u + d, v)
                V -> (u, v + d)
        moved =
            case axis of
                U -> u'
                V -> v'
    in if moved >= 0 && moved < sizeOf axis
           then pure (chart, (u', v'), heading)
           else land <$> cross chart (axis, side)
  where
    -- The along-the-edge coordinate is the one on the axis the step is not
    -- moving along; it survives the crossing, mirrored if the seam says the
    -- two edges run opposite ways.
    free =
        case axis of
            U -> v
            V -> u
    land (destChart, (destAxis, destSide), reversed) =
        let free'
                | reversed = sizeOf (otherAxis destAxis) - 1 - free
                | otherwise = free
            fixed =
                case destSide of
                    AtMin -> 0
                    AtMax -> sizeOf destAxis - 1
            landed =
                case destAxis of
                    U -> (fixed, free')
                    V -> (free', fixed)
            -- A walker landing on the destination's AtMax edge must now be
            -- heading towards AtMin (further into that chart, away from the
            -- edge it just crossed), and the reverse at AtMin --
            -- independently of which side of the source it left from, since
            -- a genuine crossing's approach direction is already fixed by
            -- 'side'.
         in (destChart, landed, Heading destAxis (oppositeSide destSide))
