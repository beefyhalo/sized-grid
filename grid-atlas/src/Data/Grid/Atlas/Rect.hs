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
-- of an arbitrary chart is, because every atlas that exists is rectangular
-- and a class fitted to instances of one shape would be a guess about the
-- next. What the split does say is where the seam between the two halves
-- would go if a non-rectangular chart ever arrives: everything in this
-- module is what @Rect@ contributes, and nothing in it belongs to any one
-- surface.
--
-- == What varies between the callers
--
-- 'rectStep' takes as arguments exactly the three things
-- "Data.Grid.Atlas.CubeMap", "Data.Grid.Atlas.Mobius" and
-- "Data.Grid.Atlas.Klein" disagree about, and nothing else:
--
--   * the size of each axis --- @n@ twice for a cube face, @w@ and @h@ for
--     a strip or a bottle;
--   * the gluing, as a lookup rather than a
--     'Data.Atlas.Topology.Seam.SeamTable' directly, so a caller can hand
--     over a table that does not glue every edge;
--   * how partial that lookup is, as the 'Applicative' it answers in. A
--     cube and a Klein bottle have no edge of their own, so their lookups
--     are total and they step in @Identity@; a Mobius strip's straight axis
--     is a genuine @Clamped@ edge with nothing on the other side, so it
--     steps in 'Maybe'. The step is exactly as partial as the gluing it is
--     given.
--
-- == What a caller must guarantee
--
-- Both boundaries of a seam must have the same length. A crossing carries
-- the along-the-edge coordinate over unchanged (or mirrored), so gluing an
-- edge of @w@ cells to one of @h@ cells with @w \/= h@ has no landing cell
-- to name for the overhang. Nothing here checks it --- the caller's own
-- @unsafeOrdinal@ assertion is what fires --- because it is a property of
-- the table, fixed once when the table is written, not of a step.
--
-- Which is also why a mirrored crossing measures against the /destination/
-- axis rather than the source's sibling. The two coincide whenever a seam
-- lands on the axis it left from --- which is every seam a strip or a
-- bottle has, and is why a hand-written single-chart stepper can use the
-- sibling and still be right --- but a cube's seams turn a corner, and only
-- the all-sizes-equal accident hides the difference there. 'rectStep' takes
-- one size function for the whole atlas rather than one per chart because
-- @Atlas cs k a@ gives every chart the same shape @cs@ by construction;
-- charts of differing sizes would make this a per-chart question again.
module Data.Grid.Atlas.Rect
  ( Axis (..),
    Heading (..),
    Crossing (..),
    crossedSeam,
    reversedFrame,
    Landing (..),
    rectStep,
    mirroringHalfEdges,
    orientable,
  )
where

import Data.Atlas.Topology.Seam (HalfEdge, SeamTable (..))
import Data.Functor.Identity (Identity (..))
import Data.Grid.Sized (Extremum (..))

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
  { headingAxis :: Axis,
    headingSide :: Extremum
  }
  deriving (Eq, Show)

-- | What a step did on the way, which is the part that neither the landing
-- coordinate nor the new heading says.
--
-- A walker on a surface carries more than a position and a direction. It
-- carries a /frame/ --- which way its left hand points --- and a seam can hand
-- that frame back mirrored. Nothing in the landing coordinate shows it: a step
-- along a Mobius strip's wrapped axis arrives at the mirrored row with the
-- heading it left with, and the only thing that has changed is which side the
-- walker's left is on. A caller reading direction keys in the walker's own
-- frame --- which is any caller drawing a player-centred view --- has to know.
--
-- 'Seam' is kept apart from 'Interior' as well as from 'MirroredSeam', because
-- a caller that draws or reports a crossing wants to know one happened even
-- where the frame survived it. On a cube map every crossing is a real crossing
-- and none of them mirrors anything.
data Crossing
  = -- | The step stayed inside its chart. No seam was consulted.
    Interior
  | -- | The step crossed a seam that carried the walker's frame across as it
    -- was.
    Seam
  | -- | The step crossed a seam that handed the walker's frame back reversed.
    MirroredSeam
  deriving (Eq, Show, Enum, Bounded)

-- | Did this step leave the chart it started in?
crossedSeam :: Crossing -> Bool
crossedSeam Interior = False
crossedSeam _ = True

-- | Is the walker's left hand now on its other side?
--
-- The one bit a walker on a non-orientable surface must carry and cannot work
-- out from where it is or which way it faces. Accumulate it with @xor@ along a
-- walk: an even number of mirrorings and the walker is as it started, an odd
-- number and it is not. A walk that closes --- same chart, same cell, same
-- heading --- with an odd count is a proof that the surface is not orientable,
-- and there is no such walk on a cube map.
reversedFrame :: Crossing -> Bool
reversedFrame MirroredSeam = True
reversedFrame _ = False

-- | Where a step ended up, and what it did on the way.
data Landing chart = Landing
  { landedChart :: chart,
    landedAt :: (Int, Int),
    landedHeading :: Heading,
    landedCrossing :: Crossing
  }
  deriving (Eq, Show)

-- | Which way a walker's left hand points along the axis it is /not/ moving
-- along: 'True' when it points the way that axis's coordinate increases.
--
-- A quarter turn anticlockwise from the heading, in the frame where @U@ is
-- drawn rightwards and @V@ upwards. Heading @U@ 'AtMax' has its left along
-- @+V@; heading @V@ 'AtMax' has its left along @-U@.
leftHandIncreases :: Heading -> Bool
leftHandIncreases (Heading a s) = (a == U) == (s == AtMax)

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
  (Applicative f) =>
  (Axis -> Int) ->
  (chart -> (Axis, Extremum) -> f (chart, (Axis, Extremum), Bool)) ->
  chart ->
  (Int, Int) ->
  Heading ->
  f (Landing chart)
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
        then pure (Landing chart (u', v') heading Interior)
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
          heading' = Heading destAxis (oppositeSide destSide)
          -- The seam's own bit says whether the two edges run the same way
          -- along the coordinate they share. That is not yet the walker's
          -- answer, because the walker's left lies on the increasing side
          -- of that coordinate for some headings and the decreasing side
          -- for others, and a crossing can change which. The frame survives
          -- exactly when those two disagreements cancel.
          --
          -- This is why the bit cannot be recovered by the caller from the
          -- table alone: it depends on the heading as well as the seam. A
          -- cube map's table says 'not reversed' everywhere and no cube
          -- crossing mirrors a walker; a projective plane's says 'reversed'
          -- everywhere and every crossing does; a Klein bottle's says one
          -- of each and the answers follow.
          mirrored =
            reversed
              /= (leftHandIncreases heading /= leftHandIncreases heading')
          crossing
            | mirrored = MirroredSeam
            | otherwise = Seam
       in Landing destChart landed heading' crossing

-- | The boundary incidences whose actual walker-frame crossing is mirrored.
--
-- The table's orientation bit is not sufficient here: it describes the
-- along-edge coordinates, while the frame also depends on the source and
-- destination headings.
mirroringHalfEdges ::
  (Axis -> Int) ->
  SeamTable chart (Axis, Extremum) ->
  [HalfEdge chart (Axis, Extremum)] ->
  [HalfEdge chart (Axis, Extremum)]
mirroringHalfEdges sizeOf table =
  filter (seamMirrors sizeOf table)

-- | Whether the rectangular charts admit a consistent orientation.
--
-- Each seam is an edge constraint on chart orientations: crossing it either
-- preserves or reverses the chosen orientation. The surface is orientable
-- exactly when those constraints are 2-colourable.
orientable ::
  (Eq chart) =>
  (Axis -> Int) ->
  SeamTable chart (Axis, Extremum) ->
  [HalfEdge chart (Axis, Extremum)] ->
  Bool
orientable sizeOf table halfEdges =
  components charts []
  where
    charts = [chart | (chart, _) <- halfEdges]
    constraints =
      [ (chart, destination, seamMirrors sizeOf table halfEdge)
      | halfEdge@(chart, boundary) <- halfEdges,
        let (destination, _, _) = crossSeam table chart boundary
      ]

    neighbours chart =
      [ (destination, mirrored)
      | (source, destination, mirrored) <- constraints,
        source == chart
      ]
        ++ [ (source, mirrored)
           | (source, destination, mirrored) <- constraints,
             destination == chart
           ]

    components [] _ = True
    components (chart : rest) assigned =
      case lookup chart assigned of
        Just _ -> components rest assigned
        Nothing ->
          case colour [(chart, False)] assigned of
            Nothing -> False
            Just assigned' -> components rest assigned'

    colour [] assigned = Just assigned
    colour ((chart, expected) : queue) assigned =
      case lookup chart assigned of
        Just actual
          | actual == expected -> colour queue assigned
          | otherwise -> Nothing
        Nothing ->
          colour
            ([(neighbour, expected /= mirrored) | (neighbour, mirrored) <- neighbours chart] ++ queue)
            ((chart, expected) : assigned)

seamMirrors ::
  (Axis -> Int) ->
  SeamTable chart (Axis, Extremum) ->
  HalfEdge chart (Axis, Extremum) ->
  Bool
seamMirrors sizeOf (SeamTable cross) (chart, (axis, side)) =
  let (u, v) =
        case axis of
          U -> (boundaryCoordinate side (sizeOf U), 0)
          V -> (0, boundaryCoordinate side (sizeOf V))
      Landing _ _ _ crossing =
        runIdentity $
          rectStep
            sizeOf
            (\source boundary -> Identity (cross source boundary))
            chart
            (u, v)
            (Heading axis side)
   in reversedFrame crossing
  where
    boundaryCoordinate AtMin _ = 0
    boundaryCoordinate AtMax size = size - 1
