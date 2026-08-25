-- | Seam tables: how a set of charts is glued into one surface, with nothing
-- said about what a chart holds.
module Data.Atlas.Topology.Seam
  ( HalfEdge
  , Corner
  , SeamTable(..)
  , seamIsInvolution
  , seamViolations
  , vertexCycleLengths
  , vertexCycleViolations
  ) where

import           Data.List  (elemIndex, nub)
import           Data.Maybe (mapMaybe)

-- | One side of one seam: a chart, and which boundary feature of that chart.
type HalfEdge chart boundary = (chart, boundary)

-- | The two boundary incidences of one chart corner --- the pair of
-- half-edges that meet there.
--
-- The order /within/ the pair is not consulted; a corner is identified by
-- which two half-edges it joins. What matters is the order of the
-- enumeration a corner is supplied in, because that is the only thing that
-- says which end of a boundary a corner sits at. The two corners incident
-- with one boundary must appear in the list in the direction the seam
-- table's orientation bit is measured along, so the earlier of the two is
-- that boundary's first end and the later one its second.
--
-- For a rectangular chart glued the way @Data.Grid.Atlas.Rect@ glues one
-- --- the along-edge coordinate running from @0@ upwards --- listing the
-- four corners in row-major order satisfies this.
type Corner chart boundary = (HalfEdge chart boundary, HalfEdge chart boundary)

-- | The gluing: crossing a named boundary of a chart lands on some chart at
-- some boundary of its own, plus whether the along-the-seam direction is
-- preserved (@False@) or reversed (@True@) across the crossing.
newtype SeamTable chart boundary = SeamTable
  { crossSeam :: chart -> boundary -> (chart, boundary, Bool)
  }

-- | Does this one half-edge obey the law: cross the seam, cross back, and
-- both the landing and the orientation bit are exactly what was started
-- with?
seamIsInvolution ::
       (Eq chart, Eq boundary)
    => SeamTable chart boundary
    -> chart
    -> boundary
    -> Bool
seamIsInvolution (SeamTable cross) c b =
    let (c', b', flip1) = cross c b
        (c'', b'', flip2) = cross c' b'
    in (c'', b'', flip2) == (c, b, flip1)

-- | The half-edges of a supplied enumeration that break the law --- empty
-- exactly when the table is a valid gluing of that set of half-edges.
seamViolations ::
       (Eq chart, Eq boundary)
    => SeamTable chart boundary
    -> [HalfEdge chart boundary]
    -> [HalfEdge chart boundary]
seamViolations table =
    filter (\(c, b) -> not (seamIsInvolution table c b))

-- | Lengths of the vertex cycles induced by a seam table and an enumeration
-- of its corners.
--
-- A crossing carries each end of the boundary it leaves to one end of the
-- boundary it lands on: to the end in the same position when the two run the
-- same way, and to the opposite end when the table's orientation bit says
-- they run opposite ways. Each corner therefore has exactly one landing
-- corner through each of its two boundaries, so the graph is 2-regular and
-- its connected components are the vertex cycles.
--
-- Which end of a boundary a corner sits at comes from the enumeration order
-- --- see 'Corner' for the contract that places on the caller. Using the
-- orientation bit is what separates surfaces that this cannot: a torus and a
-- Klein bottle both report @[4]@, while a projective plane reports @[2]@,
-- its two cone points of angle pi.
--
-- Distinct lengths only: two vertices of the same cycle length report once.
--
-- A half-edge the enumeration does not give exactly two ends for is skipped
-- rather than reported. Validating the enumeration itself --- that it is
-- complete, and that every boundary is incident exactly twice --- is
-- sized-grid-oj6z.
vertexCycleLengths ::
       (Eq chart, Eq boundary)
    => SeamTable chart boundary
    -> [Corner chart boundary]
    -> [Int]
vertexCycleLengths table corners = nub (map (length . component) corners)
  where
    component start = walk [start] [start]
      where
        walk seen [] = seen
        walk seen (corner:queue) =
            let fresh = [c | c <- neighbours corner, c `notElem` seen]
             in walk (fresh ++ seen) (fresh ++ queue)

    neighbours corner@(left, right) =
        nub (mapMaybe (landing corner) (nub [left, right]))

    -- The single corner reached by crossing one of this corner's two
    -- boundaries.
    landing corner (chart, boundary)
        | Just endIndex <- elemIndex corner sourceEnds
        , length sourceEnds == 2
        , length destEnds == 2 =
            Just (destEnds !! (if reversed then 1 - endIndex else endIndex))
        | otherwise = Nothing
      where
        sourceEnds = endsOf corners (chart, boundary)
        destEnds = endsOf corners (destChart, destBoundary)
        (destChart, destBoundary, reversed) = crossSeam table chart boundary

-- | The corners of an enumeration that are incident with one half-edge, in
-- enumeration order. Well-formed data gives exactly two: that half-edge's
-- two ends, in the direction the orientation bit is measured along.
endsOf ::
       (Eq chart, Eq boundary)
    => [Corner chart boundary]
    -> HalfEdge chart boundary
    -> [Corner chart boundary]
endsOf corners halfEdge =
    [corner | corner@(left, right) <- corners, left == halfEdge || right == halfEdge]


-- | The vertex cycles that are not the four-corner cycles of a flat square
-- surface. An empty result is the checkable flatness law.
vertexCycleViolations ::
       (Eq chart, Eq boundary)
    => SeamTable chart boundary
    -> [Corner chart boundary]
    -> [Int]
vertexCycleViolations table = filter (/= 4) . vertexCycleLengths table
