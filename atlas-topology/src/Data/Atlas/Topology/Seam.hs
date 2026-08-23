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

import           Data.List (nub)

-- | One side of one seam: a chart, and which boundary feature of that chart.
type HalfEdge chart boundary = (chart, boundary)

-- | The two boundary incidences of one chart corner. The order is the local
-- order around the corner; the supplied enumeration also orders the two
-- occurrences of each boundary, from one end of that boundary to the other.
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
-- of its corners. Crossing either incident boundary joins a corner to the
-- destination corners incident with the crossed boundary, and the resulting
-- connected components are the vertex cycles.
vertexCycleLengths ::
       (Eq chart, Eq boundary)
    => SeamTable chart boundary
    -> [Corner chart boundary]
    -> [Int]
vertexCycleLengths table corners =
    nub [length (componentFrom corner []) | corner <- corners]
  where
    componentFrom corner seen
        | corner `elem` seen = seen
        | otherwise =
            foldr componentFrom (corner : seen) (neighbors corner)

    neighbors (left, right) =
        nub
            [ candidate
            | boundary <- [snd left, snd right]
            , let (chart, crossedBoundary, _) =
                    crossSeam table (fst (if boundary == snd left then left else right)) boundary
            , candidate@(candidateLeft, candidateRight) <- corners
            , incidenceOf candidateLeft chart crossedBoundary ||
              incidenceOf candidateRight chart crossedBoundary
            ]

    incidenceOf (chart, boundary) expectedChart expectedBoundary =
        chart == expectedChart && boundary == expectedBoundary


-- | The vertex cycles that are not the four-corner cycles of a flat square
-- surface. An empty result is the checkable flatness law.
vertexCycleViolations ::
       (Eq chart, Eq boundary)
    => SeamTable chart boundary
    -> [Corner chart boundary]
    -> [Int]
vertexCycleViolations table = filter (/= 4) . vertexCycleLengths table
