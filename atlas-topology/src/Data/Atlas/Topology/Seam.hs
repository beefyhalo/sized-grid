-- | Seam tables: how a set of charts is glued into one surface, with nothing
-- said about what a chart holds.
module Data.Atlas.Topology.Seam
  ( HalfEdge
  , SeamTable(..)
  , seamIsInvolution
  , seamViolations
  ) where

-- | One side of one seam: a chart, and which boundary feature of that chart.
type HalfEdge chart boundary = (chart, boundary)

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
