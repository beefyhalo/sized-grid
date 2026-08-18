-- | Seam tables: how a set of charts is glued into one surface, with nothing
-- said about what a chart holds (sized-grid-1av, built as sized-grid-b15).
--
-- A discrete atlas has two halves that are usually written together and do
-- not have to be. One half is coordinate arithmetic: where inside a chart a
-- position is, and when a step off it crosses a boundary. That half needs to
-- know what a chart /is/, and it stays with whoever knows --- for the cube
-- map, "Data.Grid.Atlas.CubeMap"\'s @cubeStep@. The other half is pure
-- combinatorics: which boundary of which chart is glued to which, and with
-- what orientation. That half is this module, and it never mentions a cell,
-- a coordinate, or a grid --- hence a package whose only dependency is
-- @base@.
--
-- The split was not chosen by guessing what a second consumer might want.
-- @cubeSeam@ already had exactly this shape (a function from a face and an
-- edge label to another face, another edge label, and a flip bit), and its
-- correctness test already never mentioned @Grid@; 'seamIsInvolution' is
-- that test with @Face@ and @(Axis, Extremum)@ turned into parameters, and
-- nothing else changed.
--
-- == The law
--
-- A gluing is a pairing of half-edges, so crossing a seam and immediately
-- crossing back must land exactly where it started, with the two crossings
-- agreeing about orientation. That single law ('seamIsInvolution') is what
-- makes a table of independent entries into a surface: it forces each
-- half-edge to name a partner that names it back, so @2k@ entries describe
-- @k@ physical seams rather than @2k@ unrelated guesses. Because both the
-- chart id and the boundary label are finite in practice, checking it is
-- exhaustive enumeration, not sampling --- see 'seamViolations'.
module Data.Atlas.Topology.Seam
  ( HalfEdge
  , SeamTable(..)
  , seamIsInvolution
  , seamViolations
  ) where

-- | One side of one seam: a chart, and which boundary feature of that chart.
-- What a \"boundary feature\" is named by is the caller\'s business --- the
-- cube map labels an edge of a square face by an axis and an end of it, a
-- graph-shaped atlas might label a port by a number.
type HalfEdge chart boundary = (chart, boundary)

-- | The gluing: crossing a named boundary of a chart lands on some chart at
-- some boundary of its own, plus whether the along-the-seam direction is
-- preserved (@False@) or reversed (@True@) across the crossing.
--
-- Deliberately a function and not a @Map@: a seam table is usually written
-- as equations (the cube map\'s 24 of them) or computed from an index, and
-- either way total. A caller wanting partial seams --- a surface with a real
-- boundary, where some half-edges are glued to nothing --- would instantiate
-- @chart@ or @boundary@ with a @Maybe@-ish type of its own rather than have
-- every total table pay for a @Maybe@ here.
newtype SeamTable chart boundary = SeamTable
  { crossSeam :: chart -> boundary -> (chart, boundary, Bool)
  }

-- | Does this one half-edge obey the law: cross the seam, cross back, and
-- both the landing and the orientation bit are exactly what was started
-- with?
--
-- Note that this compares the /flip bit of both crossings/ as well as the
-- destination. A table where two paired half-edges disagree about whether
-- the seam reverses direction would satisfy \"crossing back returns me
-- home\" while still describing no consistent surface, and is rejected here.
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
--
-- The enumeration is an argument rather than a @Bounded@ \/ @Enum@
-- constraint on purpose: a boundary label is typically a product (the cube
-- map\'s @(Axis, Extremum)@), and base gives tuples 'Bounded' but not
-- 'Enum', so a class-driven version would reject the very first real caller.
-- Building the list is a comprehension at the call site, and returning the
-- offenders rather than a 'Bool' is what makes a failure say /which/ entry
-- of a 24-equation table is wrong.
seamViolations ::
       (Eq chart, Eq boundary)
    => SeamTable chart boundary
    -> [HalfEdge chart boundary]
    -> [HalfEdge chart boundary]
seamViolations table =
    filter (\(c, b) -> not (seamIsInvolution table c b))
