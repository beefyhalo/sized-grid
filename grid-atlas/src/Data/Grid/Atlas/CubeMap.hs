{-# LANGUAGE DataKinds #-}

-- | The cube map (sized-grid-68j): 6 same-shaped charts --- each a
-- @Grid '[Ordinal n, Ordinal n] a@, one per face of a cube --- glued along
-- their 12 edges by an explicit transition table. "Data.Grid.Atlas"'s own
-- atlas ('atlasFromTiles') only ever glues charts with the identity
-- transition, because a tiling never needs anything else; a cube map is the
-- smallest atlas where crossing a seam genuinely rotates the local frame,
-- sized-grid-o1n's seam rule and sized-grid-1bm's @grid space = Z^n / G@
-- framing made concrete for the first non-separable case.
--
-- == Deriving the transition table
--
-- Each face is given its own @(u, v)@ frame by choosing, per face, the two
-- world axes that are not its own normal, oriented so that the cross product
-- of @u_dir@ and @v_dir@ equals the normal --- the standard right-hand,
-- outward-normal convention, applied consistently to all 6 faces. Crossing an
-- edge of one face is then: which world axis does that edge's outward
-- direction point along (that names the destination face, since a face's
-- normal IS the direction just past its own edge); and, expressed in the destination
-- face's own @(u, v)@ frame, which axis is fixed at the shared edge, which
-- side of it, and whether the free (along-edge) coordinate runs the same way
-- or backwards.
--
-- That table was not derived by staring at a paper net --- it was computed
-- from the 6 faces' @(u, v)@ bases directly (see sized-grid-68j's closing
-- notes for the derivation), then checked two ways before being transcribed
-- here as the 24 equations of 'cubeSeam':
--
--   * every one of the 24 half-edges pairs with exactly one other half-edge,
--     each pointing back at the one that named it (the 12 physical edges of
--     a cube, matched up correctly);
--   * a walker following a fixed heading all the way around any of the
--     cube's three 4-face equatorial belts returns to its exact starting
--     cell and heading after exactly @4n@ steps --- both checks are in the
--     test suite (@Test.CubeMap@'s @cubeSeamPairsUp@ and
--     @cubeStepBeltCloses@), not just a one-off script.
--
-- The frame transform this table implies (see 'cubeStep') is /not/ only the
-- 4-element rotation subgroup a hand-drawn net might suggest: with this
-- particular per-face basis choice, some seams are orientation-reversing in
-- raw @(u, v)@-coordinate terms (a genuine reflection, not just a rotation),
-- which only shows up once the table is derived rather than guessed. It is
-- still always exactly one of the 8 elements of sized-grid-1bm's @D4@ ---
-- the signed-permutation group on a face's 2 axes --- 'cubeStep' just never
-- needs to name the element explicitly, because the coordinate landing and
-- the heading transform are both read directly off 'cubeSeam'\'s
-- @(Axis, Extremum, Bool)@ fields.
module Data.Grid.Atlas.CubeMap
  ( Face(..)
  , faceIndex
  , indexFace
  , cubeAtlas
  , Axis(..)
  , Heading(..)
  , cubeSeam
  , cubeStep
  ) where

import           Data.Grid.Atlas
import           Data.Grid.Sized

import           Data.Maybe  (fromMaybe)
import qualified Data.Vector as V
import           GHC.TypeLits

-- | The six faces of a cube map, named by the world axis and sign their
-- outward normal points along. Everywhere else in this module (and in
-- 'AtlasCoord' itself) a chart is an 'Ordinal 6', matching
-- "Data.Grid.Atlas"'s own convention; 'Face' exists only so 'cubeSeam' can be
-- written and read against names instead of the 6 magic indices, via
-- 'faceIndex' \/ 'indexFace' at the boundary.
data Face
    = PosX
    | NegX
    | PosY
    | NegY
    | PosZ
    | NegZ
    deriving (Eq, Show, Enum, Bounded)

-- | A face's chart index, in 'Face'\'s own 'Enum' order.
faceIndex :: Face -> Ordinal 6
faceIndex = unsafeOrdinal . fromEnum

-- | The face a chart index names.
indexFace :: Ordinal 6 -> Face
indexFace = toEnum . ordinalToInt

-- | Build a cube atlas from its 6 faces, given in 'Face'\'s own order
-- (@PosX, NegX, PosY, NegY, PosZ, NegZ@). Total: 6 arguments always give
-- 'atlasFromVector' a length-6 vector, so the 'Nothing' case cannot arise.
cubeAtlas ::
       forall n a.
       Grid '[ Ordinal n, Ordinal n] a
    -> Grid '[ Ordinal n, Ordinal n] a
    -> Grid '[ Ordinal n, Ordinal n] a
    -> Grid '[ Ordinal n, Ordinal n] a
    -> Grid '[ Ordinal n, Ordinal n] a
    -> Grid '[ Ordinal n, Ordinal n] a
    -> Atlas '[ Ordinal n, Ordinal n] 6 a
cubeAtlas px nx py ny pz nz =
    fromMaybe (error "cubeAtlas: impossible, six faces always match k = 6") $
    atlasFromVector (V.fromList [px, nx, py, ny, pz, nz])

-- | Which of a face's two axes: position 0 in @'[Ordinal n, Ordinal n]@, or
-- position 1. Not a type-level axis position (as 'mapAxis' names one) ---
-- both of a face's axes share a type, so a runtime tag is all a caller needs
-- to say which one a heading points along.
data Axis
    = U
    | V
    deriving (Eq, Show, Enum, Bounded)

-- | The direction a walker on the cube's surface is currently facing: one
-- axis of its current face, and which end of it the walker is heading
-- towards. Deliberately axis-aligned, the same restriction every neighbour
-- query in this library has ('vonNeumannNeighbours', 'axisSteps'): a diagonal
-- heading is not a thing a single seam crossing needs to resolve.
data Heading = Heading
    { headingAxis :: Axis
    , headingSide :: Extremum
    } deriving (Eq, Show)

sideSign :: Extremum -> Int
sideSign AtMin = -1
sideSign AtMax = 1

signSide :: Int -> Extremum
signSide d
    | d < 0 = AtMin
    | otherwise = AtMax

-- | The transition table: crossing a named 'Axis' at a named 'Extremum' of a
-- 'Face' lands on another face, at a named axis and extremum of its own, and
-- says whether the free (along-edge) coordinate runs the same way (@False@)
-- or backwards (@True@) between the two. See the module haddock for how this
-- was derived and checked; see 'cubeStep' for how the 4 fields here become
-- both a landing coordinate and a frame transform for a crossing heading.
--
-- 24 equations, one per half-edge of the cube's 12 physical edges, each
-- pointing back at the one that names it --- misassign any single entry and
-- @Test.CubeMap@'s @cubeSeamPairsUp@ catches it.
cubeSeam :: Face -> Axis -> Extremum -> (Face, Axis, Extremum, Bool)
cubeSeam PosX U AtMin = (NegY, U, AtMax, False)
cubeSeam PosX U AtMax = (PosY, V, AtMax, False)
cubeSeam PosX V AtMin = (NegZ, V, AtMax, False)
cubeSeam PosX V AtMax = (PosZ, U, AtMax, False)
cubeSeam NegX U AtMin = (NegZ, V, AtMin, False)
cubeSeam NegX U AtMax = (PosZ, U, AtMin, False)
cubeSeam NegX V AtMin = (NegY, U, AtMin, False)
cubeSeam NegX V AtMax = (PosY, V, AtMin, False)
cubeSeam PosY U AtMin = (NegZ, U, AtMax, False)
cubeSeam PosY U AtMax = (PosZ, V, AtMax, False)
cubeSeam PosY V AtMin = (NegX, V, AtMax, False)
cubeSeam PosY V AtMax = (PosX, U, AtMax, False)
cubeSeam NegY U AtMin = (NegX, V, AtMin, False)
cubeSeam NegY U AtMax = (PosX, U, AtMin, False)
cubeSeam NegY V AtMin = (NegZ, U, AtMin, False)
cubeSeam NegY V AtMax = (PosZ, V, AtMin, False)
cubeSeam PosZ U AtMin = (NegX, U, AtMax, False)
cubeSeam PosZ U AtMax = (PosX, V, AtMax, False)
cubeSeam PosZ V AtMin = (NegY, V, AtMax, False)
cubeSeam PosZ V AtMax = (PosY, U, AtMax, False)
cubeSeam NegZ U AtMin = (NegY, V, AtMin, False)
cubeSeam NegZ U AtMax = (PosY, U, AtMin, False)
cubeSeam NegZ V AtMin = (NegX, U, AtMin, False)
cubeSeam NegZ V AtMax = (PosX, V, AtMin, False)

-- | Move one cell in a heading, crossing a seam --- with its frame transform
-- applied to the heading itself --- if the step would leave the current
-- face. Total, unlike 'atlasOffsetHead': a cube has no edge of its own, only
-- seams, so every step lands somewhere.
--
-- Only ever moves the atlas coordinate by one cell. Composing several calls
-- (as a caller walking a longer heading would) is what carries a walker
-- across more than one face; nothing here needs to, because a single seam
-- crossing is already sized-grid-68j's whole point, and doing it also
-- requires no 'Atlas' value at all --- the landing chart and coordinate are
-- pure functions of the current one via 'cubeSeam', not a lookup into any
-- particular atlas's contents.
cubeStep ::
       forall n. KnownNat n
    => AtlasCoord '[ Ordinal n, Ordinal n] 6
    -> Heading
    -> (AtlasCoord '[ Ordinal n, Ordinal n] 6, Heading)
cubeStep (chart, u :| v :| EmptyCoord) heading@(Heading axis side) =
    let size = ordinalSize @n
        ui = ordinalToInt u
        vi = ordinalToInt v
        d = sideSign side
        (ui', vi') =
            case axis of
                U -> (ui + d, vi)
                V -> (ui, vi + d)
    in if ui' >= 0 && ui' < size && vi' >= 0 && vi' < size
           then ( (chart, unsafeOrdinal ui' :| unsafeOrdinal vi' :| EmptyCoord)
                , heading)
           else
               let free =
                       case axis of
                           U -> vi
                           V -> ui
                   (destFace, destAxis, destSide, reversed) =
                       cubeSeam (indexFace chart) axis side
                   free'
                       | reversed = size - 1 - free
                       | otherwise = free
                   fixed =
                       case destSide of
                           AtMin -> 0
                           AtMax -> size - 1
                   (u2, v2) =
                       case destAxis of
                           U -> (fixed, free')
                           V -> (free', fixed)
                   -- A walker landing on the destination's AtMax edge must
                   -- now be heading towards AtMin (further into that face,
                   -- away from the edge it just crossed), and the reverse at
                   -- AtMin -- independently of which side of the source it
                   -- left from, since a genuine crossing's approach
                   -- direction is already fixed by 'side'. Exactly what
                   -- 'cubeStepBeltCloses' checks, over all 600 starting
                   -- configurations on an n=5 cube.
                   newSign = negate (sideSign destSide)
               in ( ( faceIndex destFace
                    , unsafeOrdinal u2 :| unsafeOrdinal v2 :| EmptyCoord)
                  , Heading destAxis (signSide newSign))
