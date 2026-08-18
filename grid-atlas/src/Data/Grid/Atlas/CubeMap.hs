{-# LANGUAGE DataKinds #-}

-- | 6 same-shaped charts, one per face of a cube, glued along their 12 edges
-- by an explicit transition table.
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

import           Data.Atlas.Topology.Seam (SeamTable (..))
import           Data.Grid.Atlas
import           Data.Grid.Atlas.Rect
import           Data.Grid.Sized

import           Data.Functor.Identity (Identity (..))
import           Data.Maybe            (fromMaybe)
import qualified Data.Vector           as V
import           GHC.TypeLits

-- | The six faces of a cube map, named by the world axis and sign their
-- outward normal points along.
data Face
    = PosX
    | NegX
    | PosY
    | NegY
    | PosZ
    | NegZ
    deriving (Eq, Show, Enum, Bounded)

faceIndex :: Face -> Ordinal 6
faceIndex = unsafeOrdinal . fromEnum

indexFace :: Ordinal 6 -> Face
indexFace = toEnum . ordinalToInt

-- | Build a cube atlas from its 6 faces, given in 'Face'\'s own order
-- (@PosX, NegX, PosY, NegY, PosZ, NegZ@).
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

-- 'Axis' and 'Heading' are re-exported from "Data.Grid.Atlas.Rect" rather
-- than declared here: a face is a rectangular chart like any other, and the
-- coordinate half of a crossing (did this step leave the face, and where on
-- the destination does it land) is that module's 'rectStep'. Only the table
-- below is cube-specific.

-- | The transition table: crossing a named 'Axis' at a named 'Extremum' of a
-- 'Face' lands on another face, at a named axis and extremum of its own, and
-- says whether the free (along-edge) coordinate runs the same way (@False@)
-- or backwards (@True@) between the two.
cubeSeam :: SeamTable Face (Axis, Extremum)
cubeSeam = SeamTable crossCubeEdge

crossCubeEdge :: Face -> (Axis, Extremum) -> (Face, (Axis, Extremum), Bool)
crossCubeEdge PosX (U, AtMin) = (NegY, (U, AtMax), False)
crossCubeEdge PosX (U, AtMax) = (PosY, (V, AtMax), False)
crossCubeEdge PosX (V, AtMin) = (NegZ, (V, AtMax), False)
crossCubeEdge PosX (V, AtMax) = (PosZ, (U, AtMax), False)
crossCubeEdge NegX (U, AtMin) = (NegZ, (V, AtMin), False)
crossCubeEdge NegX (U, AtMax) = (PosZ, (U, AtMin), False)
crossCubeEdge NegX (V, AtMin) = (NegY, (U, AtMin), False)
crossCubeEdge NegX (V, AtMax) = (PosY, (V, AtMin), False)
crossCubeEdge PosY (U, AtMin) = (NegZ, (U, AtMax), False)
crossCubeEdge PosY (U, AtMax) = (PosZ, (V, AtMax), False)
crossCubeEdge PosY (V, AtMin) = (NegX, (V, AtMax), False)
crossCubeEdge PosY (V, AtMax) = (PosX, (U, AtMax), False)
crossCubeEdge NegY (U, AtMin) = (NegX, (V, AtMin), False)
crossCubeEdge NegY (U, AtMax) = (PosX, (U, AtMin), False)
crossCubeEdge NegY (V, AtMin) = (NegZ, (U, AtMin), False)
crossCubeEdge NegY (V, AtMax) = (PosZ, (V, AtMin), False)
crossCubeEdge PosZ (U, AtMin) = (NegX, (U, AtMax), False)
crossCubeEdge PosZ (U, AtMax) = (PosX, (V, AtMax), False)
crossCubeEdge PosZ (V, AtMin) = (NegY, (V, AtMax), False)
crossCubeEdge PosZ (V, AtMax) = (PosY, (U, AtMax), False)
crossCubeEdge NegZ (U, AtMin) = (NegY, (V, AtMin), False)
crossCubeEdge NegZ (U, AtMax) = (PosY, (U, AtMin), False)
crossCubeEdge NegZ (V, AtMin) = (NegX, (U, AtMin), False)
crossCubeEdge NegZ (V, AtMax) = (PosX, (V, AtMin), False)

-- | Move one cell in a heading, crossing a seam --- with its frame transform
-- applied to the heading itself --- if the step would leave the current
-- face. Total: a cube has no edge of its own, only seams, so every step
-- lands somewhere -- which is why this runs in 'Identity' where
-- "Data.Grid.Atlas.Mobius" runs the same 'rectStep' in 'Maybe'.
cubeStep ::
       forall n. KnownNat n
    => AtlasCoord '[ Ordinal n, Ordinal n] 6
    -> Heading
    -> (AtlasCoord '[ Ordinal n, Ordinal n] 6, Heading)
cubeStep (chart, u :| v :| EmptyCoord) heading =
    let (destFace, (u', v'), heading') =
            runIdentity $
            rectStep
                (const (ordinalSize @n))
                (\face edge -> Identity (crossSeam cubeSeam face edge))
                (indexFace chart)
                (ordinalToInt u, ordinalToInt v)
                heading
    in ( ( faceIndex destFace
         , unsafeOrdinal u' :| unsafeOrdinal v' :| EmptyCoord)
       , heading')
