{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Helpers for small, rectangular, text-written grids.
module Data.Grid.Sized.Fixture
  ( Anchor (..),
    gridFromCharRows,
    renderCharRows,
    blitGrid,
  )
where

import Data.Grid.Sized.Coord
import Data.Grid.Sized.Internal.Grid.Core
  ( Grid,
    indexGrid,
    tabulateGrid,
  )
import Data.Grid.Sized.Ordinal (ordinalSize)
import Data.Maybe (fromMaybe)

-- | Placement of a source grid in a target grid.
data Anchor
  = BottomLeft
  | BottomRight
  | TopLeft
  | TopRight
  | Centre
  deriving (Eq, Show)

-- | Parse top-down rows into a two-dimensional grid.
--
-- The first axis is horizontal and the second is vertical. Rows must be
-- rectangular and exactly fill the type-level dimensions; characters for which
-- the parser returns 'Nothing' use the supplied fill value.
gridFromCharRows ::
  forall x y a.
  (IsCoordLifted x, IsCoordLifted y) =>
  (Char -> Maybe a) ->
  a ->
  String ->
  Maybe (Grid '[x, y] a)
gridFromCharRows parse fill input
  | length rows /= height || any ((/= width) . length) rows = Nothing
  | otherwise =
      Just $
        tabulateGrid $ \c ->
          let (ix, iy) = coordIndices2 c
              ch = rows !! (height - iy - 1) !! ix
           in fromMaybe fill (parse ch)
  where
    rows = lines input
    width = ordinalSize @(CoordNat x)
    height = ordinalSize @(CoordNat y)

-- | Render a two-dimensional grid as top-down rows.
renderCharRows ::
  forall x y a.
  (IsCoordLifted x, IsCoordLifted y, IsCoordList '[x, y]) =>
  (a -> Char) ->
  Grid '[x, y] a ->
  String
renderCharRows render grid =
  unlinesExceptLast
    [ [ render (indexGrid grid (inBoundsCoord [ix, iy])) | ix <- [0 .. width - 1]
      ]
    | iy <- [height - 1, height - 2 .. 0]
    ]
  where
    width = ordinalSize @(CoordNat x)
    height = ordinalSize @(CoordNat y)
    inBoundsCoord indices =
      case coordFromIndices indices of
        Just coord -> coord
        Nothing -> error "renderCharRows: generated an out-of-bounds coordinate"

-- | Overlay a source grid on a target grid, dropping cells outside the target.
blitGrid ::
  forall sx sy tx ty a.
  ( IsCoordLifted sx,
    IsCoordLifted sy,
    IsCoordLifted tx,
    IsCoordLifted ty,
    IsCoordList '[sx, sy],
    IsCoordList '[tx, ty]
  ) =>
  Anchor ->
  Grid '[sx, sy] a ->
  Grid '[tx, ty] a ->
  Grid '[tx, ty] a
blitGrid anchor source target =
  tabulateGrid $ \targetCoord ->
    let (tx, ty) = coordIndices2 targetCoord
        (ox, oy) = origin anchor
        sourceIndices = [tx - ox, ty - oy]
     in case coordFromIndices sourceIndices of
          Just sourceCoord -> indexGrid source sourceCoord
          Nothing -> indexGrid target targetCoord
  where
    sourceWidth = ordinalSize @(CoordNat sx)
    sourceHeight = ordinalSize @(CoordNat sy)
    targetWidth = ordinalSize @(CoordNat tx)
    targetHeight = ordinalSize @(CoordNat ty)
    origin BottomLeft = (0, 0)
    origin BottomRight = (targetWidth - sourceWidth, 0)
    origin TopLeft = (0, targetHeight - sourceHeight)
    origin TopRight = (targetWidth - sourceWidth, targetHeight - sourceHeight)
    origin Centre =
      ( (targetWidth - sourceWidth) `div` 2,
        (targetHeight - sourceHeight) `div` 2
      )

unlinesExceptLast :: [String] -> String
unlinesExceptLast [] = []
unlinesExceptLast [row] = row
unlinesExceptLast (row : rows) = row ++ "\n" ++ unlinesExceptLast rows
