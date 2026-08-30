-- | The strip drawn as what it is: a band with a half twist in it.
--
-- Every other view in this game draws the chart --- a rectangle --- and then
-- tells the player that its two ends are the same end. "Sokoban.Render"'s flat
-- view says it with coloured tabs, the centred view says it by never showing
-- an end at all, and the terminal view says it in a sentence. All three are
-- asking the player to believe something. A band with a half twist in it does
-- not ask: the surface has one edge and no ends, and that is visible.
--
-- Not the view to play in --- sized-grid-lopy.1 settled that, and nothing here
-- is meant to replace it. This is the title screen, the pause between levels,
-- and the picture at the top of the README.
--
-- == The parametrisation, and why it is the right one
--
-- 'bandPoint' is the textbook Mobius strip: a circle of radius @R@ carrying a
-- segment that turns half a revolution over one lap. What makes it the right
-- one /here/ is that its gluing is the game's gluing and not merely one like
-- it. Advancing the column by a full lap negates the offset from the band's
-- centre line, so
--
-- > bandPoint b w h (u + w) (h - v) == bandPoint b w h u v
--
-- which is @(x, y) -> (x, h - 1 - y)@ on cell centres --- exactly where
-- 'Sokoban.Board.stepSpot' puts you after @w@ steps sideways. The picture is
-- not an illustration of the surface the game is played on. It is the same
-- surface, and @Test.Band@ asserts that against the game's own walker rather
-- than against this comment.
--
-- == Drawing
--
-- gloss is 2D, so the projection is done here: rotate, divide by depth, and
-- sort the facets back to front. A painter's algorithm is enough because the
-- band is convex-ish in depth and every facet is small; there is no depth
-- buffer to want.
--
-- Facets are shaded by the angle they face, and the shade is the /absolute/
-- value of that angle's cosine. That is the one lighting decision worth
-- reading twice: a one-sided surface has no back face, so darkening the
-- facets pointing away would be drawing a distinction the surface does not
-- have.
module Sokoban.Band
  ( -- * How the band is drawn
    Band (..),
    defaultBand,

    -- * The surface
    bandRadius,
    bandPoint,

    -- * Pictures
    bandPicture,
    bandExtent,
  )
where

import Data.List (sortOn)
import Graphics.Gloss.Data.Color (Color, makeColor, rgbaOfColor)
import Graphics.Gloss.Data.Picture (Picture, blank, color, pictures, polygon)

-- | The band's proportions, where it is being looked at from, and how finely
-- it is cut.
data Band = Band
  { -- | The ring's radius, as a multiple of the strip's width across. Under
    -- about 0.8 the band's inner edge reaches the middle of the ring and the
    -- surface passes through itself; the classic picture is nearer 1.
    bandGirth :: !Float,
    -- | Turns of the band about its own axis. One is a full revolution, and
    -- an animation is this and nothing else.
    bandSpin :: !Float,
    -- | How far the ring is tipped towards the viewer, in turns. Zero looks
    -- straight down on it and sees a circle with no twist visible at all; a
    -- quarter sees it edge on.
    bandTilt :: !Float,
    -- | The viewer's distance, as a multiple of the radius. Larger is a
    -- longer lens: less perspective, and a flatter picture.
    bandDistance :: !Float,
    -- | Pixels per unit, before the whole picture is scaled to the window.
    bandScale :: !Float,
    -- | How many facets each cell is cut into around the strip. One draws the
    -- band as a @w@-sided polygon; the curve needs more than that on a short
    -- strip and does not need many.
    bandFacets :: !Int,
    -- | Half the width of the line the band's one edge is drawn with, in
    -- pixels. Zero leaves it out.
    bandRim :: !Float
  }

defaultBand :: Band
defaultBand =
  Band
    { bandGirth = 1.6,
      bandSpin = 0.25,
      bandTilt = 0.2,
      bandDistance = 8,
      bandScale = 34,
      bandFacets = 12,
      bandRim = 1.6
    }

-- | The ring's radius for a strip of this shape, in the units 'bandPoint'
-- answers in --- one cell across the strip is one unit.
--
-- Two things set it, and the larger wins. A band cannot be much wider than
-- its ring without running through its own middle, which is 'bandGirth'; and
-- a long strip wrapped onto a small ring would have cells stretched flat
-- around it, which the second term prevents by growing the ring until a cell
-- is about square. Squat levels get the first, long ones the second.
bandRadius :: Band -> Int -> Int -> Float
bandRadius band around across =
  max
    (bandGirth band * fromIntegral across)
    (fromIntegral around / (2 * pi))

-- | A point of the surface, in space, at column @u@ and row @v@.
--
-- Both are continuous and neither is bounded: the corners of a cell, the
-- points between them, and the second lap of the edge are all named the same
-- way. Cell @(x, y)@ has its centre at @(x + 0.5, y + 0.5)@.
bandPoint :: Band -> Int -> Int -> Float -> Float -> (Float, Float, Float)
bandPoint band around across u v =
  (r * cos theta, r * sin theta, t * sin half)
  where
    theta = 2 * pi * u / fromIntegral around
    half = theta / 2
    -- The offset from the band's centre line. One cell across is one unit,
    -- and this is what a lap negates.
    t = v - fromIntegral across / 2
    r = bandRadius band around across + t * cos half

-- * Looking at it

-- | Space as the viewer sees it: the ring spun about its own axis and tipped
-- towards them, before anything is divided by depth.
--
-- Kept apart from the projection because facets are shaded from their normals
-- and a normal has to be measured before the perspective divide, which is not
-- linear. Doing the rotation here also puts the light in the viewer's frame
-- rather than the band's, so a spinning band is lit from one place instead of
-- carrying its own highlight around with it.
toCamera :: Band -> (Float, Float, Float) -> (Float, Float, Float)
toCamera band (x, y, z) = (xs, ys * cos tilt - z * sin tilt, ys * sin tilt + z * cos tilt)
  where
    spin = 2 * pi * bandSpin band
    tilt = 2 * pi * bandTilt band
    xs = x * cos spin - y * sin spin
    ys = x * sin spin + y * cos spin

-- | A camera-space point on the screen. The third component of its argument
-- is depth, larger towards the viewer, and it is what the sort is on.
toScreen :: Band -> Float -> (Float, Float, Float) -> (Float, Float)
toScreen band d (x, y, z) = (k * x, k * y)
  where
    k = bandScale band * d / (d - z)

-- | Where the light comes from, in the viewer's frame: over the left
-- shoulder. A unit vector, so 'shade' can dot with it directly.
lightDir :: (Float, Float, Float)
lightDir = unit (-0.4, 0.5, 0.75)

-- | How lit a facet with this normal is, between 'ambient' and one.
--
-- Absolute, because the strip has one side. A facet turned away from the
-- viewer is the same piece of surface seen from the other direction, and
-- there is no other direction.
shade :: (Float, Float, Float) -> Float
shade n = ambient + (1 - ambient) * abs (dot (unit n) lightDir)
  where
    ambient = 0.42

-- | Scale a colour's brightness, keeping its alpha.
dim :: Float -> Color -> Color
dim k c = makeColor (k * r) (k * g) (k * b) a
  where
    (r, g, b, a) = rgbaOfColor c

dot :: (Float, Float, Float) -> (Float, Float, Float) -> Float
dot (a, b, c) (x, y, z) = a * x + b * y + c * z

cross :: (Float, Float, Float) -> (Float, Float, Float) -> (Float, Float, Float)
cross (a, b, c) (x, y, z) = (b * z - c * y, c * x - a * z, a * y - b * x)

sub3 :: (Float, Float, Float) -> (Float, Float, Float) -> (Float, Float, Float)
sub3 (a, b, c) (x, y, z) = (a - x, b - y, c - z)

unit :: (Float, Float, Float) -> (Float, Float, Float)
unit v@(x, y, z)
  | len == 0 = (0, 0, 1)
  | otherwise = (x / len, y / len, z / len)
  where
    len = sqrt (dot v v)

-- * The picture

-- | One drawn thing and how near the viewer it is.
data Facet = Facet !Float Picture

-- | The whole band, with @paint@ giving the colour of the cell at each
-- @(column, row)@.
--
-- Facets are sorted far to near and drawn in that order. Ties keep their
-- order, which is why the edge is appended after the cells: where the edge and
-- the cell it borders are the same distance away, the edge wins.
bandPicture :: Band -> Int -> Int -> (Int -> Int -> Color) -> Picture
bandPicture band around across paint =
  pictures [p | Facet _ p <- sortOn (\(Facet z _) -> z) (cells ++ rim)]
  where
    radius = bandRadius band around across
    d = bandDistance band * radius
    at u v = toCamera band (bandPoint band around across u v)
    flat = toScreen band d
    facets = max 1 (bandFacets band)
    depth ps = sum [z | (_, _, z) <- ps] / fromIntegral (length ps)

    -- Every cell is drawn inside its own darker outline, because otherwise a
    -- run of cells of one colour --- a wall along the whole strip, which most
    -- of these levels have --- is a single blank ribbon and the board is not
    -- on screen at all.
    --
    -- The two insets are different numbers because a cell is not square. A
    -- strip @w@ cells around a ring of this radius has cells @2*pi*r/w@ long
    -- and one unit wide, so an inset that looks right across the strip would
    -- be several times too wide around it.
    gapV = 0.06
    gapU = min 0.3 (gapV * fromIntegral around / (2 * pi * radius))

    cells =
      [ tile
          (paint x y)
          (at u0 v0, at u1 v0, at u1 v1, at u0 v1)
          (at iu0 iv0, at iu1 iv0, at iu1 iv1, at iu0 iv1)
      | x <- [0 .. around - 1],
        y <- [0 .. across - 1],
        s <- [0 .. facets - 1],
        let wide = 1 / fromIntegral facets
            u0 = fromIntegral x + fromIntegral s * wide
            u1 = u0 + wide
            v0 = fromIntegral y
            v1 = v0 + 1
            -- Only at the cell's own ends: a gap at every facet boundary
            -- would draw the subdivision, which is an artefact of how the
            -- curve is cut and not anything about the board.
            iu0 = if s == 0 then u0 + gapU else u0
            iu1 = if s == facets - 1 then u1 - gapU else u1
            iv0 = v0 + gapV
            iv1 = v1 - gapV
      ]

    -- One cell, or a facet of one: its outline, and the cell inside it. Both
    -- at the same distance, so they cannot be separated by the sort.
    tile c outer@(a, b, b', a') inner =
      Facet
        (depth [a, b, b', a'])
        ( pictures
            [ color (dim (0.4 * light) c) (poly outer),
              color (dim light c) (poly inner)
            ]
        )
      where
        light = shade (cross (b `sub3` a) (a' `sub3` a))
    poly (a, b, b', a') = polygon [flat a, flat b, flat b', flat a']

    -- The strip's one edge, drawn as one curve because it is one curve: two
    -- laps of the ring, arriving back where it set out. Nothing else in this
    -- game says as plainly that the top and bottom rows of the picture are the
    -- same edge.
    rim
      | bandRim band <= 0 = []
      | otherwise = zipWith segment edgePoints (drop 1 edgePoints)
    edgeSteps = 2 * around * facets
    edgePoints =
      [ at (2 * fromIntegral around * fromIntegral i / fromIntegral edgeSteps) 0
      | i <- [0 .. edgeSteps]
      ]
    -- Nudged towards the viewer, because the edge lies /on/ the surface: it
    -- and the cells it borders are at the same distance, and without this
    -- they take bites out of each other wherever the two roundings disagree.
    -- A hundredth of the ring is far less than the near side of the band is
    -- in front of the far side, so it cannot bring the back edge forward.
    segment a b =
      Facet
        (depth [a, b] + radius / 100)
        (color rimColour (ribbon (bandRim band) (flat a) (flat b)))

-- | The band's one edge. Loud on purpose: it is the claim the picture is
-- making.
rimColour :: Color
rimColour = makeColor 0.95 0.35 0.75 1

-- | A screen-space quad along a segment, gloss having no thick line.
ribbon :: Float -> (Float, Float) -> (Float, Float) -> Picture
ribbon w (x0, y0) (x1, y1)
  | len == 0 = blank
  | otherwise =
      polygon
        [ (x0 + nx, y0 + ny),
          (x1 + nx, y1 + ny),
          (x1 - nx, y1 - ny),
          (x0 - nx, y0 - ny)
        ]
  where
    (dx, dy) = (x1 - x0, y1 - y0)
    len = sqrt (dx * dx + dy * dy)
    (nx, ny) = (-(dy / len * w), dx / len * w)

-- | A box the drawn band fits inside, centred on the origin, so a caller can
-- scale it to a window.
--
-- Measured off the projection rather than predicted from the radius. The tilt
-- and the perspective both move the outline, and predicting a gloss layout
-- instead of measuring it is the mistake sized-grid-23y3 is a monument to.
bandExtent :: Band -> Int -> Int -> (Float, Float)
bandExtent band around across =
  (2 * reach (map fst corners), 2 * reach (map snd corners))
  where
    reach = foldr (max . abs) 1
    d = bandDistance band * bandRadius band around across
    corners =
      [ toScreen band d (toCamera band (bandPoint band around across u v))
      | i <- [0 .. steps],
        let u = fromIntegral around * fromIntegral i / fromIntegral steps,
        v <- [0, fromIntegral across]
      ]
    steps = max 8 (around * max 1 (bandFacets band))
