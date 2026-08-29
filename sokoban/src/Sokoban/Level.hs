{-# LANGUAGE DataKinds #-}

-- | Levels, as pictures of themselves.
--
-- A level is written the way a Sokoban level has been written since 1982 ---
-- one character per cell, in the traditional character set --- so the source
-- of a level /is/ the level, and the same reader takes a file. Two departures
-- from the usual, both because of the surface:
--
--   * A hyphen is floor as well as a space. On a strip a whole row of floor is
--     a real and useful thing to draw, and a row of spaces is whatever the
--     last editor to touch the file decided it was. Anything that must survive
--     a round trip through a text editor cannot be made of trailing spaces.
--
--   * A level has no left or right wall, and drawing one is a mistake rather
--     than decoration: the first and last columns are adjacent through the
--     seam, so a wall at each end is one wall two cells thick, and the strip
--     is sealed. The top and bottom rows are the only genuine edges, and they
--     do not need a wall either, because stepping off them is already
--     refused.
--
-- == Sizes
--
-- The width of a level is the circumference of its strip, and that is part of
-- the puzzle rather than a detail of presentation --- a crate takes @w@ pushes
-- to come back mirrored and @2w@ to come home, so the same arrangement of
-- crates on a wider strip is a different problem. Levels therefore differ in
-- size, the size is in the type, and a list of levels is a list of
-- 'SomeLevel'.
module Sokoban.Level
  ( SomeLevel(..)
  , withPositive
  , parseLevel
  , parseLevelAt
  , parseLevels
  , levelPicture
  , builtinLevels
  , firstLevel
  ) where

import           Sokoban.Board
import           Sokoban.Rules

import           Data.Grid.Sized

import           Data.Char       (isSpace)
import           Data.List       (isPrefixOf, sort)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe, isNothing)
import           Data.Proxy      (Proxy (..))
import qualified Data.Set        as Set
import           Data.Type.Ord   (OrderingI (..))
import           GHC.TypeLits    (KnownNat, SomeNat (..), cmpNat, someNatVal,
                                  type (<=))

-- | A level whose size is only known at run time, which is every level read
-- from text.
data SomeLevel where
    SomeLevel :: KnownStrip w h => Level w h -> SomeLevel

-- | Reify a positive 'Int' as the @('KnownNat' n, 1 '<=' n)@ a 'Clamped' axis
-- asks for. 'Nothing' for zero or less, which is not a strip.
--
-- The @1 <= n@ half is the part that is not just 'someNatVal': 'cmpNat' is
-- what turns a run-time comparison into the type-level evidence, and both
-- 'LTI' and 'EQI' discharge it.
withPositive ::
       Int
    -> (forall n. (KnownNat n, 1 <= n) =>
                      Proxy n -> r)
    -> Maybe r
withPositive n k = do
    SomeNat (p :: Proxy n) <- someNatVal (fromIntegral (max 0 n))
    case cmpNat (Proxy @1) p of
        LTI -> Just (k p)
        EQI -> Just (k p)
        GTI -> Nothing

-- | What one character of a picture says: the terrain, and what is standing
-- on it.
data Glyph
    = OnlyTile Tile
    | Crate Tile
    | Player Tile

glyphOf :: Char -> Maybe Glyph
glyphOf '#' = Just (OnlyTile Wall)
glyphOf ' ' = Just (OnlyTile Floor)
glyphOf '-' = Just (OnlyTile Floor)
glyphOf '_' = Just (OnlyTile Floor)
glyphOf '.' = Just (OnlyTile Goal)
glyphOf '$' = Just (Crate Floor)
glyphOf '*' = Just (Crate Goal)
glyphOf '@' = Just (Player Floor)
glyphOf '+' = Just (Player Goal)
glyphOf _   = Nothing

glyphTile :: Glyph -> Tile
glyphTile (OnlyTile t) = t
glyphTile (Crate t)    = t
glyphTile (Player t)   = t

-- | A picture, read but not yet given a size. The stage between text and a
-- 'Level': everything is checked here except that the size is a size, because
-- checking that is what produces the type the level is at.
data Picture = Picture
    { picName   :: String
    , picNote   :: String
    , picCells  :: M.Map (Int, Int) Glyph
    , picPlayer :: (Int, Int)
    , picCrates :: [(Int, Int)]
    , picGoals  :: [(Int, Int)]
    , picWidth  :: Int
    , picHeight :: Int
    }

-- | Read one level, at whatever size its picture is.
parseLevel :: String -> Either String SomeLevel
parseLevel src = do
    pic <- readPicture src
    let bad = Left "a level must be at least one cell each way"
    fromMaybe bad $
        withPositive (picWidth pic) $ \(_ :: Proxy w) ->
            fromMaybe bad $
            withPositive (picHeight pic) $ \(_ :: Proxy h) ->
                SomeLevel <$> assemble @w @h pic

-- | Read one level at a size the caller already has in hand, failing if the
-- picture is a different shape. What a test with a picture written into it
-- wants, and what a level pack pinned to one strip would want.
parseLevelAt ::
       forall w h. KnownStrip w h
    => String
    -> Either String (Level w h)
parseLevelAt src = do
    pic <- readPicture src
    let want = (ordinalSize @w, ordinalSize @h)
        got = (picWidth pic, picHeight pic)
    if want /= got
        then Left ("expected a picture " ++ show want ++ ", got " ++ show got)
        else assemble @w @h pic

readPicture :: String -> Either String Picture
readPicture = build . foldl' takeLine (Header "" "" []) . lines
  where
    takeLine acc raw =
        let line = dropTrailingCR raw
        in case line of
               (';':_) -> acc
               _
                   | Just v <- afterKey "name:" line -> acc {accName = v}
                   | Just v <- afterKey "note:" line ->
                       acc {accNote = joinNote (accNote acc) v}
                   | all isSpace line -> acc
                   | otherwise -> acc {accRows = accRows acc ++ [line]}
    afterKey key line
        | key `isPrefixOf` line = Just (dropWhile isSpace (drop (length key) line))
        | otherwise = Nothing
    dropTrailingCR s = if not (null s) && last s == '\r' then init s else s
    -- A note is written as several 'note:' lines because it is prose in a
    -- source file with a right margin, and read back as one paragraph.
    joinNote "" v = v
    joinNote acc v = acc ++ " " ++ v

data Header = Header
    { accName :: String
    , accNote :: String
    , accRows :: [String]
    }

-- | Read a file of levels, separated by a line of three or more equals signs.
--
-- Neither a blank line nor a rule of hyphens will do, and both were tried: an
-- all-floor row is a real and common row on a strip, and it is written either
-- as nothing at all or as a run of hyphens. The separator has to be something
-- no picture can contain, which means a character that is not a cell.
parseLevels :: String -> Either String [SomeLevel]
parseLevels = traverse parseLevel . filter (not . blank) . splitOn isRule . lines
  where
    isRule l = let t = trim l in length t >= 3 && all (== '=') t
    blank = all (all isSpace) . lines
    splitOn p xs =
        case break p xs of
            (before, [])      -> [unlines before]
            (before, _:after) -> unlines before : splitOn p after
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

build :: Header -> Either String Picture
build (Header name note rows) = do
    () <- unless' (null rows) "no picture: a level is at least one row of cells"
    () <-
        unless'
            (not (null bad))
            ("unknown characters in the picture: " ++ show (map fst bad) ++
             " (use # wall, space or - floor, . goal, $ crate, * crate on goal,\
             \ @ player, + player on goal)")
    player <-
        case players of
            [p] -> Right p
            _ ->
                Left
                    ("expected exactly one player (@ or +), found " ++
                     show (length players))
    () <- unless' (null crates) "no crates"
    () <-
        unless'
            (length crates /= length goals)
            ("a level needs one goal per crate: " ++ show (length crates) ++
             " crates, " ++ show (length goals) ++ " goals")
    pure
        Picture
        { picName = name
        , picNote = note
        , picCells = cells
        , picPlayer = player
        , picCrates = crates
        , picGoals = goals
        , picWidth = width
        , picHeight = height
        }
  where
    unless' bad' msg =
        if bad'
            then Left msg
            else Right ()
    height = length rows
    width = foldr (max . length) 0 rows
    -- The picture reads top-down and the strip's second axis counts upwards,
    -- so the first row of the source is the top row of the board.
    placed =
        [ ((x, height - 1 - y), ch)
        | (y, row) <- zip [0 ..] rows
        , (x, ch) <- zip [0 ..] (row ++ replicate (width - length row) '-')
        ]
    bad = [(ch, xy) | (xy, ch) <- placed, isNothing (glyphOf ch)]
    cells = M.fromList [(xy, g) | (xy, ch) <- placed, Just g <- [glyphOf ch]]
    players = [xy | (xy, Player _) <- M.toList cells]
    crates = sort [xy | (xy, Crate _) <- M.toList cells]
    goals = sort [xy | (xy, g) <- M.toList cells, glyphTile g == Goal]

assemble ::
       forall w h. KnownStrip w h
    => Picture
    -> Either String (Level w h)
assemble pic = do
    start <- at (picPlayer pic)
    crateSpots <- traverse at (picCrates pic)
    goalSpots <- traverse at (picGoals pic)
    pure
        Level
        { levelName = picName pic
        , levelNote = picNote pic
        , levelBoard = boardFromGrid grid
        , levelGoals = Set.fromList (map spotCoord goalSpots)
        , levelStart =
              Play
              { playPlayer = start
              , playFlipped = False
              , playFacing = headingFor ChartFrame False DirRight
              , playCrates = Set.fromList (map spotCoord crateSpots)
              , playMoves = 0
              , playPushes = 0
              }
        }
  where
    at :: (Int, Int) -> Either String (Spot w h)
    at (x, y) =
        maybe (Left ("cell " ++ show (x, y) ++ " is off the strip")) Right (spotAt x y)
    grid :: Grid (Strip w h) Tile
    grid =
        tabulateGrid $ \c ->
            let (x, y) = spotXY (minBound, c)
            in maybe Floor glyphTile (M.lookup (x, y) (picCells pic))

-- | A level written back out as the picture it was read from. Round-trips a
-- level's terrain and its starting arrangement, not a game in progress.
levelPicture :: forall w h. KnownStrip w h => Level w h -> String
levelPicture lvl =
    unlines
        [ [charAt (x, y) | x <- [0 .. ordinalSize @w - 1]]
        | y <- reverse [0 .. ordinalSize @h - 1]
        ]
  where
    start = levelStart lvl
    charAt (x, y) =
        case spotAt @w @h x y of
            Nothing -> '?'
            Just s ->
                let onGoal = Set.member (spotCoord s) (levelGoals lvl)
                    hasCrate = crateAt start s
                    isPlayer = spotCoord s == spotCoord (playPlayer start)
                in case boardTile (levelBoard lvl) s of
                       Wall -> '#'
                       _
                           | isPlayer && onGoal -> '+'
                           | isPlayer -> '@'
                           | hasCrate && onGoal -> '*'
                           | hasCrate -> '$'
                           | onGoal -> '.'
                           | otherwise -> '-'

-- | The levels the game starts with, so it runs with no arguments.
--
-- Kept as text and read by the same reader a file goes through, so a level
-- that would not load from disk cannot hide in here. The deliberate
-- difficulty ramp, and the check that each level is solvable and is not
-- merely a flat puzzle in fancy dress, is sized-grid-lopy.3; these are the
-- ones the rules were written against.
builtinLevels :: [SomeLevel]
builtinLevels =
    either (error . ("builtinLevels: " ++)) id (parseLevels builtinSource)

-- | The first built-in level. Total, because 'builtinLevels' is a literal
-- this module can see is not empty.
firstLevel :: SomeLevel
firstLevel =
    case builtinLevels of
        (l:_) -> l
        []    -> error "builtinLevels is empty"

builtinSource :: String
builtinSource =
    unlines
        [ "; The strip's own levels. There is no left or right edge: the first and"
        , "; last columns are next to each other, and crossing between them turns"
        , "; the strip over."
        , "name: One cell wide"
        , "note: The whole level is one ring. The wall is in the way going right,"
        , "note: and going left there is no edge to stop at -- so the crate leaves"
        , "note: at one end of the picture and arrives at the other."
        , "-$@#-.-"
        , "==="
        , "name: The far row"
        , "note: Walls above and below: the crate can only go sideways, and"
        , "note: sideways is through the seam. The seam mirrors the axis it does"
        , "note: not wrap, so the crate arrives in the row on the other side of the"
        , "note: middle -- which is the only way anything reaches the goal. Roll"
        , "note: this strip into a cylinder instead and the level has no solution."
        , "------"
        , "--.---"
        , "######"
        , "-@$---"
        , "------"
        , "==="
        , "name: Twice around"
        , "note: One lap puts a crate in the mirrored row; only a second lap brings"
        , "note: it back to the row it left, and this goal is in the row it left."
        , "note: Pushing right is the trap: the crate fits between the player and"
        , "note: the wall, and nothing can get behind it again."
        , "------"
        , "------"
        , "######"
        , "-$@#.-"
        , "------"
        ]
