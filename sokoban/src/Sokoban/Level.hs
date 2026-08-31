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
  ( -- * A level's picture, before it has a size
    Layout (..),
    readLayout,

    -- * Levels
    SomeLevel (..),
    parseLevel,
    parseLevelAt,
    parseLevels,
    levelPicture,
    builtinLevels,
    firstLevel,
  )
where

import Data.Char (isSpace)
import Data.Grid.Sized
import Data.List (isPrefixOf, sort)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Sokoban.Board
import Sokoban.Rules

-- | A level whose size is only known at run time, which is every level read
-- from text.
data SomeLevel where
  SomeLevel :: (KnownStrip w h) => Level w h -> SomeLevel

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
glyphOf _ = Nothing

glyphTile :: Glyph -> Tile
glyphTile (OnlyTile t) = t
glyphTile (Crate t) = t
glyphTile (Player t) = t

-- | A level's picture, read and checked but not yet given a size.
--
-- The stage between text and a 'Level', and public because it is what a second
-- surface would consume. "Sokoban.Flat" plays the same layout on a cylinder
-- and on a plain rectangle in order to say whether a level needs the twist,
-- and it has to be reading the /same/ picture as the strip does for that
-- comparison to mean anything. One parser, two surfaces.
--
-- Everything here is in picture coordinates: @(column, row)@ with row zero at
-- the bottom, the way 'spotXY' counts, and not the way the text reads.
data Layout = Layout
  { layoutName :: String,
    -- | Which gluing the level asked for, already looked up. A level that
    -- names no surface gets a Mobius strip, which is what every level written
    -- before there was a choice meant.
    layoutSurface :: Surface,
    layoutNote :: String,
    layoutWalls :: Set (Int, Int),
    layoutGoals :: Set (Int, Int),
    layoutCrates :: Set (Int, Int),
    layoutPlayer :: (Int, Int),
    layoutWidth :: Int,
    layoutHeight :: Int
  }
  deriving (Eq, Show)

-- | Read one level, at whatever size its picture is.
parseLevel :: String -> Either String SomeLevel
parseLevel src = do
  lay <- readLayout src
  let bad = Left "a level must be at least one cell each way"
  fromMaybe bad $
    reifySize (layoutWidth lay) $ \w ->
      fromMaybe bad $
        reifySize (layoutHeight lay) $ \h ->
          SomeLevel <$> assemble @w @h lay

-- | Read one level at a size the caller already has in hand, failing if the
-- picture is a different shape. What a test with a picture written into it
-- wants, and what a level pack pinned to one strip would want.
parseLevelAt ::
  forall w h.
  (KnownStrip w h) =>
  String ->
  Either String (Level w h)
parseLevelAt src = do
  lay <- readLayout src
  let want = (ordinalSize @w, ordinalSize @h)
      got = (layoutWidth lay, layoutHeight lay)
  if want /= got
    then Left ("expected a picture " ++ show want ++ ", got " ++ show got)
    else assemble @w @h lay

readLayout :: String -> Either String Layout
readLayout = build . foldl' takeLine (Header "" "" "" []) . lines
  where
    takeLine acc raw =
      let line = dropTrailingCR raw
       in case line of
            (';' : _) -> acc
            _
              | Just v <- afterKey "name:" line -> acc {accName = v}
              | Just v <- afterKey "note:" line ->
                  acc {accNote = joinNote (accNote acc) v}
              | Just v <- afterKey "surface:" line -> acc {accSurface = v}
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
  { accName :: String,
    accSurface :: String,
    accNote :: String,
    accRows :: [String]
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
        (before, []) -> [unlines before]
        (before, _ : after) -> unlines before : splitOn p after
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

build :: Header -> Either String Layout
build (Header name wanted note rows) = do
  surface <-
    case wanted of
      "" -> Right mobius
      n ->
        maybe
          ( Left
              ( "unknown surface "
                  ++ show n
                  ++ " (one of "
                  ++ show (map surfaceName surfaces)
                  ++ ")"
              )
          )
          Right
          (surfaceNamed n)
  () <- unless' (null rows) "no picture: a level is at least one row of cells"
  () <-
    unless'
      (not (null bad))
      ( "unknown characters in the picture: "
          ++ show (map fst bad)
          ++ " (use # wall, space or - floor, . goal, $ crate, * crate on goal,\
             \ @ player, + player on goal)"
      )
  player <-
    case players of
      [p] -> Right p
      _ ->
        Left
          ( "expected exactly one player (@ or +), found "
              ++ show (length players)
          )
  () <- unless' (null crates) "no crates"
  () <-
    unless'
      (length crates /= length goals)
      ( "a level needs one goal per crate: "
          ++ show (length crates)
          ++ " crates, "
          ++ show (length goals)
          ++ " goals"
      )
  pure
    Layout
      { layoutName = name,
        layoutSurface = surface,
        layoutNote = note,
        layoutWalls = Set.fromList walls,
        layoutGoals = Set.fromList goals,
        layoutCrates = Set.fromList crates,
        layoutPlayer = player,
        layoutWidth = width,
        layoutHeight = height
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
      | (y, row) <- zip [0 ..] rows,
        (x, ch) <- zip [0 ..] (row ++ replicate (width - length row) '-')
      ]
    bad = [(ch, xy) | (xy, ch) <- placed, isNothing (glyphOf ch)]
    cells = M.fromList [(xy, g) | (xy, ch) <- placed, Just g <- [glyphOf ch]]
    players = [xy | (xy, Player _) <- M.toList cells]
    crates = sort [xy | (xy, Crate _) <- M.toList cells]
    goals = sort [xy | (xy, g) <- M.toList cells, glyphTile g == Goal]
    walls = [xy | (xy, g) <- M.toList cells, glyphTile g == Wall]

assemble ::
  forall w h.
  (KnownStrip w h) =>
  Layout ->
  Either String (Level w h)
assemble lay = do
  start <- at (layoutPlayer lay)
  crateSpots <- traverse at (Set.toList (layoutCrates lay))
  goalSpots <- traverse at (Set.toList (layoutGoals lay))
  pure
    Level
      { levelName = layoutName lay,
        levelSurface = layoutSurface lay,
        levelNote = layoutNote lay,
        levelBoard = boardFromGrid (layoutSurface lay) grid,
        levelGoals = Set.fromList (map spotCoord goalSpots),
        levelStart =
          Play
            { playPlayer = start,
              playTurn = square,
              playFacing = headingFor ChartFrame square DirRight,
              playCrates = Set.fromList (map spotCoord crateSpots),
              playMoves = 0,
              playPushes = 0
            }
      }
  where
    at :: (Int, Int) -> Either String (Spot w h)
    at (x, y) =
      maybe (Left ("cell " ++ show (x, y) ++ " is off the strip")) Right (spotAt x y)
    grid :: Grid (Strip w h) Tile
    grid =
      tabulateGrid $ \c ->
        let xy = spotXY (minBound, c)
         in if Set.member xy (layoutWalls lay)
              then Wall
              else
                if Set.member xy (layoutGoals lay)
                  then Goal
                  else Floor

-- | A level written back out as the picture it was read from. Round-trips a
-- level's terrain and its starting arrangement --- not a game in progress, and
-- not the header: what comes back is the cells, which is what every caller of
-- this wants, and a level's name and surface are already in hand wherever one
-- is.
levelPicture :: forall w h. (KnownStrip w h) => Level w h -> String
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

-- | The levels the game starts with, so the game runs with no arguments.
--
-- Kept as text and read by the same reader a file goes through, so a level
-- that would not load from disk cannot hide in here.
--
-- == The ramp
--
-- Ordered by idea rather than by size. Each level introduces exactly one thing
-- and its note says what; nothing later needs an idea that has not been shown.
-- The third is deliberately two pushes long, because the idea /is/ the level
-- and padding it out would only bury it.
--
-- The ramp is the eight on the Mobius strip. The two after it are on the other
-- two surfaces grid-atlas carries (sized-grid-lopy.7) and are a coda rather
-- than a continuation: a player who has finished the strip has met everything
-- the game teaches, and these say what the same rules look like when the
-- gluing changes underneath them.
--
-- Every level after the first is verified to have no solution on a cylinder of
-- the same shape or on a plain rectangle of the same shape --- see
-- "Sokoban.Flat" and @sokoban --check@. That is the standard: a level whose
-- answer does not need the half turn is a level about some other surface.
--
-- The first is the exception and is meant to be. It is a ring one cell wide,
-- it needs the wrap and not the twist, and it goes first precisely because a
-- player who has not yet believed that the two ends of the picture are the
-- same place cannot be taught anything else.
builtinLevels :: [SomeLevel]
builtinLevels =
  either (error . ("builtinLevels: " ++)) id (parseLevels builtinSource)

-- | The first built-in level. Total, because 'builtinLevels' is a literal this
-- module can see is not empty.
firstLevel :: SomeLevel
firstLevel =
  case builtinLevels of
    (l : _) -> l
    [] -> error "builtinLevels is empty"

builtinSource :: String
builtinSource =
  unlines
    [ "; Sokoban on a Mobius strip. There is no left or right edge: the first",
      "; and last columns of every picture are next to each other, and crossing",
      "; between them turns the strip over, so you arrive in the row on the",
      "; other side of the middle.",
      ";",
      "; The ramp is by idea. See Sokoban.Level's header, and check any level",
      "; you doubt with `sokoban --check`.",
      "name: No edge to stop at",
      "note: One ring, one cell wide. The wall blocks the way right, and going",
      "note: left there is no edge to stop at -- so the crate leaves at one end",
      "note: of the picture and arrives at the other. Nothing here needs the",
      "note: half turn yet. A cylinder would do, which is why this is first.",
      "-$@#-.-",
      "===",
      "name: The other row",
      "note: The crate has a wall above it and the end of the strip below it,",
      "note: so there is nowhere to stand to push it either way: sideways is",
      "note: all it has. Sideways goes through the seam, and the seam mirrors",
      "note: the axis it does not wrap -- so the crate comes back one row the",
      "note: other side of the middle, which is where the goal is.",
      "#####",
      "---.-",
      "-@$--",
      "#####",
      "===",
      "name: Go yourself",
      "note: The crate is already on the deck it belongs on and there is nobody",
      "note: behind it. Two solid rows separate the decks -- and separate",
      "note: nothing, because the seam joins the top row to the bottom one.",
      "note: Two pushes. Getting to them is the level.",
      "-@----",
      "######",
      "######",
      "-.-$--",
      "===",
      "name: The long way",
      "note: This time the wall is in the crate's own row, and there is no",
      "note: getting behind it: the way past a wall on a strip is not around",
      "note: it but off the end of the picture and back on the far row.",
      "#######",
      "---.---",
      "-$@#---",
      "#######",
      "===",
      "name: The far row",
      "note: A wall the whole way across, and it seals nothing: its two sides",
      "note: are the same side, one seam apart. That is true of every",
      "note: full-width wall on a Mobius strip and of none on a cylinder.",
      "------",
      "--.---",
      "######",
      "-@$---",
      "------",
      "===",
      "name: One stays, one goes",
      "note: Two crates, and a goal on each row. One of them has a short walk",
      "note: and the other has to go all the way round. Deciding which is",
      "note: which, before you push anything, is the level.",
      "######",
      "--.---",
      "-@$-$.",
      "######",
      "===",
      "name: Both at once",
      "note: Both crates have to cross, and the corridor is one crate wide.",
      "note: Send the wrong one first and there is nothing left behind the",
      "note: other to push it with.",
      "#######",
      "--.-.--",
      "-$@$---",
      "#######",
      "===",
      "name: Three",
      "note: The same corridor, one more crate, and no room to change your",
      "note: mind halfway. Fifteen pushes. Undo is on u.",
      "#######",
      "--.-..-",
      "-$@$-$-",
      "#######",
      "===",
      "; The coda. Same rules, same keys, a different gluing underneath.",
      "surface: klein",
      "name: The other deck",
      "note: A Klein bottle: sideways still turns you over, and now the top",
      "note: and bottom rows are joined too -- straight through, the way a",
      "note: cylinder joins, so this board has no edge anywhere at all. The",
      "note: wall gives the crate nowhere to go but sideways, and sideways is",
      "note: still the seam with the turn in it.",
      "------",
      "---.--",
      "######",
      "-@$---",
      "------",
      "===",
      "surface: projective",
      "name: Next door, the long way",
      "note: A projective plane: both pairs of edges turn you over. The goal",
      "note: is the next cell along and you cannot push the crate into it,",
      "note: because there is nowhere to stand on the far side. Off the bottom",
      "note: swaps your left with your right, so that is the way across --",
      "note: and the crate has to come all the way back down.",
      "##-@##",
      "##--##",
      "##--##",
      "##--##",
      "##$.##"
    ]
