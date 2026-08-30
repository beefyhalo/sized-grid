{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The board itself: what a cell holds, how a 9x9 grid is cut into the
-- twenty-seven slices the rules are about, and how a board is read and
-- written as text.
--
-- The slicing is the part worth looking at. Rows, columns and 3x3 squares are
-- all @gridTiles@ at a different tile /type/ --- nothing here indexes, and
-- nothing here knows that 9 is 3 times 3 except the type signatures.
module Sudoku.Board
  ( -- * Cells and boards
    Symbol
  , Cs
  , Board
  , symbolChar
  , displaySymbol
  , coordRowCol
    -- * Slices
  , rows
  , columns
  , squares
  , rowAtPoint
  , columnAtPoint
  , squareAtPoint
  , withAllSlices
    -- * Predicates
  , gameIsSolved
  , gameIsInvalid
  , allValues
  , candidateAllowed
  , placeSymbol
    -- * Reading and writing
  , exampleBoard
  , parseBoard
  , displayBoard
  , displaySlice
  , showSlices
  ) where

import           Data.Grid.Sized      hiding (All, Compose)

import           Data.Foldable        (toList)
import           Data.List            (intercalate)
import           Data.Maybe           (catMaybes, isJust)
import           Data.Monoid          (All (..), Any (..))

newtype Symbol = Symbol (Ordinal 9)
  deriving stock   (Show)
  deriving newtype (Eq, Ord, Enum, Bounded)

-- | The digit a symbol prints as. @Ordinal 9@ counts from zero and sudoku
-- counts from one, and this is the only place that difference lives.
symbolChar :: Symbol -> Char
symbolChar (Symbol n) = toEnum (fromEnum '1' + ordinalToNum @Int n)

displaySymbol :: Maybe Symbol -> String
displaySymbol (Just s) = [symbolChar s]
displaySymbol Nothing  = "_"

-- | The board's shape. @Ordinal@ on both axes rather than @Clamped@ or
-- @Periodic@: a sudoku board has no edge behaviour to speak of, because
-- nothing ever steps off it.
type Cs = '[ Ordinal 9, Ordinal 9]

type Board = Grid Cs (Maybe Symbol)

-- | A coordinate as @(row, column)@. The first axis is the row, which is what
-- 'collapseGrid' and 'rows' below already assume; naming the two axes is all
-- this adds to 'coordIndices2'.
coordRowCol :: Coord Cs -> (Int, Int)
coordRowCol = coordIndices2

rows :: Board -> [Grid '[ Ordinal 1, Ordinal 9] (Maybe Symbol)]
rows = gridTiles

columns :: Board -> [Grid '[ Ordinal 9, Ordinal 1] (Maybe Symbol)]
columns = zipLowerDim gridTiles

squares :: Board -> [Grid '[ Ordinal 3, Ordinal 3] (Maybe Symbol)]
squares b = do
    band :: Grid '[ Ordinal 3, Ordinal 9] (Maybe Symbol) <- gridTiles b
    zipLowerDim gridTiles band

rowAtPoint ::
       Coord Cs
    -> Board
    -> Grid '[ Ordinal 1, Ordinal 9] (Maybe Symbol)
rowAtPoint (x :| _) b = rows b !! ordinalToNum x

columnAtPoint ::
       Coord Cs
    -> Board
    -> Grid '[ Ordinal 9, Ordinal 1] (Maybe Symbol)
columnAtPoint (_ :| y :| _) b = columns b !! ordinalToNum y

squareAtPoint ::
       Coord Cs
    -> Board
    -> Grid '[ Ordinal 3, Ordinal 3] (Maybe Symbol)
squareAtPoint (x :| y :| _) b =
    squares b !! (3 * (ordinalToNum x `div` 3) + (ordinalToNum y `div` 3))

withAllSlices ::
       Monoid x
    => (forall f. Foldable f =>
                      f (Maybe Symbol) -> x)
    -> Board
    -> x
withAllSlices func b =
    mconcat (map func (rows b) ++ map func (columns b) ++ map func (squares b))

allUnique :: Eq a => [a] -> Bool
allUnique []     = True
allUnique (a:as) = notElem a as && allUnique as

sliceSolved :: Eq a => [Maybe a] -> Bool
sliceSolved as = all isJust as && allUnique as

gameIsSolved :: Board -> Bool
gameIsSolved = getAll . withAllSlices (All . sliceSolved . toList)

gameIsInvalid :: Board -> Bool
gameIsInvalid = getAny . withAllSlices (Any . not . allUnique . catMaybes . toList)

allValues :: Board -> Grid Cs [Symbol]
allValues b =
    let helper Nothing  = [minBound .. maxBound]
        helper (Just x) = [x]
     in helper <$> b

candidateAllowed :: Coord Cs -> Board -> Symbol -> Bool
candidateAllowed point board symbol =
    notElem symbol (catMaybes (toList (rowAtPoint point board))) &&
    notElem symbol (catMaybes (toList (columnAtPoint point board))) &&
    notElem symbol (catMaybes (toList (squareAtPoint point board)))

placeSymbol :: Coord Cs -> Maybe Symbol -> Board -> Board
placeSymbol point symbol =
    imapGrid (\currentPoint value ->
        if currentPoint == point then symbol else value)

-- * Reading and writing

-- | Read a board from text.
--
-- Deliberately forgiving about layout: every character that is a digit
-- @1@-@9@ becomes a clue, every @0@, @.@ or @_@ becomes a blank, and
-- everything else --- newlines, spaces, @|@ and @-@ rules, comment text --- is
-- ignored. So the same parser reads a bare 81-character line, the nine-line
-- form the demo prints, and most of what a puzzle site will hand you. What it
-- will not do is guess: exactly 81 cells must be present, and the board must
-- not already break the rules.
parseBoard :: String -> Either String Board
parseBoard input
    | length cells /= 81 =
        Left $
        "expected 81 cells (digits 1-9 for clues, 0 . or _ for blanks), found " ++
        show (length cells)
    | otherwise =
        case gridFromList (chunk9 cells) of
            Nothing -> Left "internal error: 81 cells did not fill a 9x9 grid"
            Just b
                | gameIsInvalid b ->
                    Left "the board breaks the rules before the search starts"
                | otherwise -> Right b
  where
    cells = [c | Just c <- map cellOf input]
    cellOf ch
        | ch `elem` ("0._" :: String) = Just Nothing
        | ch >= '1' && ch <= '9' =
            Just (Symbol <$> numToOrdinal (fromEnum ch - fromEnum '1'))
        | otherwise = Nothing
    chunk9 [] = []
    chunk9 xs = let (a, b) = splitAt 9 xs in a : chunk9 b

-- | The board the demo falls back on with no file argument. Kept as text so
-- it reads as a board and goes through the same parser everything else does;
-- the 'error' is unreachable unless this literal is edited into an invalid
-- one.
exampleBoard :: Board
exampleBoard =
    case parseBoard exampleText of
        Right b  -> b
        Left err -> error ("the built-in example board is malformed: " ++ err)
  where
    exampleText =
        unlines
            [ "..3.2.6.."
            , "9..3.5..1"
            , "..18.64.."
            , "..81.29.."
            , "7.......8"
            , "..67.82.."
            , "..26.95.."
            , "8..2.3..9"
            , "..5.1.3.."
            ]

displayBoard :: Board -> String
displayBoard = unlines . map (concatMap displaySymbol) . collapseGrid

displaySlice :: Foldable f => f (Maybe Symbol) -> String
displaySlice = intercalate "," . map displaySymbol . toList

showSlices :: Foldable f => String -> [f (Maybe Symbol)] -> String
showSlices label slices =
    unlines $ (label ++ " (" ++ show (length slices) ++ "):")
            : map (("  " ++) . displaySlice) slices
