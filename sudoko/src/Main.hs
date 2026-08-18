{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A sudoku board sliced into its rows, columns and 3x3 squares purely by
-- type-level shape: each slice is a @Grid@ of a smaller size cut out of the
-- 9x9 board, and the compiler checks that the shapes tile.
--
-- 'main' validates the board rather than solving it: the search itself is
-- still to be written.
module Main (main) where

import           Data.Grid.Sized            hiding (All, Compose)

import           Data.Foldable        (toList)
import           Data.List            (intercalate)
import           Data.Maybe           (catMaybes, fromJust, isJust)
import           Data.Monoid          (All (..), Any (..))

newtype Symbol = Symbol (Ordinal 9)
  deriving stock   (Show)
  deriving newtype (Eq, Ord, Enum, Bounded)

displaySymbol :: Maybe Symbol -> String
displaySymbol (Just (Symbol n)) = show (1 + ordinalToNum @Integer n)
displaySymbol _                 = "_"

type Board = Grid '[ Ordinal 9, Ordinal 9] (Maybe Symbol)

exampleGrid :: Board
exampleGrid =
    (\x -> Symbol <$> numToOrdinal (x - 1 :: Integer)) <$>
    fromJust (gridFromList
        [ [0, 0, 3, 0, 2, 0, 6, 0, 0]
         , [9, 0, 0, 3, 0, 5, 0, 0, 1]
         , [0, 0, 1, 8, 0, 6, 4, 0, 0]
         , [0, 0, 8, 1, 0, 2, 9, 0, 0]
         , [7, 0, 0, 0, 0, 0, 0, 0, 8]
         , [0, 0, 6, 7, 0, 8, 2, 0, 0]
         , [0, 0, 2, 6, 0, 9, 5, 0, 0]
         , [8, 0, 0, 2, 0, 3, 0, 0, 9]
         , [0, 0, 5, 0, 1, 0, 3, 0, 0]
         ])

rows :: Board -> [Grid '[ Ordinal 1, Ordinal 9] (Maybe Symbol)]
rows = gridTiles

-- | 'zipLowerDim', not 'mapLowerDim': the nine per-row slices are to be zipped
-- into nine columns, not multiplied into 9^9 combinations.
columns :: Board -> [Grid '[ Ordinal 9, Ordinal 1] (Maybe Symbol)]
columns = zipLowerDim gridTiles

-- | Three horizontal bands of three squares each, in band-major order.
squares :: Board -> [Grid '[ Ordinal 3, Ordinal 3] (Maybe Symbol)]
squares b = do
    band :: Grid '[ Ordinal 3, Ordinal 9] (Maybe Symbol) <- gridTiles b
    zipLowerDim gridTiles band

rowAtPoint ::
       Coord '[ Ordinal 9, Ordinal 9]
    -> Board
    -> Grid '[ Ordinal 1, Ordinal 9] (Maybe Symbol)
rowAtPoint (x :| _) b = rows b !! ordinalToNum x

columAtPoint ::
       Coord '[ Ordinal 9, Ordinal 9]
    -> Board
    -> Grid '[ Ordinal 9, Ordinal 1] (Maybe Symbol)
columAtPoint (_ :| y :| _) b = columns b !! ordinalToNum y

-- | The board is cut into three horizontal bands of three squares each, so the
-- square holding @(x, y)@ is at @3 * (x `div` 3) + (y `div` 3)@. This used to
-- say @mod@ rather than @div@, which addresses the squares in a repeating
-- pattern instead of walking them once.
squareAtPoint ::
       Coord '[ Ordinal 9, Ordinal 9]
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

-- | Only the filled cells are checked for duplicates. Running 'allUnique' over
-- the @Maybe@s directly calls every partially-filled board invalid, because two
-- blanks are two equal 'Nothing's.
gameIsInvalid :: Board -> Bool
gameIsInvalid = getAny . withAllSlices (Any . not . allUnique . catMaybes . toList)

allValues :: Board -> Grid '[ Ordinal 9, Ordinal 9] [Symbol]
allValues b =
    let helper Nothing  = [minBound .. maxBound]
        helper (Just x) = [x]
     in helper <$> b

displayBoard :: Board -> String
displayBoard = unlines . map (concatMap displaySymbol) . collapseGrid

-- | Renders any one-dimensional-ish slice as a flat list, so a row and a column
-- can be compared side by side.
displaySlice :: Foldable f => f (Maybe Symbol) -> String
displaySlice = intercalate "," . map displaySymbol . toList

showSlices :: Foldable f => String -> [f (Maybe Symbol)] -> String
showSlices label slices =
    unlines $ (label ++ " (" ++ show (length slices) ++ "):")
            : map (("  " ++) . displaySlice) slices

-- | A point picked purely to demonstrate the by-point lookups below on an
-- empty cell, where the candidate list is worth looking at; nothing about
-- the coordinate itself is special to the puzzle.
samplePoint :: Coord '[ Ordinal 9, Ordinal 9]
samplePoint =
    fromJust (numToOrdinal (4 :: Integer)) :|
    singleCoord (fromJust (numToOrdinal (4 :: Integer)))

main :: IO ()
main = do
    putStrLn "Board:"
    putStr (displayBoard exampleGrid)
    putStrLn ""
    -- Each of these is 9 slices. Printing them all is the point: it is what
    -- makes a slicing bug visible, and it is only affordable because
    -- 'zipLowerDim' zips the per-row results instead of multiplying them.
    putStr (showSlices "rows" (rows exampleGrid))
    putStr (showSlices "columns" (columns exampleGrid))
    putStr (showSlices "squares" (squares exampleGrid))
    putStrLn ""
    putStrLn ("solved:  " ++ show (gameIsSolved exampleGrid))
    putStrLn ("invalid: " ++ show (gameIsInvalid exampleGrid))
    putStrLn ""
    -- The by-point lookups a solver would call once per cell, exercised here
    -- at a single sample point rather than over the whole board.
    putStrLn "Slices through (4,4):"
    putStr (showSlices "row"    [rowAtPoint    samplePoint exampleGrid])
    putStr (showSlices "column" [columAtPoint  samplePoint exampleGrid])
    putStr (showSlices "square" [squareAtPoint samplePoint exampleGrid])
    putStrLn ("candidates at (4,4): " ++
              displaySlice (map Just (indexGrid (allValues exampleGrid) samplePoint)))
