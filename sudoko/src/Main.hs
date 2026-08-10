{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A sudoku board sliced into its rows, columns and 3x3 squares purely by
-- type-level shape: each slice is a @Grid@ of a smaller size cut out of the
-- 9x9 board, and the compiler checks that the shapes tile.
--
-- 'main' is still a demonstration rather than a solver: 'columns' and
-- 'squares' are built from 'mapLowerDim', which currently takes a cartesian
-- product of the sub-slices instead of zipping them (sized-grid-61o), so any
-- code that forces the whole list of columns will not terminate on a 9x9
-- board. The definitions below are the ones the solver will use once that is
-- fixed, and they all typecheck today.
module Main where

import           SizedGrid            hiding (All, Compose)

import           Data.Foldable        (toList)
import           Data.List            (intercalate)
import           Data.Maybe           (fromJust, isJust, listToMaybe)
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
        ([ [0, 0, 3, 0, 2, 0, 6, 0, 0]
         , [9, 0, 0, 3, 0, 5, 0, 0, 1]
         , [0, 0, 1, 8, 0, 6, 4, 0, 0]
         , [0, 0, 8, 1, 0, 2, 9, 0, 0]
         , [7, 0, 0, 0, 0, 0, 0, 0, 8]
         , [0, 0, 6, 7, 0, 8, 2, 0, 0]
         , [0, 0, 2, 6, 0, 9, 5, 0, 0]
         , [8, 0, 0, 2, 0, 3, 0, 0, 9]
         , [0, 0, 5, 0, 1, 0, 3, 0, 0]
         ]))

rows :: Board -> [Grid '[ Ordinal 1, Ordinal 9] (Maybe Symbol)]
rows = gridWindows

columns :: Board -> [Grid '[ Ordinal 9, Ordinal 1] (Maybe Symbol)]
columns = mapLowerDim gridWindows

squares :: Board -> [Grid '[ Ordinal 3, Ordinal 3] (Maybe Symbol)]
squares b = do
    a :: Grid '[ Ordinal 3, Ordinal 9] (Maybe Symbol) <- gridWindows b
    mapLowerDim gridWindows a

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
allUnique (a:as) = all (/= a) as && allUnique as

sliceSolved :: Eq a => [Maybe a] -> Bool
sliceSolved as = all isJust as && allUnique as

gameIsSolved :: Board -> Bool
gameIsSolved = getAll . withAllSlices (All . sliceSolved . toList)

gameIsInvalid :: Board -> Bool
gameIsInvalid = getAny . withAllSlices (Any . not . allUnique . toList)

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

-- | Forces only the first slice, never the length of the list.
firstSlice :: Foldable f => [f (Maybe Symbol)] -> String
firstSlice = maybe "<none>" displaySlice . listToMaybe

main :: IO ()
main = do
    putStrLn "Board:"
    putStr (displayBoard exampleGrid)
    putStrLn ""
    putStrLn ("first row:    " ++ firstSlice (rows exampleGrid))
    -- Only the first element is forced. 'columns' and 'squares' currently
    -- yield 9^9 results rather than 9 (sized-grid-61o), so
    -- 'length (columns exampleGrid)' would not terminate. The first element of
    -- that cartesian product is the "take the first window everywhere" choice,
    -- which coincides with the genuine first column -- so these two lines look
    -- right, and every later element does not.
    putStrLn ("first column: " ++ firstSlice (columns exampleGrid))
    putStrLn ("first square: " ++ firstSlice (squares exampleGrid))
