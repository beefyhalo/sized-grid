{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A sudoku board sliced into its rows, columns and 3x3 squares purely by
-- type-level shape, then solved by backtracking.
module Main (main) where

import           Data.Grid.Sized            hiding (All, Compose)

import           Data.Foldable        (asum, toList)
import           Data.List            (find, intercalate)
import           Data.Maybe           (catMaybes, fromJust, isJust, isNothing)
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

columns :: Board -> [Grid '[ Ordinal 9, Ordinal 1] (Maybe Symbol)]
columns = zipLowerDim gridTiles

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

gameIsInvalid :: Board -> Bool
gameIsInvalid = getAny . withAllSlices (Any . not . allUnique . catMaybes . toList)

allValues :: Board -> Grid '[ Ordinal 9, Ordinal 9] [Symbol]
allValues b =
    let helper Nothing  = [minBound .. maxBound]
        helper (Just x) = [x]
     in helper <$> b

candidateAllowed :: Coord '[ Ordinal 9, Ordinal 9] -> Board -> Symbol -> Bool
candidateAllowed point board symbol =
    notElem symbol (catMaybes (toList (rowAtPoint point board))) &&
    notElem symbol (catMaybes (toList (columAtPoint point board))) &&
    notElem symbol (catMaybes (toList (squareAtPoint point board)))

placeSymbol :: Coord '[ Ordinal 9, Ordinal 9] -> Symbol -> Board -> Board
placeSymbol point symbol =
    imapGrid (\currentPoint value ->
        if currentPoint == point then Just symbol else value)

solveBoard :: Board -> Maybe Board
solveBoard board
    | gameIsInvalid board = Nothing
    | otherwise =
        case findEmpty board of
            Nothing -> if gameIsSolved board then Just board else Nothing
            Just point ->
                asum
                    [ solveBoard (placeSymbol point symbol board)
                    | symbol <- indexGrid (allValues board) point
                    , candidateAllowed point board symbol
                    ]
  where
    findEmpty :: Board -> Maybe (Coord '[ Ordinal 9, Ordinal 9])
    findEmpty currentBoard =
        find (isNothing . indexGrid currentBoard) allCoord

displayBoard :: Board -> String
displayBoard = unlines . map (concatMap displaySymbol) . collapseGrid

displaySlice :: Foldable f => f (Maybe Symbol) -> String
displaySlice = intercalate "," . map displaySymbol . toList

showSlices :: Foldable f => String -> [f (Maybe Symbol)] -> String
showSlices label slices =
    unlines $ (label ++ " (" ++ show (length slices) ++ "):")
            : map (("  " ++) . displaySlice) slices

samplePoint :: Coord '[ Ordinal 9, Ordinal 9]
samplePoint =
    fromJust (numToOrdinal (4 :: Integer)) :|
    singleCoord (fromJust (numToOrdinal (4 :: Integer)))

main :: IO ()
main = do
    putStrLn "Board:"
    putStr (displayBoard exampleGrid)
    putStrLn ""
    putStr (showSlices "rows" (rows exampleGrid))
    putStr (showSlices "columns" (columns exampleGrid))
    putStr (showSlices "squares" (squares exampleGrid))
    putStrLn ""
    putStrLn ("solved:  " ++ show (gameIsSolved exampleGrid))
    putStrLn ("invalid: " ++ show (gameIsInvalid exampleGrid))
    putStrLn ""
    putStrLn "Slices through (4,4):"
    putStr (showSlices "row"    [rowAtPoint    samplePoint exampleGrid])
    putStr (showSlices "column" [columAtPoint  samplePoint exampleGrid])
    putStr (showSlices "square" [squareAtPoint samplePoint exampleGrid])
    putStrLn ("candidates at (4,4): " ++
              displaySlice (map Just (indexGrid (allValues exampleGrid) samplePoint)))
    putStrLn ""
    case solveBoard exampleGrid of
        Nothing -> putStrLn "No solution."
        Just solved -> do
            putStrLn "Solved board:"
            putStr (displayBoard solved)
