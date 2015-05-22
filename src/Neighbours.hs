module Neighbours where

notInBounds :: Int -> Int -> [[Int]] -> Bool
notInBounds row col board | col < 0 || row < 0 || col >= length board || row >= length board  = True
                          | otherwise = False

leftCell :: Int -> Int -> [[Int]] -> Int
leftCell row col = cell row (col - 1)

rightCell :: Int -> Int -> [[Int]] -> Int
rightCell row col = cell row (col + 1)

cell :: Int -> Int -> [[Int]] -> Int
cell row col board | notInBounds row col board = 0
cell row col board = (board !! row) !! col

upperCell :: Int -> Int -> [[Int]] -> Int
upperCell row = cell (row -1)

lowerCell :: Int -> Int -> [[Int]] -> Int
lowerCell row = cell (row +1)

upperLeftCell :: Int -> Int -> [[Int]] -> Int
upperLeftCell row = leftCell (row -1)

lowerRightCell :: Int -> Int -> [[Int]] -> Int
lowerRightCell row = rightCell (row +1)

upperRightCell :: Int -> Int -> [[Int]] -> Int
upperRightCell row = rightCell (row -1)

lowerLeftCell :: Int -> Int -> [[Int]] -> Int
lowerLeftCell row = leftCell (row + 1)

neighboursOfCell :: Int -> Int -> [[Int]] -> Int
neighboursOfCell row col board = leftCell row col board + rightCell row col board
                                 + upperCell row col board + lowerCell row col board
                                 + upperLeftCell row col board + lowerLeftCell row col board
                                 + upperRightCell row col board + lowerRightCell row col board

-- todo: use zip3?
zipBoardWithIndices :: Int -> Int -> [[Int]] -> [[(Int,Int,Int)]]
zipBoardWithIndices _ _ [] = []
zipBoardWithIndices firstRow firstCol (x:xs) = (zipRowWithIndices firstRow firstCol x) : (zipBoardWithIndices (firstRow + 1) firstCol xs)

zipRowWithIndices :: Int -> Int -> [Int] -> [(Int,Int,Int)]
zipRowWithIndices _ _ [] = []
zipRowWithIndices rowNum firstCol (v:vs) = (zipColWithIndices rowNum firstCol  v) : (zipRowWithIndices rowNum (firstCol + 1) vs)

zipColWithIndices :: Int -> Int -> Int -> (Int, Int, Int)
zipColWithIndices rowNum colNum value  = (rowNum, colNum, value)

neighbours :: [[Int]] -> [[Int]]
neighbours board = neighboursBoard board (zipBoardWithIndices 0 0 board)

neighboursBoard :: [[Int]] -> [[(Int,Int,Int)]] -> [[Int]]
neighboursBoard _ [] = []
neighboursBoard board (r:rows) = neighboursRow board r : neighboursBoard board rows

neighboursRow :: [[Int]] -> [(Int,Int,Int)] -> [Int]
neighboursRow _ [] = []
neighboursRow board (x:xs) = neighboursTupel board x :neighboursRow board xs

neighboursTupel :: [[Int]] -> (Int, Int, Int)  -> Int
neighboursTupel board (row, col, _) = neighboursOfCell row col board

nextGeneration :: [[Int]] -> [[Int]]
nextGeneration prevGeneration = map (map aliveInNextGen) (neighbours prevGeneration)

aliveInNextGen :: Int -> Int
aliveInNextGen numOfNeighbours | numOfNeighbours <= 1 = 0
                               | numOfNeighbours <= 3 = 1
                               | otherwise = 0
