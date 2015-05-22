module Neighbours where

-- leftNeighbours :: [[Int]] -> [[Int]]
-- leftNeighbours cells = undefined

leftCell :: Int -> Int -> [[Int]] -> Int
leftCell _ col _ | col <= 0 = 0
leftCell row col board = cell row (col - 1) board

rightCell :: Int -> Int -> [[Int]] -> Int
rightCell _ col _ | col >= 2 = 0
rightCell row col board = cell row (col + 1) board

cell :: Int -> Int -> [[Int]] -> Int
cell row col board = (board !! row) !! col

upperCell :: Int -> Int -> [[Int]] -> Int
upperCell row _ _ | row <= 0 = 0
upperCell row col board = cell (row -1) col board

lowerCell :: Int -> Int -> [[Int]] -> Int
lowerCell row _ _ | row >= 2 = 0
lowerCell row col board = cell (row +1) col board

upperLeftCell :: Int -> Int -> [[Int]] -> Int
upperLeftCell row | row == 0 = \_ _ -> 0
upperLeftCell row = leftCell (row -1)

lowerRightCell :: Int -> Int -> [[Int]] -> Int
lowerRightCell row | row >= 2 = \_ _ -> 0
lowerRightCell row = rightCell (row +1)

upperRightCell :: Int -> Int -> [[Int]] -> Int
upperRightCell row | row == 0 = \_ _ -> 0
upperRightCell row = rightCell (row -1)

lowerLeftCell :: Int -> Int -> [[Int]] -> Int
lowerLeftCell row | row >= 2 = \_ _ -> 0
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
