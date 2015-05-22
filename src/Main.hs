module Main where
import           Control.Concurrent
import           Neighbours
import           System.Console.ANSI

main :: IO ()
main = game (generateField 40 40) 50

game :: [[Int]] -> Int -> IO()
game _ 0 = return()
game board runsLeft = do
                        printBoard board
                        putChar '\n'
                        let nextGen = nextGeneration board
                        threadDelay 1000000
                        clearScreen
                        game nextGen (runsLeft - 1)

printBoard :: [[Int]] -> IO()
printBoard [] = return()
printBoard (r:rows) = do printRow r
                         putChar '\n'
                         printBoard rows

printRow :: [Int] -> IO()
printRow [] = return()
printRow (c:cells) = do case c of
                         1 -> do setSGR [SetColor Foreground Vivid Green]
                                 putChar '+'
                                 setSGR [SetColor Foreground Vivid Red]

                         0 -> putChar '-'
                        printRow cells

generateField :: Int -> Int -> [[Int]]
generateField 0 _  = []
generateField rows cols = generateRow cols : generateField (rows -1) cols

generateRow :: Int -> [Int]
generateRow cols = [ if even v then 1 else 0 | v <- [1..cols]]
