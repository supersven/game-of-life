module Main where
import           Control.Concurrent
import           Neighbours
import           System.Console.ANSI

main :: IO ()
main = game [[0,1,1],[0,0,0],[0,1,0]] 50

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
                         1 -> putChar '*'
                         0 -> putChar '.'
                        printRow cells
