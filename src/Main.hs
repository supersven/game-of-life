module Main where

main :: IO ()
main = putStr "Foo Bar"

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
