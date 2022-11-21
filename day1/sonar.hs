import Data.List
import System.IO (openFile, IOMode (ReadMode), hGetContents)

main :: IO()

main = do 
    depths <- readInput
    let windows = slidingWindow depths
    let num_increases = countIncreases windows
    print num_increases


readInput :: IO[Int]
readInput = do 
    handle <- openFile "input.txt" ReadMode
    input <- hGetContents handle
    let rows = lines input
    let numbers = map read rows
    return numbers

slidingWindow :: [Int] -> [(Int, Int)]
slidingWindow numbers = do 
    zip numbers (mconcat [tail numbers, [0]])

countIncreases :: [(Int, Int)] -> Int 
countIncreases windows = do 
    sum (map (\(a,b) -> if a > b then 0 else 1) windows)