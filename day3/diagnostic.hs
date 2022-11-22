import System.IO (openFile, IOMode (ReadMode), hGetContents)
import Data.List (transpose)
import Data.Bits

main :: IO()

main = do 
    matrix <- readInput 
    let transposed = transpose matrix 
    let most_common_per_column = map getMostCommon transposed
    let gamma = convertBinary most_common_per_column
    print gamma
    let epsilon = convertBinary (invertList most_common_per_column)
    print epsilon
    print (epsilon * gamma)

readInput :: IO[[Int]]
readInput = do 
    handle <- openFile "input.txt" ReadMode
    input <- hGetContents handle
    let rows = lines input
    -- let numbers = map (\row -> map read (map (\c -> [c]) row)) rows
    let numbers = map (map read . map (\c -> [c])) rows
    return numbers

getMostCommon :: [Int] -> Int 
getMostCommon numbers = do 
    let count_0 = sum (map (\x -> if x == 0 then 1 else 0) numbers)
    let count_1 = length numbers - count_0
    if count_0 >= count_1 then 0 else 1

convertBinary :: [Int] -> Int

convertBinary arr = foldl (\acc x -> (acc * 2) + x) 0 arr 

invertList :: [Int] -> [Int]
invertList l = map (\x -> if x == 0 then 1 else 0) l