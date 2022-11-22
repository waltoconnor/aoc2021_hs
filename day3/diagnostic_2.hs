import System.IO (openFile, IOMode (ReadMode), hGetContents)
import Data.List (transpose)
import Data.Bits

main :: IO()

main = do 
    matrix <- readInput 
    let transposed = transpose matrix 
    let most_common_per_column = map getMostCommon_o2 transposed
    let least_common_per_column = map getLeastCommon_co2 transposed
    let oxygen = convertBinary (filterMatrix matrix True 0)
    let co2 = convertBinary (filterMatrix matrix False 0)
    print ("Oxygen: " ++ (show oxygen))
    print ("CO2: " ++ (show co2))
    print (oxygen * co2)

readInput :: IO[[Int]]
readInput = do 
    handle <- openFile "input.txt" ReadMode
    input <- hGetContents handle
    let rows = lines input
    -- let numbers = map (\row -> map read (map (\c -> [c]) row)) rows
    let numbers = map (map read . map (\c -> [c])) rows
    return numbers

getMostCommon_o2 :: [Int] -> Int 
getMostCommon_o2 numbers = do 
    let count_0 = sum (map (\x -> if x == 0 then 1 else 0) numbers)
    let count_1 = length numbers - count_0
    if count_1 >= count_0 then 1 else 0

getLeastCommon_co2 :: [Int] -> Int 
getLeastCommon_co2 numbers = do 
    let count_0 = sum (map (\x -> if x == 0 then 1 else 0) numbers)
    let count_1 = length numbers - count_0
    if count_1 < count_0 then 1 else 0


convertBinary :: [Int] -> Int
convertBinary arr = foldl (\acc x -> (acc * 2) + x) 0 arr 

invertList :: [Int] -> [Int]
invertList l = map (\x -> if x == 0 then 1 else 0) l

filterMatrix :: [[Int]] -> Bool -> Int -> [Int]
filterMatrix [row] list place = row
filterMatrix matrix mostCommon place = do
    let list = if mostCommon then map getMostCommon_o2 (transpose matrix) else map getLeastCommon_co2 (transpose matrix)  
    let value_n = list !! place
    let filtered = filter (\row -> (row !! place) == value_n) matrix
    filterMatrix filtered mostCommon (place + 1)
    -- this has no handling for malformed input and will loop or crash

 

