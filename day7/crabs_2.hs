import System.IO (openFile, IOMode (ReadMode), hGetContents)
import Data.Text (splitOn, pack, unpack, Text)
import Data.Text.IO
import Data.List (sort)

type Crabs = [Int]

-- brute force approach, but it still finds the answer in a fraction of a second
main :: IO()
main = do 
    crabs <- readInput
    let median = computeMedian crabs
    let mini = foldl min 1000000 crabs :: Int
    let maxi = foldl max 0 crabs :: Int
    let costs = map (sum . computeOverallCost crabs) [mini..maxi] 
    print (minimum costs)

readInput :: IO(Crabs)
readInput = do 
    input <- Data.Text.IO.readFile "input.txt"
    let split = splitOn (pack ",") input
    let vals = map (read . unpack) split
    return vals

computeMedian :: Crabs -> Int
computeMedian crabs = do 
    let sorted = sort crabs 
    let median = sorted !! (length crabs `div` 2)
    median

computeCost :: Int -> Int 
computeCost dist = (dist * (dist + 1)) `div` 2

computeOverallCost :: Crabs -> Int -> [Int]
computeOverallCost crabs point = map (\x -> computeCost (abs (x - point))) crabs