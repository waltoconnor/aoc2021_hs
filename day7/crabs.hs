import System.IO (openFile, IOMode (ReadMode), hGetContents)
import Data.Text (splitOn, pack, unpack, Text)
import Data.Text.IO
import Data.List (sort)

type Crabs = [Int]

main :: IO()
main = do 
    crabs <- readInput
    let median = computeMedian crabs
    let distances = computeDistances crabs median
    print (sum distances)

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

computeDistances :: Crabs -> Int -> [Int]
computeDistances crabs point = map (\x -> abs (x - point)) crabs