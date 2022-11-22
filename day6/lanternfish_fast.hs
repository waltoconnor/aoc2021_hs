import System.IO (openFile, IOMode (ReadMode), hGetContents)
import Data.Text (splitOn, pack, unpack, Text)
import Data.Text.IO

type LanternFish = [Int] -- this time this corresponds to 9 buckets each holding a count of fish

main :: IO()
main = do 
    input <- readInput 
    print input
    let final_fish = runSim input 256
    print (sum final_fish)

getCount :: LanternFish -> Int -> Int
getCount fish age = length (filter (== age) fish)

readInput :: IO(LanternFish)
readInput = do 
    input <- Data.Text.IO.readFile "input.txt"
    let split = splitOn (pack ",") input
    let vals = map (read . unpack) split
    
    let counts = map (getCount vals) [0..8]
    return counts

simulateDay :: LanternFish -> LanternFish
simulateDay [d0, d1, d2, d3, d4, d5, d6, d7, d8] =
    -- shift the next six slots down one
    -- then cycle the entries in slot 6 and add the fish that just gave birth to them
    -- then slide the new fish from last cycle in, followed by the newly born fish 
    [d1, d2, d3, d4, d5, d6, d7 + d0, d8, d0]

simulateDay _ = []

runSim :: LanternFish -> Int -> LanternFish
runSim state 0 = state
runSim state days = runSim (simulateDay state) (days - 1)