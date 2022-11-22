import System.IO (openFile, IOMode (ReadMode), hGetContents)
import Data.Text (splitOn, pack, unpack, Text)
import Data.Text.IO

type CountDown = Int
type LanternFish = CountDown
type FishState = [LanternFish]

main :: IO()
main = do 
    input <- readInput 
    let final_fish = runSim input 80
    print (length final_fish)

readInput :: IO(FishState)
readInput = do 
    input <- Data.Text.IO.readFile "input.txt"
    let split = splitOn (pack ",") input
    let vals = map (read . unpack) split
    return vals

simulateDay :: FishState -> FishState 
simulateDay state = do 
    let num_new = sum (map (\x -> if x == 0 then 1 else 0) state)
    let decremented_list = map pred state
    let reset_list = map (\f -> if f == -1 then 6 else f) decremented_list
    reset_list ++ replicate num_new 8
    
runSim :: FishState -> Int -> FishState
runSim state 0 = state
runSim state days = runSim (simulateDay state) (days - 1)

    


