import System.IO (openFile, IOMode (ReadMode), hGetContents)
main :: IO()

main = do 
    input_commands <- readInput
    let deltas = map commandToDelta input_commands
    -- print deltas
    let final_position = foldr sumDelta (0, 0) deltas

    print (fst final_position * snd final_position)


readInput :: IO [(String, Int)]
readInput = do 
    handle <- openFile "input.txt" ReadMode
    input <- hGetContents handle
    let input_lines = lines input
    let v = map (\l -> let [cmd, num] = words l in (cmd, read num)) input_lines
    return v

commandToDelta :: (String, Int) -> (Int, Int)
commandToDelta ("forward", x) = (x, 0)
commandToDelta ("down", y) = (0, y)
commandToDelta ("up", y) = (0, -y)
commandToDelta(_, x) = (0, 0)


sumDelta :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumDelta (a,b) (x, y) = (a + x, b + y)
