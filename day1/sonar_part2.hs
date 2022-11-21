import System.IO (openFile, IOMode (ReadMode), hGetContents)

main :: IO()

main = do 
    depths <- readInput
    takes <- windowSum depths
    let windows = slidingWindow takes
    let num_increases = countIncreases windows
    print num_increases


readInput :: IO[Int]
readInput = do 
    handle <- openFile "input.txt" ReadMode
    input <- hGetContents handle
    let rows = lines input
    let numbers = map read rows
    return numbers

windowSum :: [Int] -> IO [Int]
windowSum numbers = do
    let windows = windowSumTake numbers
    let values = map sum windows
    print (values)
    return values
    

    
windowSumTake :: [Int] -> [[Int]]
windowSumTake [] = []
windowSumTake [_, _] = []
windowSumTake numbers = take 3 numbers : windowSumTake (tail numbers)


slidingWindow :: [Int] -> [(Int, Int)]
slidingWindow numbers = do 
    zip numbers (mconcat [tail numbers, [0]])

countIncreases :: [(Int, Int)] -> Int 
countIncreases windows = do 
    sum (map (\(a,b) -> if a >= b then 0 else 1) windows)