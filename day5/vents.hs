import System.IO (openFile, IOMode (ReadMode), hGetContents)
import Data.Text.IO (readFile) 
import Data.Text (pack, unpack, splitOn, Text)
import Debug.Trace

type Coord = (Int, Int)
type Vent = (Coord, Coord)
type VentLine = [Coord]
type SeaFloor = [[Int]]

-- This is an extremely slow approach that involves cloning a 1 million cell grid thousands of times
-- there are certainly more efficient ways of doing this but I don't know haskell well enough to figure it out yet
main :: IO()
main = do 
    vents <- readInput
    let max_coords = getMaxCoords vents
    let seafloor = generateSeafloor max_coords
    let final_seafloor = putVentsOnSeaFloor seafloor vents
    --mapM_ (\row -> print row) final_seafloor
    print (countIntersections final_seafloor)
    

textToCoord :: Text -> Coord
textToCoord text = do
    let numbers = splitOn (pack ",") text
    let [a, b] = map (read . unpack) numbers
    (a,b)

lineToVent :: Text -> Vent 
lineToVent text = do 
    let [c1, _, c2] = splitOn (pack " ") text
    (textToCoord c1, textToCoord c2)
    
readInput :: IO [Vent]
readInput = do 
    input <- Data.Text.IO.readFile "input.txt"
    let lines = splitOn (pack "\n") input
    let vents = map lineToVent lines
    return vents

genRange :: Int -> Int -> [Int]
genRange a1 a2 
    | a1 <= a2 = [a1..a2]
    | a1 > a2 = [a1,(pred a1)..a2]
    | otherwise = []


ventToCoords :: Vent -> VentLine
ventToCoords ((x1, y1), (x2, y2)) 
    | x1 /= x2 && y1 /= y2 = zip (genRange x1 x2) (genRange y1 y2) -- uncomment for part2
    -- | x1 /= x2 && y1 /= y2 = [] -- comment for part2
    | x1 == x2 && y1 /= y2 = zip (replicate (1 + abs (y2 - y1)) x1) (genRange y1 y2)
    | x1 /= x2 && y1 == y2 = zip (genRange x1 x2) (replicate (1 + abs (x2 - x1)) y1)
    | otherwise = []

getMaxCoords :: [Vent] -> Coord
getMaxCoords vents = do 
    let max_x1 = maximum (map (\((x1, y1), (x2, y2)) -> x1) vents)
    let max_x2 = maximum (map (\((x1, y1), (x2, y2)) -> x2) vents)
    let max_y1 = maximum (map (\((x1, y1), (x2, y2)) -> y1) vents)
    let max_y2 = maximum (map (\((x1, y1), (x2, y2)) -> y2) vents)
    (1 + max max_x1 max_x2, 1 + max max_y1 max_y2)

generateSeafloor :: Coord -> SeaFloor
generateSeafloor (x_max, y_max) = do map (\_ -> replicate x_max 0) [0..y_max]

insertIntoSplit :: [Int] -> [Int] -> Int -> [Int]
insertIntoSplit head [] incr = head -- SHOULD NOT HAPPEN
insertIntoSplit head [val] incr = head ++ [val + incr]
insertIntoSplit head (old:tail) incr = head ++ (old + incr) : tail

addToList :: [Int] -> Int -> Int -> [Int]
addToList list idx val = do 
    let (head, tail) = splitAt idx list
    insertIntoSplit head tail val

putCoordOnSeaFloor :: SeaFloor -> Coord -> Int -> SeaFloor
putCoordOnSeaFloor mat (x, y) val = do 
    let (hd,row:tl) = splitAt y mat
    let new_row = addToList row x val 
    hd ++ new_row:tl

putCoordsOnSeaFloor :: SeaFloor -> VentLine -> SeaFloor
putCoordsOnSeaFloor = foldl (\ seafloor x -> putCoordOnSeaFloor seafloor x 1)

putVentOnSeaFloor :: SeaFloor -> Vent -> SeaFloor
putVentOnSeaFloor seafloor vent = do 
    -- let coords = traceShow vent ventToCoords vent
    -- traceShow coords putCoordsOnSeaFloor seafloor coords
    let coords = ventToCoords vent
    putCoordsOnSeaFloor seafloor coords

putVentsOnSeaFloor :: SeaFloor -> [Vent] -> SeaFloor
putVentsOnSeaFloor = foldl putVentOnSeaFloor


countIntersections :: SeaFloor -> Int 
countIntersections seafloor = sum (map (\row -> sum (map (\c -> if c > 1 then 1 else 0) row)) seafloor)
