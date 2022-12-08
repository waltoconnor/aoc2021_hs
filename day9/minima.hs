import Data.Map (Map)
import qualified Data.Map as Map
import System.IO (openFile, IOMode (ReadMode), hGetContents)
import Data.Char(digitToInt)
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (Set, insert, empty, size, member)
import Data.List (sort)
import Debug.Trace (traceShow)

type HeightMap = [[Int]]
type HeightMapMap = Map (Int, Int) Int -- grid operations in haskell suck, so i'm doing this with a hashmap

main :: IO()
main = do 
    input <- readInput 
    let m = inputToMap input
    print (computePart1Score m)
    print (computePart2Score m)
    print "hi"

readInput :: IO(HeightMap)
readInput = do
    handle <- openFile "input.txt" ReadMode
    input <- hGetContents handle
    let out = map (map digitToInt) (lines input)
    return out

tagRow :: Int -> [Int] -> [((Int, Int), Int)]
tagRow rowId row = map (\(colIdx, val) -> ((colIdx, rowId), val)) (zip [0..] row)

inputToMap :: HeightMap -> HeightMapMap
inputToMap hm = Map.fromList (concat (map (\(rowId, row) -> tagRow rowId row) (zip [0..] hm)))

computePart1Score :: HeightMapMap -> Int 
computePart1Score hmm = sum (map (+ 1) (findMinima hmm))

isLocalMinima :: HeightMapMap -> (Int, Int) -> Bool 
isLocalMinima hmm (x, y) = do 
    let top = fromMaybe 10 (Map.lookup (x, (y - 1)) hmm) -- edges are considered to be high, and our input range is 0 to 9
    let left = fromMaybe 10 (Map.lookup ((x - 1), y) hmm)
    let right = fromMaybe 10 (Map.lookup ((x + 1), y) hmm) 
    let bottom = fromMaybe 10 (Map.lookup (x, (y + 1)) hmm)
    let val = fromMaybe 10 (Map.lookup (x, y) hmm) -- actual value guarenteed to exist
    let search = [top, left, right, bottom]
    length (filter (\neighbor -> neighbor <= val) search) == 0


findMinima :: HeightMapMap -> [Int]
findMinima hmm = do 
    let isMinima =  map (\((x, y), val) -> (isLocalMinima hmm (x, y), val)) (Map.toList hmm)
    map snd (filter fst isMinima)

checkAdj :: HeightMapMap -> Set (Int, Int) -> (Int, Int) -> Set(Int, Int)
checkAdj hmm curBasin (x, y) = do 
    let above = (x, y - 1)
    let left = (x - 1, y)
    let right = (x + 1, y)
    let below = (x, y + 1)
    let sa = findBasin hmm curBasin above
    let sl = findBasin hmm sa left 
    let sr = findBasin hmm sl right 
    let sb = findBasin hmm sr below 
    sb 

findBasin :: HeightMapMap -> Set (Int, Int) -> (Int, Int) -> Set(Int, Int)
findBasin hmm curBasin (x, y) = do 
    if fromMaybe 10 (Map.lookup (x,y) hmm) >= 9 || (member (x, y) curBasin)
        then curBasin 
        else  checkAdj hmm (insert (x, y) curBasin ) (x, y)

findMinimaLoc :: HeightMapMap -> [(Int, Int)]
findMinimaLoc hmm = do 
    let isMinima =  map (\((x, y), val) -> (isLocalMinima hmm (x, y), (x,y))) (Map.toList hmm)
    map snd (filter fst isMinima)

computePart2Score :: HeightMapMap -> Int 
computePart2Score hmm = do 
    let minima = findMinimaLoc hmm
    let basins = map (\(x,y) -> findBasin hmm empty (x,y)) minima
    let a:b:c:_ = traceShow basins reverse (sort (map size basins))
    traceShow (a, b, c) a * b * c

 