import System.IO (openFile, IOMode (ReadMode), hGetContents)
import Data.Text (splitOn, pack, unpack, Text)
import Data.Text.IO
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (elemIndex)
import Data.Maybe (fromJust, fromMaybe)
import Debug.Trace (trace, traceShow)

type Signal = Char
type OutputSignal = [Signal]
type InputSignal = [Signal]

processInputLine :: String -> ([InputSignal], [OutputSignal])
processInputLine line = do 
    let [input_s, output_s] = splitOn (pack "|") (pack line)
    let input_signals = map unpack (splitOn (pack " ") input_s)
    let output_signals = map unpack (splitOn (pack " ") output_s)
    (input_signals, output_signals)

readInput :: IO([([InputSignal], [OutputSignal])])
readInput = do 
    input <- Data.Text.IO.readFile "input.txt"
    return (map processInputLine (lines (unpack input)))

toEasyDigit :: OutputSignal -> Int 
toEasyDigit output_signal = do 
    let len = length output_signal 
    case len of 
        2 -> 1
        3 -> 7
        4 -> 4
        7 -> 8
        _ -> -1

numTimesFound :: Ord a => a -> [a] -> Int
numTimesFound x xs = (length . filter (== x)) xs

part1 :: [([InputSignal], [OutputSignal])] -> Int 
part1 signals = do 
    let outputs = concat (map snd signals)
    let list = map toEasyDigit outputs
    let ones = numTimesFound 1 list
    let sevens = numTimesFound 7 list 
    let fours = numTimesFound 4 list 
    let eights = numTimesFound 8 list 
    ones + sevens + fours + eights

(\\) :: Ord a => Set a -> Set a -> Set a
xs \\ ys = xs Set.\\ ys

setsEqual :: Ord a => Set a -> Set a -> Bool
setsEqual a b = Set.isSubsetOf a b && Set.isSubsetOf b a

checkNine :: Set Signal -> Set Signal -> Set Signal -> Bool 
checkNine unknown fourSet sevenSet = (Set.size unknown == 6) && Set.isSubsetOf fourSet unknown
    -- let nine_segments = Set.union sevenSet fourSet
    -- traceShow "nine" traceShow sevenSet traceShow fourSet traceShow unknown traceShow nine_segments (setsEqual unknown nine_segments)

checkSix :: Set Signal -> Set Signal -> Bool 
checkSix unknown oneSet = traceShow "six" (Set.size unknown == 6) && Set.size (Set.intersection oneSet unknown) == 1

checkZero :: Set Signal -> Set Signal -> Set Signal -> Bool 
checkZero unknown sixSet nineSet = traceShow "zero" (Set.size unknown == 6) && (unknown /= sixSet) && (unknown /= nineSet)

checkTwo :: Set Signal -> Set Signal -> Set Signal -> Bool 
checkTwo unknown eightSet nineSet = traceShow "two" (Set.size unknown == 5) && Set.isSubsetOf (Set.difference eightSet nineSet) unknown

checkFive :: Set Signal -> Set Signal -> Set Signal -> Bool 
checkFive unknown eightSet twoSet = traceShow "five" (Set.size unknown == 5) && Set.isSubsetOf (Set.difference eightSet twoSet) unknown

checkThree :: Set Signal -> Set Signal -> Bool 
checkThree unknown sevenSet = traceShow "three" (Set.size unknown == 5) && Set.isSubsetOf (sevenSet) unknown

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x:xs) = Just x 

unwrap :: Maybe(Set a) -> Set a
unwrap Nothing = Set.empty 
unwrap x = fromJust x

handleLine :: ([InputSignal], [OutputSignal]) -> Int
handleLine (inputs, outputs) = do 
    let sets = traceShow inputs map Set.fromList inputs
    let one = head' (filter (\x -> 2 == Set.size x) sets)
    let seven = head' (filter (\x -> 3 == Set.size x) sets)
    let four = head' (filter (\x -> 4 == Set.size x) sets)
    let eight = head' (filter (\x -> 7 == Set.size x) sets)
    let nine = head' (filter (\x -> checkNine x (unwrap four) (unwrap seven)) sets)
    let six = head' (filter (\x -> checkSix x (unwrap one)) sets)
    let zero = head' (filter (\x -> checkZero x (unwrap six) (unwrap nine)) sets)
    let two = head' (filter (\x -> checkTwo x (unwrap eight) (unwrap nine)) sets)
    let five = head' (filter (\x -> checkFive x (unwrap eight) (unwrap two)) sets)
    let three = head' (filter (\x -> checkThree x (unwrap seven)) sets)

    let digits = [zero, one, two, three, four, five, six, seven, eight, nine]
    let digits_unwrapped = map unwrap digits 

    let output_sets = map Set.fromList (filter (/= "") outputs)
    let x = map (\x -> fromJust (elemIndex x digits_unwrapped)) output_sets
    --let f:r = traceShow output_sets output_sets;
    --let x = traceShow f (elemIndex f (map unwrap digits))
    let [th, h, tn, o] = x
    (th * 1000) + (h * 100) + (tn * 10) + o
    --fromJust th


part2 :: [([InputSignal], [OutputSignal])] -> Int
part2 input = sum (map handleLine input)

main :: IO()
main = do 
    input <- readInput
    print (part1 input)
    print (part2 input)





    