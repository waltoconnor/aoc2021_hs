import System.IO (openFile, IOMode (ReadMode), hGetContents)
import Data.List (transpose, sortBy)
import Data.Text (splitOn, pack, unpack, Text)
import Data.Text.IO
import Debug.Trace

type BingoNumber = Int 
type BingoHit = Bool
type BingoCell = (BingoNumber, BingoHit)
type CallIdx = Int
type Score = Int
type HasWon = (Bool, CallIdx, Score) 
type BingoBoardId = Int
type BingoBoard = ([[BingoCell]], HasWon, BingoBoardId)

tagCell :: BingoNumber -> BingoCell -> BingoCell 
tagCell number (board_number, hit) = if board_number == number then (board_number, True) else (board_number, hit)

tagRow :: BingoNumber -> [BingoCell] -> [BingoCell]
tagRow number row = do map (tagCell number) row

tagBoard :: BingoNumber -> BingoBoard -> BingoBoard
tagBoard number (board, win, id) = do (map (tagRow number) board, win, id)

-- top level call for updating boards
tagBoards :: BingoNumber -> [BingoBoard] -> [BingoBoard]
tagBoards number boards = do map (tagBoard number) boards 

sumEmpty :: [[BingoCell]] -> Score
sumEmpty board = do sum (map (\row -> sum (map (\(val, hit) -> if hit then 0 else val) row)) board)

computeScore :: Score -> [[BingoCell]] -> Score
computeScore called_number board = do called_number * (sumEmpty board)

checkRowWin :: [BingoCell] -> Bool 
checkRowWin row = do sum (map (\(_, c) -> if c then 1 else 0) row) == length row

checkBoardRows :: [[BingoCell]] -> Bool 
checkBoardRows board = do or (map checkRowWin board)

checkWon :: [[BingoCell]] -> Bool
checkWon board = do checkBoardRows board || checkBoardRows (transpose board)

updateWin :: BingoNumber -> CallIdx -> BingoBoard -> BingoBoard 
updateWin call_value call_idx (board, (hasWon, wonStep, score), id) = do 
    if hasWon then (board, (hasWon, wonStep, score), id)
    else if checkWon board then
        (board, (True, call_idx, computeScore call_value board), id)
    else (board, (False, 0, 0), id)

-- top level call for updating win state of boards
checkBoards :: BingoNumber -> CallIdx -> [BingoBoard] -> [BingoBoard]
checkBoards num idx boards = do map (updateWin num idx) boards

runGame :: [BingoNumber] -> CallIdx -> [BingoBoard] -> [BingoBoard]
runGame [] _ boards = do boards
runGame [num] n boards = do checkBoards num n (tagBoards num boards)
runGame (num:num_tail) n boards = do
    let new_boards = checkBoards num n (tagBoards num boards)
    runGame num_tail (n + 1) new_boards

genCell :: BingoNumber -> BingoCell
genCell num = (num, False)

readBoard :: (Int, Text) -> BingoBoard
readBoard (id, input) = do
    let board_rows_text = splitOn (Data.Text.pack "\n") input
    let board_rows_split_text = map (splitOn (Data.Text.pack " ")) board_rows_text
    
    -- let board_rows = map (map (genCell . read . unpack)) board_rows_split_text :: [[BingoCell]]
    let board_rows_unpacked = map (map unpack) board_rows_split_text
    let filtered_rows = map (filter (/= "")) board_rows_unpacked
    let board_rows_read = map (map read) filtered_rows :: [[Int]]
    let board_rows = map (map genCell) board_rows_read
    let board = (board_rows, (False, 0, 0), id) :: BingoBoard
    board

readInput :: IO([BingoNumber], [BingoBoard])
readInput = do 
    input <- Data.Text.IO.readFile "input.txt"
    let split = splitOn (Data.Text.pack "\n\n") input
    print (length split)
    let numbers:boards_raw = split
    let bingo_numbers = map read (map unpack (splitOn (Data.Text.pack ",") numbers))
    let boards = map readBoard (zip [0::Int ..] boards_raw)
    return (bingo_numbers, boards)

main :: IO()
main = do 
    (numbers, boards) <- readInput

    let final_boards = runGame numbers 0 boards
    let sorted = sortBy (\(_,(_, idx_a, _), _) (_,(_,idx_b,_),_) -> compare idx_a idx_b) final_boards
    let results_map = map (\(_,(_,iter,score),id) -> (id, score, iter)) (filter (\(_,(won, idx,_),_) -> won && (idx /= 0)) sorted)
    print "WINNING BOARDS:"
    print "(id, score, won on call)"
    mapM_ print results_map