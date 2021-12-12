import Data.List (transpose)
import Data.List.Split (chunksOf)
import System.IO ()
import Debug.Trace (trace)

split :: Char -> String -> [String]
split _ "" = []
split delimiter str =
    let (start, rest) = break (== delimiter) str
        (_, remain) = span (== delimiter) rest
     in start : split delimiter remain

-- we'll define the output as [[numbers], [boards]]
parseInput :: String -> ([Int], [[[(Int, Bool)]]])
parseInput input = (numbers, boards)
    where
        numbers = map read $ split ',' $ head (lines input) :: [Int]
        boards = chunksOf 5 $ map (map (\x -> (x, False)) . (map read :: [String] -> [Int]) . words) $ filter (/="") $ tail $ lines input

-- update a board's flags given a new number and return that board
updateBoard :: Int -> [[(Int, Bool)]] -> [[(Int, Bool)]]
updateBoard num board = map (map (\(val, flag) -> if not flag then (val,val==num) else (val, flag))) board

-- returns true if a given board has an entire row or column of seen values
-- i.e. if all the flags for a given row or column are True 
hasBingo :: [[(Int, Bool)]] -> Bool
hasBingo board = rowCheck || columnCheck
    where
        rowCheck = 5 `elem` map (foldl (\acc (_, flag) -> if flag then acc + 1 else acc) 0) board
        columnCheck = 5 `elem` map (foldl (\acc (_, flag) -> if flag then acc + 1 else acc) 0) (transpose board)

-- 
callNumbers :: [Int] -> [[[(Int, Bool)]]] -> (Int, [[(Int, Bool)]])
callNumbers [] _ = error "out of numbers"
callNumbers _ [] = error "no boards"
-- callNumbers (x:xs) [board] = (x, board)
callNumbers (x:xs) boards = 
    -- no board is a winner
    if null bingoBoard 
        then 
            -- trace("x: " ++ show x)
            -- trace("boards: " ++ show boards)
            callNumbers xs newBoards 
    else if length boards > 1 
        then callNumbers xs antiBingoBoard
    else 
        (x, bingoBoard!!0)
    where
        newBoards = map (updateBoard x) boards
        bingoBoard = filter (hasBingo) newBoards
        antiBingoBoard = filter (not . hasBingo) newBoards

doProblem :: ([Int], [[[(Int, Bool)]]]) -> Int
doProblem (numbers, boards) = lastNum * sumLeftover
    where
        --(numbers, boards) = parseInput input
        (lastNum, winner) = callNumbers numbers boards
        sumLeftover = foldl (\acc (val, flag) -> if not flag then acc + val else acc) 0 (concat winner)

main :: IO ()
main = do
        contents <- readFile "input.txt"
        print (doProblem (parseInput contents))