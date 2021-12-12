import Data.List (transpose)
import Data.List.Split (chunksOf)
import System.IO ()
import Debug.Trace ()


split :: Char -> String -> [String]
split _ "" = []
split delimiter str = 
    let (start, rest) = break (== delimiter) str
        (_, remain) = span (== delimiter) rest
     in start : split delimiter remain

data Board = Board [[(Int, Bool)]]

-- we'll define the output as [[numbers], [boards]]
parseInput :: String -> ([Int], [[[(Int, Bool)]]])
parseInput input = (numbers, boards)
    where
        numbers = map read $ split ',' $ head (lines input) :: [Int]
        boards = chunksOf 5 $ map (map (\x -> (x, False)) . (map read :: [String] -> [Int]) . words) $ filter (/="") $ tail $ lines input 

-- update a board's flags given a new number and return that board
updateBoard :: Int -> Board -> Board 
updateBoard num board = map (map (\(val, flag) -> (val,val==num))) board

-- returns true if a given board has an entire row or column of seen values
-- i.e. if all the flags for a given row or column are True 
hasBingo :: Board -> Bool
hasBingo board = rowCheck || columnCheck
    where
        rowCheck = 5 `elem` (map (\row -> foldl (\acc (_, flag) -> if flag then acc + 1 else acc) 0 row) board)
        columnCheck = 5 `elem` (map (\row -> foldl (\acc (_, flag) -> if flag then acc + 1 else acc) 0 row) $ transpose board)



doProblem :: ([Int], [[[(Int, Bool)]]]) -> Int
doProblem input = 5

-- main :: IO ()
-- main = do
--         contents <- readFile "input.txt"
--         print (doProblem (parseInput contents))