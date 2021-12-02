import qualified Data.Map as Map
import qualified Data.List.Split as Split 
import qualified Data.List (tails)
import qualified Data.Fixed as Fixed 
import System.IO ()

parseInput :: String -> [Int]
parseInput input = map read $ lines input ::[Int]

rollingSumsEfficient :: Int -> [Int] -> [Int]
rollingSumsEfficient _ [] = error "Empty list!"
rollingSumsEfficient 0 _ = error "Window size 0!"
rollingSumsEfficient n xs = scanl (+) firstSum deltas 
    where
        firstSum = sum (take n xs)
        deltas   = zipWith (-) (drop n xs) xs -- front - back

calcNumIncreases :: [Int] -> Int -> Int
calcNumIncreases [] _ = error "Empty list!"
calcNumIncreases [x] acc = acc 
calcNumIncreases (x:xs) acc = if x < head xs
                                then calcNumIncreases xs (acc + 1)
                                else calcNumIncreases xs acc

doProblem :: [Int] -> Int
doProblem xs = calcNumIncreases (rollingSumsEfficient 3 xs) 0

main :: IO ()
main = do
        contents <- readFile "input.txt" 
        putStr (show (doProblem (parseInput contents)))
        -- putStr contents