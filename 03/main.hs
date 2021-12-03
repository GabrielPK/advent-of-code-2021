import Data.List
import System.IO ()

parseInput :: String -> [String]
parseInput input = lines input

transposeString :: [String] -> [String]
transposeString xs 
    | all null xs = []
    | otherwise = map safeHead xs : transpose (map safeTail xs)
    where
        safeHead x = if null x then ' ' else head x
        safeTail x = if null x then [] else tail x

flip' :: Char -> Char
flip' '1' = '0'
flip' '0' = '1'

average xs = realToFrac (sum xs) / genericLength xs

findGamma :: [String] -> String
findGamma xs = 
    [if average (map (read . (:"")) col :: [Int]) > 0.5 then '1' else '0'
    | col <- transposeString xs]

-- epsilon = !gamma

findEpsilon :: [String] -> String
findEpsilon xs = 
    [if average (map (read . (:"")) col :: [Int]) > 0.5 then '0' else '1'
    | col <- transposeString xs]

bin2dec = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
    where c2i c = if c == '0' then 0 else 1

doProblem :: [String] -> Int
doProblem xs = gamma * epsilon
    where gamma = bin2dec (findGamma xs)
          epsilon = bin2dec (findEpsilon xs)

main :: IO ()
main = do
        contents <- readFile "input.txt" 
        putStrLn (show (doProblem (parseInput contents)))