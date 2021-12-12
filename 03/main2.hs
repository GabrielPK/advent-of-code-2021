import Data.List
import System.IO ()
import Debug.Trace

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
flip' _ = error "not binary!"

average :: (Fractional a1, Real a2) => [a2] -> a1
average xs = realToFrac (sum xs) / genericLength xs

bin2dec :: [Char] -> Int
bin2dec = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
    where c2i c = if c == '0' then 0 else 1

findMode :: String -> Char
findMode col = if average (map (read . (:"")) col :: [Int]) >= 0.5 then '1' else '0'

findO2 :: [String] -> Int -> String
findO2 [str] _ = str
findO2 xs pos =
    findO2 (filter (\x -> curMode == x!!pos) xs) (pos + 1)
    where
        txs = transposeString xs
        curMode = findMode $ txs!!pos

findCO2 :: [String] -> Int -> String
findCO2 [str] _ = str
findCO2 xs pos =
    findCO2 (filter (\x -> curMode == x!!pos) xs) (pos + 1)
    where
        txs = transposeString xs
        curMode = flip' (findMode (txs!!pos))

doProblem :: [String] -> Int
doProblem xs =
    o2 * cO2
    where o2 = bin2dec (findO2 xs 0)
          cO2 = bin2dec (findCO2 xs 0)

main :: IO ()
main = do
        contents <- readFile "input.txt"
        print (doProblem (parseInput contents))