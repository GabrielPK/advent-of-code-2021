{-# LANGUAGE ViewPatterns #-}
import qualified Data.Map as Map
import qualified Data.List.Split as Split 
import Data.List
import System.IO ()

-- represent directions as vectors of movement like (horizontal, depth, aim)
parseLine :: String -> (Int, Int)
parseLine (stripPrefix "forward " -> Just restOfString) = (read restOfString :: Int, 0)
parseLine (stripPrefix "down " -> Just restOfString) = (0, read restOfString :: Int)
parseLine (stripPrefix "up " -> Just restOfString) = (0, -1 * read restOfString :: Int)

parseInput :: String -> [(Int, Int)]
parseInput input = map parseLine $ lines input

compute xs = foldl (\(accx, accy) (x, y) -> (accx + x, accy + y)) (0,0) xs

doProblem :: [(Int, Int)] -> Int
doProblem xs = horizontal * depth 
    where
        (horizontal, depth) = compute xs

main :: IO ()
main = do
        contents <- readFile "input.txt" 
        putStrLn (show (doProblem (parseInput contents)))