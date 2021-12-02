{-# LANGUAGE ViewPatterns #-}
import qualified Data.Map as Map
import qualified Data.List.Split as Split 
import Data.List
import System.IO ()

-- represent directions as vectors of movement like (horizontal, depth, aim)
parseLine :: String -> (Int, Int, Int)
parseLine (stripPrefix "forward " -> Just restOfString) = (read restOfString :: Int, 0, 0)
parseLine (stripPrefix "down " -> Just restOfString) = (0, 0, read restOfString :: Int)
parseLine (stripPrefix "up " -> Just restOfString) = (0, 0, -1 * read restOfString :: Int)

parseInput :: String -> [(Int, Int, Int)]
parseInput input = map parseLine $ lines input

lam :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
lam (x1, y1, z1) (0, _, z2) =  (x1, y1, z1 + z2) -- up or down
lam (x1, y1, z1) (x2, _, 0) =  (x1 + x2, y1 + (x2*z1), z1) -- forward

compute xs = foldl (lam) (0,0,0) xs

doProblem :: [(Int, Int, Int)] -> Int
doProblem xs = horizontal * depth 
    where
        (horizontal, depth, aim) = compute xs

main :: IO ()
main = do
        contents <- readFile "input2.txt" 
        putStr (show (doProblem (parseInput contents)))