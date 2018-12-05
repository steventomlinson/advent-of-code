module AlchemicalReduction (
    alchemicalReduction
) where

import Data.Char
import qualified Data.Map as Map
import Data.List (minimum)

removeAllDuplicates :: String -> String
removeAllDuplicates = foldr removeDuplicate ""
    where
        removeDuplicate :: Char -> String -> String
        removeDuplicate c1 (c2:cs) | c1 /= c2 && toLower c1 == toLower c2 = cs
        removeDuplicate c cs = c:cs 

part1_solve :: String -> Int
part1_solve = length . removeAllDuplicates

part2_solve :: String -> Int
part2_solve s = minimum (foldr getLength [] ['a' .. 'z'])
    where
        getLength :: Char -> [Int] -> [Int]
        getLength c xs = part1_solve (filter ((/=c) . toLower) s) : xs

solveFromFile :: String -> (String -> a) -> IO a
solveFromFile s f = do
    input_data <- readFile s
    return (f input_data)

alchemicalReduction :: IO ()
alchemicalReduction = do
    let solve = solveFromFile "day5/input.txt"
    part1_result <- solve part1_solve
    print part1_result
    part2_result <- solve part2_solve
    print part2_result
                                    
