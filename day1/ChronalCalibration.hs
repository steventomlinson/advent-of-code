module ChronalCalibration where

import Data.Set (Set)
import qualified Data.Set as Set

inputToList :: String -> [Integer]
inputToList s = map read $ words (filter (not . flip elem ",'+") s)

part1_solve :: String -> Integer
part1_solve = sum . inputToList

part1_solveFromFile :: String -> IO Integer
part1_solveFromFile s = do 
    input_data <- readFile s
    return (part1_solve input_data)

part2_solve :: String -> Integer
part2_solve = inner_solve 0 (Set.fromList []) . (cycle . inputToList)
    where 
        inner_solve :: Integer -> Set Integer -> [Integer] -> Integer
        inner_solve curr_item items_seen (x:xs) 
            | curr_item `Set.member` items_seen = curr_item
            | otherwise = inner_solve (curr_item + x) (curr_item `Set.insert` items_seen) xs

part2_solveFromFile :: String -> IO Integer
part2_solveFromFile s = do
    input_data <- readFile s
    return (part2_solve input_data)
