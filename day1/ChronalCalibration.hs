module Main where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

inputToList :: String -> [Int]
inputToList s = map read $ words (filter (not . flip elem ",'+") s)

part1_solve :: String -> Int
part1_solve = sum . inputToList

part1_solveFromFile :: String -> IO Int
part1_solveFromFile s = do 
    input_data <- readFile s
    return (part1_solve input_data)

part2_solve :: String -> Int
part2_solve = inner_solve 0 (IntSet.fromList []) . (cycle . inputToList)
    where 
        inner_solve :: Int -> IntSet -> [Int] -> Int
        inner_solve curr_item items_seen (x:xs) 
            | curr_item `IntSet.member` items_seen = curr_item
            | otherwise = inner_solve (curr_item + x) (curr_item `IntSet.insert` items_seen) xs

part2_solveFromFile :: String -> IO Int
part2_solveFromFile s = do
    input_data <- readFile s
    return (part2_solve input_data)

main :: IO ()
main = do
    part1_result <- part1_solveFromFile "day1/input.txt"
    print part1_result
    part2_result <- part2_solveFromFile "day1/input.txt"
    print part2_result

