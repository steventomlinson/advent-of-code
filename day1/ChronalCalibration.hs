module ChronalCalibration (
    chronalCalibration
) where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

inputToList :: String -> [Int]
inputToList s = map read $ words (filter (not . flip elem ",'+") s)

part1_solve :: String -> Int
part1_solve = sum . inputToList
    
part2_solve :: String -> Int
part2_solve = inner_solve 0 (IntSet.fromList []) . (cycle . inputToList)
    where 
        inner_solve :: Int -> IntSet -> [Int] -> Int
        inner_solve curr_item items_seen (x:xs) 
            | curr_item `IntSet.member` items_seen = curr_item
            | otherwise = inner_solve (curr_item + x) (curr_item `IntSet.insert` items_seen) xs
        inner_solve _ _ [] = error "Urm how?!?!?!"

solveFromFile :: String -> (String -> Int) -> IO Int
solveFromFile s f = do
    input_data <- readFile s
    return (f input_data)

chronalCalibration :: IO ()
chronalCalibration = do
    let solve = solveFromFile "day1/input.txt"
    part1_result <- solve part1_solve 
    print part1_result
    part2_result <- solve part2_solve
    print part2_result

