module ChronalCalibration (
    chronalCalibration
) where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

inputToList :: String -> [Int]
inputToList s = map read $ words (filter (not . flip elem ",'+") s) -- This just parses my input into a list

part1_solve :: String -> Int
part1_solve = sum . inputToList
    
part2_solve :: String -> Int
part2_solve = get_freq 0 (IntSet.fromList []) . (cycle . inputToList) -- Makes the list into a cycle so we can continue to recurse through it until the frequency is found
    where 
        get_freq :: Int -> IntSet -> [Int] -> Int
        get_freq curr_item items_seen (x:xs) 
            | curr_item `IntSet.member` items_seen = curr_item
            | otherwise = get_freq (curr_item + x) (curr_item `IntSet.insert` items_seen) xs -- If this is not the answer then we continue to recurse and add the item to the log
        get_freq _ _ [] = error "Urm how?!?!?!" -- Should never reach since the list is infinate

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

