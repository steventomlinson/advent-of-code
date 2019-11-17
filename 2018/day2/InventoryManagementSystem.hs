module InventoryManagementSystem (
    inventoryManagementSystem
) where

import qualified Data.List as List
import qualified Data.Maybe as Maybe

part1_solve :: String -> Int
part1_solve = inner_solve 0 0 . lines
    where
        hasx :: String -> Int -> Int
        hasx s i    | (any (\x -> length x == i) . List.group . List.sort) s = 1 -- If there are any items with the frequency then return 1
                    | otherwise = 0
        inner_solve :: Int -> Int -> [String] -> Int
        inner_solve n2 n3 [] = n2 * n3 -- No more items to calculate then calculate the answer
        inner_solve n2 n3 (l:ls) = inner_solve (n2 + hasx l 2) (n3 + hasx l 3) ls -- Continue to calculate the frequencies adding them to the counts

part2_solve :: String -> String
part2_solve = inner_solve . List.sort . lines -- If we sort then we can do it in linear time
    where
        off_by_one :: String -> String -> Maybe String
        off_by_one a b = let s = filter (uncurry (==)) (zip a b) in  -- s is the string of all the common elements
            if length s == length a - 1 then (Just . map fst) s else Nothing -- Therefore if s is 1 less then we have a valid Id
        inner_solve :: [String] -> String
        inner_solve [] = error "No valid ids"
        inner_solve [_] = error "No valid ids"
        inner_solve (x1:x2:xs)  = Maybe.fromMaybe (inner_solve (x2:xs)) (off_by_one x1 x2)

solveFromFile :: String -> (String -> a) -> IO a
solveFromFile s f = do
    input_data <- readFile s
    return (f input_data)

inventoryManagementSystem :: IO ()
inventoryManagementSystem = do
    let solve = solveFromFile "day2/input.txt"
    part1_result <- solve part1_solve
    print part1_result
    part2_result <- solve part2_solve
    print part2_result

