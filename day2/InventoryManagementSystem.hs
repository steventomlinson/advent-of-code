module InventoryManagementSystem (
    inventoryManagementSystem
) where

import qualified Data.List as List
import qualified Data.Maybe as Maybe

part1_solve :: String -> Int
part1_solve = inner_solve 0 0 . lines
    where
        hasx :: String -> Int -> Int
        hasx s i    | i `elem` map length ((List.group . List.sort) s) = 1
                    | otherwise = 0
        inner_solve :: Int -> Int -> [String] -> Int
        inner_solve n2 n3 [] = n2 * n3
        inner_solve n2 n3 (l:ls) = inner_solve (n2 + hasx l 2) (n3 + hasx l 3) ls

part2_solve :: String -> String
part2_solve = inner_solve . List.sort . lines
    where
        off_by_one :: String -> String -> Maybe String
        off_by_one a b = let s = filter (uncurry (==)) (zip a b) in 
            if length s == length a - 1 then (Just . map fst) s else Nothing
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

