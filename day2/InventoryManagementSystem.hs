module InventoryManagementSystem (
    inventoryManagementSystem
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Debug.Trace

part1_solve :: String -> Int
part1_solve = inner_solve 0 0 . lines
    where
        inner_solve :: Int -> Int -> [String] -> Int
        inner_solve n2 n3 [] = n2 * n3
        inner_solve n2 n3 (l:ls) = let freq_list = get_freq l (Map.fromSet (const 0) (Set.fromList ['a' .. 'z'])) in
            inner_solve (n2 + hasx 2 freq_list) (n3 + hasx 3 freq_list) ls
            where
                hasx :: Eq a => a -> [a] -> Int
                hasx x xs   | x `elem` xs = 1
                            | otherwise = 0
                get_freq :: String -> Map.Map Char Int -> [Int]
                get_freq [] m = Map.elems m
                get_freq (c:cs) m = get_freq cs (Map.adjust (+1) c m)

part2_solve :: String -> String
part2_solve = inner_solve . List.sort . lines
    where
        off_by_one :: String -> String -> Maybe String
        off_by_one a b = let s = filter (uncurry (==)) (zip a b) in 
            if length s == length a - 1 then (Just . map fst) s else Nothing
        inner_solve :: [String] -> String
        inner_solve [] = error "No input"
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

