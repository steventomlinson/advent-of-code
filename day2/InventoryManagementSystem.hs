module InventoryManagementSystem (
    inventoryManagementSystem
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Maybe as Maybe

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
part2_solve = inner_solve . lines
    where
        is_id :: String -> String -> Bool
        is_id s1 s2 = off_by_one s1 s2 False
            where
                off_by_one :: String -> String -> Bool -> Bool
                off_by_one [] [] True = True
                off_by_one (c1:cs1) (c2:cs2) False  | c1 /= c2 = off_by_one cs1 cs2 True
                                                    | otherwise = off_by_one cs1 cs2 False
                off_by_one (c1:cs1) (c2:cs2) True   | c1 /= c2 = False
                                                    | otherwise = off_by_one cs1 cs2 True
        get_id :: String -> [String] -> Maybe String
        get_id _ [] = Nothing
        get_id l (x:xs) = if is_id l x then
                Just (l `List.intersect` x)
            else
                get_id l xs
        inner_solve :: [String] -> String
        inner_solve [] = error "Not valid input"
        inner_solve (l:ls) = Maybe.fromMaybe (inner_solve ls) (get_id l ls)

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

