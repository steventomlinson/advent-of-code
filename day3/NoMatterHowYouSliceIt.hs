module NoMatterHowYouSliceIt where

import Data.Char
import qualified Data.Map as Map
import Debug.Trace

convert_input :: [String ] -> [(Int, (Int, Int, Int, Int))]
convert_input [] = []
convert_input (x:xs) = (to_tuple . map (read :: String->Int) . words . map (\c -> if isNumber c then c else ' ')) x : convert_input xs
    where 
        to_tuple [a,b,c,d,e] = (a, (b, c , d -1, e -1))

fill_in_data :: [[Int]] -> [(Int, (Int, Int, Int, Int))] -> [[Int]]
fill_in_data xss [] = xss
fill_in_data xss (t:ts') = fill_in_data (fill_in_row t xss) ts'
    where
        fill_in_row :: (Int, (Int, Int, Int, Int)) -> [[Int]] -> [[Int]]
        fill_in_row (id,(x,y,width,height)) xss' = let updated_list = take (y + height) xss' ++ [fill_in_element (xss' !! (y + height)) (width)] ++ drop ((y + height) + 1) xss' in
            if height == 0 then updated_list else fill_in_row (id,(x,y,width,height-1)) updated_list
            where
                fill_in_element ::[Int] -> Int -> [Int]
                fill_in_element xs curr_pos = let updated_row = take (x + curr_pos) xs ++ [(xs !! (x + curr_pos)) + 1] ++ drop (x + 1 + curr_pos) xs in
                    if curr_pos == 0 then updated_row else fill_in_element updated_row (curr_pos-1)

part1_solve_other :: String -> Int
part1_solve_other = (inner_solve Map.empty) . convert_input . lines
    where
        inner_solve :: Map.Map (Int, Int) Int -> [(Int, (Int, Int, Int, Int))] -> Int
        inner_solve m [] = (Map.size . Map.filter (>1)) m
        inner_solve m (c:cs) = inner_solve (fill_claim (get_coords c) m) cs 
            where
                get_coords :: (Int, (Int, Int, Int, Int)) -> [(Int, Int)]
                get_coords (_,(x,y,w,h)) = [(a,b) | a <- [x..w+x], b <- [y..h+y]]
                fill_claim :: [(Int, Int)] -> Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int
                fill_claim [] m' = m'
                fill_claim (coor:coords) m' | Map.member coor m' = (fill_claim coords . Map.adjust (+1) coor) m'
                                            | otherwise = (fill_claim coords .Map.insert coor 1) m'

part2_solve :: String -> Int
part2_solve = inner_solve . {-List.sortBy (\(aI,(a1,a2,a3,a4)) (bI,(b1,b2,b3,b4)) -> compare a1 b1) .-} convert_input. lines
    where
        inner_solve :: [(Int, (Int, Int, Int, Int))] -> Int
        inner_solve cs = check_data (fill_in_data ((replicate 1000 . replicate 1000) 0) cs) cs
            where
                check_data :: [[Int]] -> [(Int, (Int, Int, Int, Int))] -> Int
                check_data _ [] = error "No Valid"
                check_data xss (c:cs')  | is_valid xss c = fst c
                                        | otherwise = check_data xss cs'
                    where 
                        is_valid :: [[Int]] -> (Int, (Int, Int, Int, Int)) -> Bool
                        is_valid xss' c | all (all (==1)) (get_square xss' c) = True
                                        | otherwise = False
                            where
                                get_square :: [[Int]] -> (Int, (Int, Int, Int, Int)) -> [[Int]]
                                get_square xss' (_,(x,y,width,height)) = (map (take (width + 1) . drop x) . take (height + 1) . drop y) xss'

solveFromFile :: String -> (String -> a) -> IO a
solveFromFile s f = do
    input_data <- readFile s
    return (f input_data)

noMatterHowYouSliceIt :: IO ()
noMatterHowYouSliceIt = do
    let solve = solveFromFile "day3/input.txt"
    part1_result <- solve part1_solve_other
    print part1_result
    --part2_result <- solve part2_solve
    --print part2_result
