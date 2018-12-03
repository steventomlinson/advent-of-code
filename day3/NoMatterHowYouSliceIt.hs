module NoMatterHowYouSliceIt where

import Data.Char

part1_solve :: String -> Int
part1_solve = inner_solve . convert_input . lines
    where
        convert_input :: [String] -> [(Int, Int, Int, Int)]
        convert_input [] = []
        convert_input (x:xs) = convert_to_dimensions x : convert_input xs
            where
                convert_to_dimensions :: String -> (Int, Int, Int, Int)
                convert_to_dimensions = to_tuple . drop 1 . map (read :: String->Int) . words . map (\c -> if isNumber c then c else ' ')
                    where 
                        to_tuple :: [Int] -> (Int, Int, Int, Int)
                        to_tuple [a,b,c,d] = (a, b, c - 1, d - 1)
        inner_solve :: [(Int, Int, Int, Int)] -> Int
        inner_solve ts = (count_duplicate . fill_in_data ((replicate 1000 . replicate 1000) 0)) ts
                        where 
                            fill_in_data :: [[Int]] -> [(Int, Int, Int, Int)] -> [[Int]]
                            fill_in_data xss [] = xss
                            fill_in_data xss (t:ts') = fill_in_data (fill_in_row t xss) ts'
                                where
                                    fill_in_row :: (Int, Int, Int, Int) -> [[Int]] -> [[Int]]
                                    fill_in_row (x,y,width,height) xss' = let updated_list = take (y + height) xss' ++ [fill_in_element (xss' !! (y + height)) (width)] ++ drop ((y + height) + 1) xss' in
                                        if height == 0 then updated_list else fill_in_row (x,y,width,height-1) updated_list
                                        where
                                            fill_in_element ::[Int] -> Int -> [Int]
                                            fill_in_element xs curr_pos = let updated_row = take (x + curr_pos) xs ++ [(xs !! (x + curr_pos)) + 1] ++ drop (x + 1 + curr_pos) xs in
                                                if curr_pos == 0 then updated_row else fill_in_element updated_row (curr_pos-1)
                            count_duplicate :: [[Int]] -> Int
                            count_duplicate = length . filter (>1) . concat

solveFromFile :: String -> (String -> a) -> IO a
solveFromFile s f = do
    input_data <- readFile s
    return (f input_data)

noMatterHowYouSliceIt :: IO ()
noMatterHowYouSliceIt = do
    let solve = solveFromFile "day3/input.txt"
    part1_result <- solve part1_solve
    print part1_result
    --part2_result <- solve part2_solve
    --print part2_result
