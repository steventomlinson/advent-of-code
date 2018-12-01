module ChronalCalibration where

inputToList :: String -> [Integer]
inputToList s = map read $ words (filter (not . flip elem ",'+") s)

part1_solve :: String -> Integer
part1_solve s = sum (inputToList s)

part1_solveFromFile :: String -> IO Integer
part1_solveFromFile s = do 
    input_data <- readFile s
    return (part1_solve input_data)

part2_solve :: String -> Integer
part2_solve s = let init_xs = inputToList s in 
    let inner_solve trace xs = case xs of 
                                [] -> inner_solve trace init_xs
                                (x:xs') 
                                    | elem (x + head trace) trace -> x + head trace
                                    | otherwise-> inner_solve ([x + head trace] ++ trace) xs'
    in inner_solve [0] init_xs

part2_solveFromFile :: String -> IO Integer
part2_solveFromFile s = do
    input_data <- readFile s
    return (part2_solve input_data)
