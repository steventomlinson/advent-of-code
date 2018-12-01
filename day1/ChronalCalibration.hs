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
part2_solve s = let inner_solve items_seen (x:xs)   |  (x + head items_seen) `elem` items_seen = x + head items_seen
                                                    | otherwise = inner_solve ((x + head items_seen) : items_seen) xs
    in inner_solve [0] (cycle (inputToList s))

part2_solveFromFile :: String -> IO Integer
part2_solveFromFile s = do
    input_data <- readFile s
    return (part2_solve input_data)
