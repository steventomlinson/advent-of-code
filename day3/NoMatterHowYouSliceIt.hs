module NoMatterHowYouSliceIt where

import Data.Char
import qualified Data.Map as Map

convert_input :: [String ] -> [(Int, (Int, Int, Int, Int))]
convert_input [] = []
convert_input (x:xs) = (to_tuple . map (read :: String->Int) . words . map (\c -> if isNumber c then c else ' ')) x : convert_input xs
    where 
        to_tuple [a,b,c,d,e] = (a, (b, c , d -1, e -1))

get_coords :: (Int, (Int, Int, Int, Int)) -> [(Int, Int)]
get_coords (_,(x,y,w,h)) = [(a,b) | a <- [x..w+x], b <- [y..h+y]]

fillAllClaims :: Map.Map (Int, Int) Int -> [(Int, (Int, Int, Int, Int))] -> Map.Map (Int, Int) Int
fillAllClaims m [] = m
fillAllClaims m (c:cs) = fillAllClaims (fill_claim (get_coords c) m) cs 
    where
        fill_claim :: [(Int, Int)] -> Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int
        fill_claim [] m' = m'
        fill_claim (coor:coords) m' | Map.member coor m' = (fill_claim coords . Map.adjust (+1) coor) m'
                                    | otherwise = (fill_claim coords .Map.insert coor 1) m'


part1_solve :: String -> Int
part1_solve = Map.size . Map.filter (>1) . fillAllClaims Map.empty . convert_input . lines

part2_solve :: String -> Int
part2_solve s = findValid (Map.filter (==1) (fillAllClaims Map.empty cs)) cs
    where
        cs = (convert_input . lines) s
        findValid :: Map.Map (Int, Int) Int -> [(Int, (Int, Int, Int, Int))] -> Int
        findValid _ [] = error "No valid Id"
        findValid m (c:cs') | (all (`Map.member` m) . get_coords) c = fst c
                            | otherwise = findValid m cs'

solveFromFile :: String -> (String -> a) -> IO a
solveFromFile s f = do
    input_data <- readFile s
    return (f input_data)

noMatterHowYouSliceIt :: IO ()
noMatterHowYouSliceIt = do
    let solve = solveFromFile "day3/input.txt"
    part1_result <- solve part1_solve
    print part1_result
    part2_result <- solve part2_solve
    print part2_result
