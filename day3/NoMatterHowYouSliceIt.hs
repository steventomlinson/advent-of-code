module NoMatterHowYouSliceIt (
    noMatterHowYouSliceIt
) where

import Data.Char
import qualified Data.Map as Map

convertInput :: [String ] -> [(Int, (Int, Int, Int, Int))]
convertInput [] = []
convertInput (x:xs) = (to_tuple . map (read :: String->Int) . words . map (\c -> if isNumber c then c else ' ')) x : convertInput xs -- We parse the input into tuples to do our calculations on
    where 
        to_tuple [a,b,c,d,e] = (a, (b, c , d -1, e -1)) -- Put the list into a tuple
        to_tuple _ = error "invalid input" -- If the wrong length tuple then error

getCoords :: (Int, (Int, Int, Int, Int)) -> [(Int, Int)]
getCoords (_,(x,y,w,h)) = [(a,b) | a <- [x..w+x], b <- [y..h+y]] -- Get all the coords from the point of the square

fillAllClaims :: Map.Map (Int, Int) Int -> [(Int, (Int, Int, Int, Int))] -> Map.Map (Int, Int) Int
fillAllClaims m [] = m
fillAllClaims m (c:cs) = fillAllClaims (fill_claim (getCoords c) m) cs 
    where
        fill_claim :: [(Int, Int)] -> Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int
        fill_claim [] m' = m'
        fill_claim (coor:coords) m' | Map.member coor m' = (fill_claim coords . Map.adjust (+1) coor) m' -- Add 1 to the current count of squares that have this point in their area
                                    | otherwise = (fill_claim coords .Map.insert coor 1) m' -- Initialise the coord


part1_solve :: String -> Int
part1_solve = Map.size . Map.filter (>1) . fillAllClaims Map.empty . convertInput . lines -- We find all the points where the count of squares are over 1 then we take the size of the corresponding map

part2_solve :: String -> Int
part2_solve s = findValid (Map.filter (==1) (fillAllClaims Map.empty cs)) cs -- filter out all the items over the count of 1 to reduce search space
    where
        cs = (convertInput . lines) s
        findValid :: Map.Map (Int, Int) Int -> [(Int, (Int, Int, Int, Int))] -> Int
        findValid _ [] = error "No valid Id"
        findValid m (c:cs') | (all (`Map.member` m) . getCoords) c = fst c -- If all the members are in the map then there is no overlap
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
