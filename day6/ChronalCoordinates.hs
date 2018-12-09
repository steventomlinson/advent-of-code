module ChronalCoordinates (
    chronalCoordinates
) where

import qualified Data.Map as M
import Data.List (minimumBy, sortBy)
import Data.List.Split
import Debug.Trace

type Coord = (Int, Int)

parseCoords :: String -> [Coord]
parseCoords = map (toTuple . splitOn ", ") . lines
    where
        toTuple :: [String] -> Coord
        toTuple [a, b] = (read a, read b)

mDist :: Coord -> Coord -> Int
mDist c1 c2 = abs (fst c1 - fst c2) + abs (snd c1 - snd c2)

fillMap :: [Coord] -> M.Map Coord [Coord]
fillMap xs = fillMapInternal [(x, y) | x <- [0..(maximum . map fst) xs], y <- [0..(maximum . map snd) xs]] M.empty
    where
        fillMapInternal :: [Coord] -> M.Map Coord [Coord] -> M.Map Coord [Coord]
        fillMapInternal [] m = m
        fillMapInternal (c:cs) m = let sortedDists = sortBy (\a b -> compare (snd a) (snd b)) [(p, dist) | p <- xs, let dist = mDist c p] in
            case sortedDists of
                (d1:d2:ds)  | snd d1 == snd d2 -> fillMapInternal cs m
                            | otherwise -> fillMapInternal cs (M.insertWith (++) (fst d1) [c] m)

part1_solve :: String -> Int
part1_solve s = (maximum . map length . filter (all checkBound) . M.elems . fillMap) coords
    where
        coords = parseCoords s
        maxX = (maximum . map fst) coords :: Int
        maxY = (maximum . map snd) coords :: Int
        checkBound :: Coord -> Bool
        checkBound (x, y) = x > 0 && x < maxX && y > 0 && y < maxY
        getMax m = 1

getSize :: [Coord] -> Int
getSize xs = countValid [(x, y) | x <- [0..(maximum . map fst) xs], y <- [0..(maximum . map snd) xs]] 0
    where
        countValid :: [Coord] -> Int -> Int
        countValid [] n = n
        countValid (c:cs) n = let dists = [dist | p <- xs, let dist = mDist c p] in
            if sum dists < 10000 then countValid cs (n + 1) else countValid cs n


part2_solve :: String -> Int
part2_solve = getSize . parseCoords

solveFromFile :: String -> (String -> a) -> IO a
solveFromFile s f = do
    input_data <- readFile s
    return (f input_data)

chronalCoordinates :: IO ()
chronalCoordinates = do
    let solve = solveFromFile "day6/input.txt"
    part1_result <- solve part1_solve
    print part1_result
    part2_result <- solve part2_solve
    print part2_result