module ReposeRecord where

import qualified Data.List as List
import Data.List.Split
import qualified Data.Map as Map
import Data.Ord (comparing)
import Data.Monoid

data EventType = ShiftStart | WakeUp | FallAsleep deriving (Eq,Show)

data LogRecord = LogRecord {
    time :: (Int, Int, Int, Int, Int),
    guard :: Int,
    event :: EventType
} deriving (Show)

sortLogRecord :: LogRecord -> LogRecord -> Ordering
sortLogRecord l1 l2 = compare (time l1) (time l2)

toLogs :: String -> [LogRecord]
toLogs = getLogs . lines
    where
        getLog :: [String] -> LogRecord
        getLog (y:mo:d:h:mi:ss) = uncurry (LogRecord ((read y), (read mo), (read d), (read h), (read mi))) (getType ss)
        getType :: [String] -> (Int, EventType)
        getType (s:ss)  | s == "Guard" = ((read . drop 1 . head) ss, ShiftStart)
                        | s == "falls" = (0, FallAsleep)
                        | s == "wakes" = (0, WakeUp)
        getLogs :: [String] -> [LogRecord]
        getLogs [] = []
        getLogs (s:ss) = (getLog ((splitOneOf "- :]" (drop 1 s)) List.\\ [""]) : getLogs ss)

incTime :: (Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int)
incTime (y,mo,d,h,mi)   | mi == 59 = incHour (y,mo,d,h)
                        | otherwise = (y,mo,d,h,mi + 1)
    where
        incHour :: (Int, Int, Int, Int) -> (Int, Int, Int, Int, Int)
        incHour (y,mo,d,h)  | h == 23 = incDay (y, mo, d)
                            | otherwise = (y,mo,d,h + 1,0)
        incDay :: (Int, Int, Int) -> (Int, Int, Int, Int, Int)
        incDay (y,mo,d) | (elem mo [1,3,5,7,8,10,12] && d == 31) || (elem mo [4,6,9,10] && d == 30) || (mo == 2 && d == 28) = incMonth (y,mo)
                        | otherwise = (y,mo,d + 1, 0, 0)
        incMonth :: (Int, Int) -> (Int, Int, Int, Int, Int)
        incMonth (y,mo) | mo == 12 = (y+1,1,0,0,0)
                        | otherwise = (y,mo+1,0,0,0)

buildMinutesAsleep :: Map.Map Int (Map.Map Int Int) -> Int -> [LogRecord] ->  Map.Map Int (Map.Map Int Int)
buildMinutesAsleep m _ [] = m
buildMinutesAsleep m id (l:ls) 
    | event l == ShiftStart = buildMinutesAsleep m (guard l) ls
    | event l == FallAsleep = buildMinutesAsleep (addData m id l (head ls)) id (drop 1 ls) 
    | event l == WakeUp = error "Whats!!!"
    where
        updateMinutes :: Map.Map Int Int -> (Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int) -> Map.Map Int Int
        updateMinutes m (y1, mo1, d1, h1, mi1) (y2, mo2, d2, h2, mi2)
            | (y1, mo1, d1, h1, mi1) == (y2, mo2, d2, h2, mi2) = m
            | Map.member mi1 m = updateMinutes (Map.adjust (+1) mi1 m) (incTime (y1, mo1, d1, h1, mi1)) (y2, mo2, d2, h2, mi2)
            | otherwise = updateMinutes (Map.insert mi1 1 m) (incTime (y1, mo1, d1, h1, mi1)) (y2, mo2, d2, h2, mi2)
        addData :: Map.Map Int (Map.Map Int Int) -> Int -> LogRecord -> LogRecord -> Map.Map Int (Map.Map Int Int)
        addData m id l1 l2 = Map.insert id (updateMinutes (Map.findWithDefault (Map.empty) id m) (time l1) (time l2)) m

part1_solve :: String -> Int
part1_solve = findGuard . buildMinutesAsleep Map.empty 0 . List.sortBy sortLogRecord . toLogs
    where
        getMaxValues :: [(Int,[(Int, Int)])] -> [(Int, (Int, Int))]
        getMaxValues [] = [(0, (0, 0))]
        getMaxValues (x:xs) = (fst x, ((getSum . snd) x , (getMax . snd) x)) : getMaxValues xs
            where
                getMax :: [(Int, Int)] -> Int
                getMax xs = fst (List.maximumBy (comparing snd) xs)
                getSum xs = sum (map snd xs)
        getValue :: (Int, (Int, Int)) -> Int
        getValue (id,(_,max)) = id * max
        findGuard :: Map.Map Int (Map.Map Int Int) -> Int
        findGuard = getValue . List.maximumBy (comparing (fst . snd)) . getMaxValues . map (\(k,v) -> (k, Map.toList v)) . Map.toList

part2_solve :: String -> Int
part2_solve = findGuard . buildMinutesAsleep Map.empty 0 . List.sortBy sortLogRecord . toLogs
    where
        getMostFrequent :: [(Int, [(Int, Int)])] -> [(Int, (Int, Int))]
        getMostFrequent [] = []
        getMostFrequent ((id, v):xs) = (id, List.maximumBy (comparing snd) v) : getMostFrequent xs
        getValue :: (Int, (Int, Int)) -> Int
        getValue (id,(min,_)) = id * min
        findGuard :: Map.Map Int (Map.Map Int Int) -> Int
        findGuard = getValue . List.maximumBy (comparing (snd . snd)) . getMostFrequent . map (\(k,v) -> (k, Map.toList v)) . Map.toList

solveFromFile :: String -> (String -> a) -> IO a
solveFromFile s f = do
    input_data <- readFile s
    return (f input_data)

responseRecord :: IO ()
responseRecord = do
    let solve = solveFromFile "day4/input.txt"
    part1_result <- solve part1_solve
    print part1_result
    part2_result <- solve part2_solve
    print part2_result
