module AdventOfCode.Day12.Part2.Solution where

import qualified Data.Text as T
import Data.List (intercalate)


solution :: IO ()
solution = do
    -- content <- readFile "src/AdventOfCode/2023/Day12/Part2/input.txt"
    content <- readFile "src/AdventOfCode/2023/Day12/Part2/sample.txt"
    let
        conditionsAndBrokenNumsList :: [(Conditions, BrokenNums)]
        conditionsAndBrokenNumsList = zip conditionsListUpdated brokenNumsList
        conditionsAndBrokenNumsList' = zip conditionsListUpdated' brokenNumsList'

        conditionsList :: [Conditions]
        -- add final Operational state to ease algorithm
        conditionsList = map ( map parseCondition . head . words) . lines $ content
        conditionsListUpdated = map (++ [Operational]) conditionsList
        conditionsList' = map (intercalate [Unknown] . replicate 5) conditionsList
        conditionsListUpdated' = map (++ [Operational])  conditionsList'

        brokenNumsList :: [BrokenNums]
        brokenNumsList = map (map (read . T.unpack) . T.splitOn (T.pack ",") . last . T.words) . T.lines . T.pack $ content
        brokenNumsList' = map (concat . replicate 5) brokenNumsList

        numMatchingPossibilities :: [Int]
        numMatchingPossibilities =  map (\(cs, ns) -> length $ getMatches cs ns 0 [] ) conditionsAndBrokenNumsList
        numMatchingPossibilities' =  map (\(cs, ns) -> length $ getMatches cs ns 0 [] ) conditionsAndBrokenNumsList'

    -- print  conditionsList
    -- print  conditionsList'
    print  conditionsListUpdated'
    print  brokenNumsList'

    -- print  numMatchingPossibilities
    print  numMatchingPossibilities'
    print . sum $ numMatchingPossibilities'


type Conditions = [Condition]
type BrokenNums = [Int]
data Condition = Operational
                 | Broken
                 | Unknown deriving (Show, Eq)

parseCondition :: Char -> Condition
parseCondition '.' = Operational
parseCondition '#' = Broken
parseCondition '?' = Unknown
parseCondition x = error $ "Unknown Condition " ++ show x


getMatches :: Conditions -> BrokenNums -> Int -> Conditions -> [Conditions]
getMatches [] ns _ acc  = [reverse acc | null ns]
getMatches cs [] _  acc = [reverse (cs ++ acc) | Broken `notElem` cs]
getMatches (c:cs) (n:ns) cnt acc = case c of
    Operational ->  if cnt == 0 -- no previous broken states. Continue
                    then  getMatches cs (n:ns) 0 (c:acc)
                    else
                        if cnt == n  -- check if previous non-zero continguous BrokenNum matches expected one
                        then getMatches cs ns 0 (c:acc) -- continue
                        else [] -- Not Match. Therefore abort.
    Broken -> getMatches cs (n:ns) (cnt + 1) (c:acc)
    Unknown -> getMatches (Operational:cs) (n:ns) cnt acc ++ getMatches (Broken:cs) (n:ns) cnt acc


