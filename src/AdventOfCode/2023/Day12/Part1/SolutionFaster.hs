module AdventOfCode.Day12.Part1.SolutionFaster where

import qualified Data.Text as T

solution :: IO ()
solution = do
    content <- readFile "src/AdventOfCode/2023/Day12/Part1/input.txt"
    -- content <- readFile "src/AdventOfCode/2023/Day12/Part1/sample.txt"
    let
        conditionsAndBrokenNumsList :: [(Conditions, BrokenNums)]
        conditionsAndBrokenNumsList = zip conditionsListUpdated brokenNumsList

        conditionsList :: [Conditions]
        -- add final Operational state to ease algorithm
        conditionsList = map ( map parseCondition . head . words) . lines $ content
        conditionsListUpdated = map (++ [Operational]) conditionsList

        brokenNumsList :: [BrokenNums]
        brokenNumsList = map (map (read . T.unpack) . T.splitOn (T.pack ",") . last . T.words) . T.lines . T.pack $ content

        numMatchingPossibilities :: [Int]
        numMatchingPossibilities =  map (\(cs, ns) -> getNumMatches cs ns 0 ) conditionsAndBrokenNumsList

    -- print  conditionsList
    -- print  conditionsList'
    print  conditionsListUpdated
    print  brokenNumsList

    -- print  numMatchingPossibilities
    print  numMatchingPossibilities
    print . sum $ numMatchingPossibilities


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


getNumMatches :: Conditions -> BrokenNums -> Int -> Int
getNumMatches [] ns _   = if null ns then 1 else 0
getNumMatches cs [] _   = if Broken `notElem` cs then 1 else 0
getNumMatches (c:cs) (n:ns) cnt  = case c of
    Operational ->  if cnt == 0 -- no previous broken states. Continue
                    then  getNumMatches cs (n:ns) 0
                    else
                        if cnt == n  -- check if previous non-zero continguous BrokenNum matches expected one
                        then getNumMatches cs ns 0  -- continue
                        else 0 -- Not Match. Therefore abort.
    Broken -> getNumMatches cs (n:ns) (cnt + 1)
    Unknown -> getNumMatches (Operational:cs) (n:ns) cnt  + getNumMatches (Broken:cs) (n:ns) cnt


