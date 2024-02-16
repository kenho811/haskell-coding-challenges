module AdventOfCode.Day12.Part1.SolutionFaster where

{- Greedy Algorithm inspect each spring and stop when first failure is met-}

import qualified Data.Text as T
import Data.List.GroupBy ( groupBy ) 


solution :: IO ()
solution = do
    content <- readFile "src/AdventOfCode/2023/Day12/Part1/input.txt"
    -- content <- readFile "src/AdventOfCode/2023/Day12/Part1/sample.txt"
    let 
        conditionsAndBrokenNumsList :: [(Conditions, BrokenNums)]
        conditionsAndBrokenNumsList = zip conditionsList brokenNumsList

        conditionsList :: [Conditions]
        conditionsList = map (map parseCondition . head . words) . lines $ content

        brokenNumsList :: [BrokenNums]
        brokenNumsList = map (map (read . T.unpack) . T.splitOn (T.pack ",") . last . T.words) . T.lines . T.pack $ content

        numMatchingPossibilities :: [Int]
        numMatchingPossibilities =  map (uncurry getNumMatches) conditionsAndBrokenNumsList


    -- print conditions
    -- print brokenNums
    -- print .  map (generatePossibleConditions . fst) $ conditionsAndBrokenNumsList
    print  numMatchingPossibilities
    print . sum $ numMatchingPossibilities


type Conditions = [Condition]
type BrokenNums = [Int]
data Condition = Operational
                 | Broken
                 | Unknown deriving (Show, Eq)

getNumMatches :: Conditions -> BrokenNums -> Int
getNumMatches cs targetBrokenNums = length . filter (==targetBrokenNums) $ possibleBrokenNums
    where possibleConditions = generatePossibleConditions cs
          possibleBrokenNums = map getBrokenNum possibleConditions  


getBrokenNum :: Conditions -> BrokenNums
getBrokenNum = map length 
              . groupBy (\a b -> b == a+1 ) 
              . map fst 
              . filter (\(_, c) -> c == Broken ) 
              .  zip [0..]

parseCondition :: Char -> Condition
parseCondition '.' = Operational
parseCondition '#' = Broken
parseCondition '?' = Unknown
parseCondition x = error $ "Unknown Condition " ++ show x

generatePossibleConditions :: [Condition] -> [[Condition]]
generatePossibleConditions  = foldr (
    \c acc -> case c of
        Unknown -> [ c': xs | xs <- acc, c' <- [Operational, Broken]]
        _ -> [c: xs | xs <- acc]
    ) [[]]


simpleTest :: IO ()
simpleTest = do
    let conditions = map parseCondition "???.###"
        possibleConditions = generatePossibleConditions conditions
        possibleBrokenNums = map getBrokenNum possibleConditions
    print conditions
    print possibleConditions
    print possibleBrokenNums