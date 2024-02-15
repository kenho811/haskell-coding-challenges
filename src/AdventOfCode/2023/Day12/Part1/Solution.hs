module AdventOfCode.Day12.Part1.Solution where

import qualified Data.Text as T


solution :: IO ()
solution = do
    content <- readFile "src/AdventOfCode/2023/Day12/Part1/input.txt"
    let 
        conditionsAndBrokenNums :: [([Condition], BrokenNum)]
        conditionsAndBrokenNums = zip conditions brokenNums

        conditions :: [[Condition]]
        conditions = map (map parseCondition . head . words) . lines $ content

        brokenNums :: [BrokenNum]
        brokenNums = map (map (read . T.unpack) . T.splitOn (T.pack ",") . last . T.words) . T.lines . T.pack $ content

    -- print conditions
    -- print brokenNums
    print conditionsAndBrokenNums


type BrokenNum = [Int]
data Condition = Operational
                 | Broken
                 | Unknown deriving Show

parseCondition :: Char -> Condition
parseCondition '.' = Operational
parseCondition '#' = Broken
parseCondition '?' = Unknown
parseCondition x = error $ "Unknown Condition " ++ show x

generateRecords :: [Condition] -> [[Condition]]
generateRecords  = map reverse . foldr (
    \c acc -> case c of
        Unknown -> [ c': xs | xs <- acc, c' <- [Operational, Broken]]
        _ -> [c: xs | xs <- acc]
    ) [[]]

