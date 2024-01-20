module AdventOfCode.Day1.Part2.Solution where
import Data.Char (isDigit)
import qualified Data.Map as Map


solution :: IO ()
solution =  do
    content <- readFile "src/AdventOfCode/2023/Day1/Part2/input.csv"
    let ls = lines content
    print $ getSum ls

debugSolution :: IO ()
debugSolution =  do
    content <- readFile "src/AdventOfCode/2023/Day1/Part2/input.csv"
    let ls = lines content
    mapM_ print (zip ls (map getNumber ls))


wordToInt :: Map.Map String Integer
wordToInt = Map.fromList [
        ("one", 1)
        ,("two", 2)
        ,("three", 3)
        ,("four", 4)
        ,("five", 5)
        ,("six", 6)
        ,("seven", 7)
        ,("eight", 8)
        ,("nine", 9)
    ]

lookups :: Ord k => [k] -> Map.Map k a -> Maybe a
lookups [] _ = Nothing
lookups (x:xs) mp = case Map.lookup x mp of
                        Just y -> Just y
                        Nothing -> lookups xs mp


getSum :: [String] -> Integer
getSum = sum . map getNumber


getNumber :: String -> Integer
getNumber line = fSt*10 + lSt
    where l =  lineToNumbers line [] []
          fSt = head l
          lSt = l !! (length l - 1)



lineToNumbers :: String -> [Integer] -> [String] -> [Integer]
lineToNumbers [] acc  _ = acc
lineToNumbers (x:xs) acc  combos
    | isDigit x = lineToNumbers xs (acc ++ [read [x]]) []
    | otherwise = case lookups nextCombos wordToInt of
                        Just y -> lineToNumbers xs (acc ++ [y]) nextCombos
                        Nothing -> lineToNumbers xs acc nextCombos
        where nextCombos = map (++ [x]) combos ++ [[x]]