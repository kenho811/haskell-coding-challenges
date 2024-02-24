{-# Language OverloadedStrings #-}
module AdventOfCode.Day14.Part1.Solution where

import qualified Data.Text as T
import qualified Data.Ord as O
import Data.List (sortBy, intercalate, transpose)
import Data.Char (ord)





solution :: IO ()
solution = do
    let
        -- inputFilePath = "src/AdventOfCode/2023/Day15/Part1/sample.txt"
        inputFilePath = "src/AdventOfCode/2023/Day15/Part1/input.txt"
        -- outputFilePath =  "src/AdventOfCode/2023/Day15/Part1/sampleOutput.txt"
        outputFilePath =  "src/AdventOfCode/2023/Day15/Part1/inputOutput.txt"

    content <- readFile inputFilePath

    let strs = map T.unpack $ T.splitOn (T.pack ",") (T.pack content)
        nums = map runString strs

    print  nums
    print . sum $ nums



runChar :: Char -> Int -> Int
runChar c acc = ((ord c + acc)*17) `mod` 256

runString :: String -> Int
runString = foldl (flip runChar) 0