{-# Language OverloadedStrings #-}
module AdventOfCode.Day14.Part1.Solution where

import qualified Data.Text as T
import qualified Data.Ord as O
import Data.List (sortBy, intercalate, transpose)





solution :: IO()
solution = do
    let 
        -- inputFilePath = "src/AdventOfCode/2023/Day14/Part1/sample.txt"
        inputFilePath = "src/AdventOfCode/2023/Day14/Part1/input.txt"
        -- outputFilePath =  "src/AdventOfCode/2023/Day14/Part1/sampleOutput.txt"
        outputFilePath =  "src/AdventOfCode/2023/Day14/Part1/inputOutput.txt"


    content <- readFile inputFilePath
    let platform  = lines content
        slidedToNorthPlatform = transpose . map rearrangeRocks . transpose $ platform
        load = calculateLoad slidedToNorthPlatform
    dumpTransformed slidedToNorthPlatform outputFilePath
    print . unlines $ slidedToNorthPlatform
    print load

type Rocks = String
type Platform = [Rocks]

dumpTransformed :: Platform -> FilePath -> IO()
dumpTransformed p fp = writeFile fp (unlines p)


rearrangeRocks :: Rocks -> Rocks
rearrangeRocks = intercalate "#" . map (sortBy (O.comparing O.Down) . T.unpack) . T.splitOn "#" . T.pack


calculateLoad :: Platform -> Int
calculateLoad p =
     let
          platformLength = length p
          rocksWithWeight = zip (reverse [1 .. platformLength]) p in 
    foldr (
        \(weight, rocks) acc ->
            weight * (length . filter (=='O') $ rocks) + acc
    ) 0 rocksWithWeight