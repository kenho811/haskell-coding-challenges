module AdventOfCode.Day16.Part1.Solution where

import qualified Data.Map as Map

solution :: IO ()
solution = do
    let
        -- inputFilePath = "src/AdventOfCode/2023/Day16/Part1/sample.txt"
        inputFilePath = "src/AdventOfCode/2023/Day16/Part1/input.txt"
        -- outputFilePath =  "src/AdventOfCode/2023/Day16/Part1/sampleOutput.txt"
        outputFilePath =  "src/AdventOfCode/2023/Day16/Part1/inputOutput.txt"

    content <- readFile inputFilePath

    let grid = Map.fromList [((rowNum, colNum), t) | (rowNum, ln) <- zip [0.. ] (lines content), (colNum, t) <- zip [0..] ln]
    print grid


type Coordinate = (Int, Int)
data Direction = East 
                | South
                | West
                | North
type Record = Map.Map (Coordinate, Direction) [Coordinate]

type Tile = Char
type Grid = Map.Map Coordinate Tile


