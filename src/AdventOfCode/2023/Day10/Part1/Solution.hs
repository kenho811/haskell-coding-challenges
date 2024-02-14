module AdventOfCode.Day9.Part1.Solution where

import qualified Data.Map as Map

solution :: IO()
solution = do
    content <- readFile "src/AdventOfCode/2023/Day10/Part1/input.txt"
    let coordinateMap = parseMap . lines $ content
        startingPosition = getStartingCoordinate . lines $ content
        coordinateMap' = Map.insert startingPosition [North, South] coordinateMap -- override initial direction manually
        totalSteps = getSteps coordinateMap' startingPosition ((-1,-1), startingPosition) 0

    -- print coordinateMap'
    print . take 10 . Map.toList $ coordinateMap'
    print  totalSteps -- floor division
    -- print startingPosition
    print . (`div` 2) $ totalSteps -- floor division


type Pos = Int
type Coordinate = (Pos, Pos)
type CoordinateMap = Map.Map Coordinate [Direction]
data Direction = North
                 | South
                 | East
                 | West
                 | Unknown
                deriving (Show, Eq)

getSteps :: CoordinateMap -> Coordinate -> (Coordinate, Coordinate) -> Int -> Int
getSteps mp startingCoordinate (prevCoordinate, currCoordinate) acc =
    if nextCoordinate /= startingCoordinate
        then getSteps mp startingCoordinate (currCoordinate, nextCoordinate) (acc + 1)
        else acc + 1 -- terminate when it returns to starting point
    where nextCoordinates = getNextCoordinates currCoordinate mp
          nextCoordinate = head . filter (/= prevCoordinate)   $ nextCoordinates

getOppositeDirection :: Direction -> Direction
getOppositeDirection North = South
getOppositeDirection South = North
getOppositeDirection East = West
getOppositeDirection West = East
getOppositeDirection x = error $ "Unhandled Direction " ++ show x


moveCoordinate :: Direction -> Coordinate -> Coordinate
moveCoordinate North (x1, y1)  = (x1 -1, y1)
moveCoordinate South (x1, y1)  = (x1 +1, y1)
moveCoordinate East (x1, y1)  = (x1, y1 + 1)
moveCoordinate West (x1, y1)  = (x1, y1 - 1)
moveCoordinate x _ = error $ "Unknown direction "  ++ show x


getNextCoordinates :: Coordinate -> CoordinateMap -> [Coordinate]
getNextCoordinates curr mp = case Map.lookup curr mp of
    Nothing -> error $ "Cannot find coordinate " ++ show curr ++ " in the map!!!"
    Just ds -> [moveCoordinate d curr | d <- ds]

parseMap :: [[Char]] -> CoordinateMap -- return coordinate map and starting position
parseMap xxs = Map.fromList [((rowNum, colNum), parseDirection x) | (rowNum, xs) <- zip [0.. ] xxs, (colNum, x) <- zip [0..] xs ]


getStartingCoordinate :: [[Char]] -> Coordinate -- return coordinate map and starting position
getStartingCoordinate xxs = head [(rowNum, colNum) | (rowNum, xs) <- zip [0.. ] xxs, (colNum, x) <- zip [0..] xs, x == 'S' ]

parseDirection :: Char -> [Direction]
parseDirection '|' = [North, South]
parseDirection '-' = [East, West]
parseDirection 'L' = [North, East]
parseDirection 'J' = [North, West]
parseDirection '7' = [South, West]
parseDirection 'F' = [South, East]
parseDirection 'S' = [Unknown]
parseDirection '.' = []
parseDirection x = error $ "No direction is defined for " ++ [x]




