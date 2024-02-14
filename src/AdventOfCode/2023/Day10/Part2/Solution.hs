module AdventOfCode.Day10.Part2.Solution where

import qualified Data.Map as Map

solution :: IO()
solution = do
    content <- readFile "src/AdventOfCode/2023/Day10/Part2/input.txt"
    let coordinateMap = parseMap . lines $ content
        startingPosition = getStartingCoordinate . lines $ content
        coordinateMap' = Map.insert startingPosition [South, North] coordinateMap -- override initial direction manually
        tracedCoordinates = traceCoordinates coordinateMap' startingPosition ((-1,-1), startingPosition) []

        showlaceCoordinates = reverse $ last tracedCoordinates : tracedCoordinates -- append last element to front for showlace algo. reverse to get clockwise direction.
        showlaceArea = calculateShoelaceArea showlaceCoordinates
        interiorArea = calculateInteriorArea showlaceArea (length tracedCoordinates)

    -- print coordinateMap'
    print startingPosition
    print . take 10  $ tracedCoordinates
    print . take 10 $ showlaceCoordinates
    print showlaceArea
    print interiorArea


type Pos = Int
type Coordinate = (Pos, Pos)
type CoordinateMap = Map.Map Coordinate [Direction]
data Direction = North
                 | South
                 | East
                 | West
                 | Unknown
                deriving (Show, Eq)

calculateShoelaceArea :: [Coordinate] -> Double
calculateShoelaceArea ((x1,y1):c2@(x2,y2): cs) = 0.5 * (fromIntegral y1 + fromIntegral y2) * (fromIntegral x1 - fromIntegral x2) + calculateShoelaceArea (c2:cs)
calculateShoelaceArea [_] = 0 -- base

calculateInteriorArea :: Double -> Int -> Double
calculateInteriorArea area numBoundaryPoints = (area + 1) - (fromIntegral numBoundaryPoints / 2)


-- isTileWithin :: Coordinate -> [Coordinate] -> Bool
-- isTileWithin c@(x1, y1) tracedCoordinates =
--     not (c `elem` filteredTracedCoordinates || even (length filteredTracedCoordinates))
--     where filteredTracedCoordinates = filter (\(x2, y2) -> x1 == x2 && y1 >= y2) tracedCoordinates


traceCoordinates :: CoordinateMap -> Coordinate -> (Coordinate, Coordinate) -> [Coordinate] -> [Coordinate]
traceCoordinates mp startingCoordinate (prevCoordinate, currCoordinate) xs =
    if nextCoordinate /= startingCoordinate
        then traceCoordinates mp startingCoordinate (currCoordinate, nextCoordinate) (currCoordinate:xs)
        else currCoordinate:xs -- terminate when it returns to starting point
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




