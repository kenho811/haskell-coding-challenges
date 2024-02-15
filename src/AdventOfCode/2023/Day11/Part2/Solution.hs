module AdventOfCode.Day11.Part2.Solution where
import Data.List (transpose, sort)


solution :: IO()
solution = do
    -- content <- readFile "src/AdventOfCode/2023/Day11/Part2/sample.txt"
    content <- readFile "src/AdventOfCode/2023/Day11/Part2/input.txt"
    let cosmicGrid :: CosmicGrid
        cosmicGrid = map (map parseCosmicObject) .  lines $ content

        rowsToExpand = getRowsToExpand cosmicGrid
        columnsToExpand = getColumnsToExpand cosmicGrid
        rowFactor = 1000000
        colFactor = 1000000
        -- rowFactor = 100
        -- colFactor = 100

        galaxyCoordinates = getCosmicObjectCoordinates CosmicGalaxy cosmicGrid
        galaxyCoordinatesPairs = getCombosByTwo galaxyCoordinates
        galaxyDistance = map (\ (c1, c2) -> getDistance c1 c2 rowFactor colFactor rowsToExpand columnsToExpand) galaxyCoordinatesPairs

    -- print cosmicGrid
    print . length $ cosmicGrid
    print $ "Expanded Rows " ++ show rowsToExpand
    print . length . transpose $ cosmicGrid
    print $ "Expanded Columns " ++ show columnsToExpand

    print galaxyCoordinatesPairs
    print galaxyDistance
    print .sum $ galaxyDistance

type CosmicGrid = [[CosmicObject]]

type Coordinate = (Int, Int)

data CosmicObject = CosmicSpace
                    | CosmicGalaxy deriving (Show, Eq)


parseCosmicObject :: Char -> CosmicObject
parseCosmicObject '.' = CosmicSpace
parseCosmicObject '#' = CosmicGalaxy
parseCosmicObject x = error $ "Undefined Cosmic Object " ++ show x


getCosmicObjectCoordinates :: CosmicObject -> CosmicGrid -> [Coordinate]
getCosmicObjectCoordinates co xxs = [(rowNum, colNum) | (rowNum, xs) <- zip [0..] xxs, (colNum, x) <- zip [0..] xs, x == co]

getDistance :: Coordinate -> Coordinate -> Int -> Int -> [Int] -> [Int] -> Int
getDistance (x1, y1) (x2, y2) rowFactor columnFactor rowsToExpand colsToExpand = basicDistance + additionalX + additionalY
    where basicDistance = abs (x2 - x1) + abs (y2 - y1)
          additionalX = sum . (`replicate` (columnFactor - 1)) . length . filter (\col -> col >= min y1 y2 && col <max y1 y2 ) $ colsToExpand
          additionalY = sum . (`replicate` (rowFactor -1)) . length . filter (\row -> row >= min x1 x2 && row <max x1 x2 ) $ rowsToExpand


getCombosByTwo :: [a] -> [(a, a)]
getCombosByTwo (x:xs) = getCombosByTwo' x xs ++ getCombosByTwo xs
    where getCombosByTwo' :: a -> [a] -> [(a,a)]
          getCombosByTwo' z (y:ys) = (z,y) : getCombosByTwo' z ys
          getCombosByTwo' _ [] = []
getCombosByTwo [] = []

getRowsToExpand :: CosmicGrid -> [Int]
getRowsToExpand = map fst . filter (\(_,c) -> willExpand c) . zip [0..]

getColumnsToExpand :: CosmicGrid -> [Int]
getColumnsToExpand = map fst . filter (\(_,c) -> willExpand c) . zip [0..] . transpose

willExpand :: [CosmicObject] -> Bool
willExpand = all (==CosmicSpace)

