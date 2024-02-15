module AdventOfCode.Day11.Part1.Solution where
import Data.List (transpose, sort)


solution :: IO()
solution = do
    content <- readFile "src/AdventOfCode/2023/Day11/Part1/sample.txt"
    -- content <- readFile "src/AdventOfCode/2023/Day11/Part1/input.txt"
    let cosmicGrid :: CosmicGrid
        cosmicGrid = map (map parseCosmicObject) . lines $ content
        expandedCosmicGrid = expandCosmicGrid 2 cosmicGrid
        -- expandedCosmicGrid = expandCosmicGrid 10 cosmicGrid

        galaxyCoordinates = getCosmicObjectCoordinates CosmicGalaxy expandedCosmicGrid
        galaxyCoordinatesPairs = getCombosByTwo galaxyCoordinates
        galaxyDistance = map (uncurry getDistance) galaxyCoordinatesPairs

    -- print cosmicGrid
    print . length $ cosmicGrid
    print . length . transpose $ cosmicGrid

    -- print expandedCosmicGrid
    print . length $ expandedCosmicGrid
    print . length . transpose $ expandedCosmicGrid


    print galaxyCoordinates
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

getDistance :: Coordinate -> Coordinate -> Int
getDistance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)


getCombosByTwo :: [a] -> [(a, a)]
getCombosByTwo (x:xs) = getCombosByTwo' x xs ++ getCombosByTwo xs
    where getCombosByTwo' :: a -> [a] -> [(a,a)]
          getCombosByTwo' z (y:ys) = (z,y) : getCombosByTwo' z ys
          getCombosByTwo' _ [] = []
getCombosByTwo [] = []

expandCosmicGrid :: Int -> CosmicGrid -> CosmicGrid
expandCosmicGrid factor = transpose . expandCosmicGrid' . transpose . expandCosmicGrid'
    where expandCosmicGrid' :: CosmicGrid -> CosmicGrid
          expandCosmicGrid' = reverse . foldr (
                \xs acc -> if willExpand xs
                    then replicate factor xs ++ acc
                    else xs: acc
            ) [[]]
          willExpand :: [CosmicObject] -> Bool
          willExpand = all (==CosmicSpace)

