module AdventOfCode.Day13.Part1.Solution where
import Data.List (groupBy, transpose)
import qualified Data.IntMap as IMap

solution :: IO()
solution = do
    -- content <- readFile "src/AdventOfCode/2023/Day13/Part1/sample.txt"
    content <- readFile "src/AdventOfCode/2023/Day13/Part1/input.txt"
    let valleys :: [Valley]
        valleys = map (filter (not . null)) . groupBy (\_ b -> not . null $ b) . lines $ content

        horizontalReflections = concatMap getHorizontalReflectionLines valleys
        verticalReflections = concatMap getVerticalLines valleys
        scores = getScore verticalReflections horizontalReflections

        vly = transpose . head $ valleys
        valleyWithLocation = IMap.fromList $ zip [1..] vly
        totalLines = length valleyWithLocation  
        idx = 4
        comparisonLines = getLinePairsAlong (idx, idx+1) totalLines

    print valleys
    print valleyWithLocation
    print totalLines
    print comparisonLines
    print . getReflectionLines $ vly
    print scores





type Valley = [String]
type VerticalReflectionLine = Int
type HorizontalReflectionLine = Int

getScore :: [VerticalReflectionLine] -> [HorizontalReflectionLine] -> Int
getScore vs hs = sum $ vs ++ map (*100) hs


getVerticalLines :: Valley -> [Int]
getVerticalLines = getReflectionLines . transpose

getHorizontalReflectionLines :: Valley -> [Int]
getHorizontalReflectionLines = getReflectionLines


getReflectionLines :: Valley -> [Int]
getReflectionLines vly = foldr (
        \(idx, _) acc 
            -> let comparisonLines = getLinePairsAlong (idx, idx+1) totalLines in 
                if (idx /= totalLines) -- skip last line
                     && all (\(upLineNum, downLineNum) -> IMap.lookup upLineNum valleyWithLocation == IMap.lookup downLineNum valleyWithLocation) comparisonLines then idx:acc else acc
    ) [] (IMap.toList valleyWithLocation)
    where valleyWithLocation = IMap.fromList $ zip [1..] vly
          totalLines = length valleyWithLocation  


getLinePairsAlong :: (Int, Int) -> Int -> [(Int, Int)]
getLinePairsAlong (up, down) total 
    | up > 0 && down <= total =  (up, down) : getLinePairsAlong (up -1,down + 1) total
    | otherwise = []