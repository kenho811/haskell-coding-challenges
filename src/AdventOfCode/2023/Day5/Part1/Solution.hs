module AdventOfCode.Day5.Part1.Solution where

import qualified Data.Text as T
import Data.Function ((&))


solution:: IO ()
solution = do
    content <- readFile "src/AdventOfCode/2023/Day5/Part1/input.txt"
    let ls = lines content
        seeds:: [Int]
        seeds = map read $ tail $ words $ head ls
        seedToSoil = generateListFromLines ls 4 45 
        soliToFertilizer = generateListFromLines ls 48 96
        fertilizerToWater = generateListFromLines ls 99 130
        waterToLight = generateListFromLines ls 133 179
        lightToTemperature = generateListFromLines ls 182 202
        temperatureToHumidity = generateListFromLines ls 205 241
        humidityToLocation = generateListFromLines ls 244 279

        locations :: [Int]
        locations = do
            seed <- seeds
            return $ seed & mapToDests seedToSoil
                          & mapToDests soliToFertilizer
                          & mapToDests fertilizerToWater
                          & mapToDests waterToLight
                          & mapToDests lightToTemperature
                          & mapToDests temperatureToHumidity
                          & mapToDests humidityToLocation

    print seeds
    print locations
    print $ minimum locations



generateListFromLines :: [String] -> Int -> Int -> [(Source, Destination, Step)]
generateListFromLines ls from to = filteredTups
    where filteredLs = take (to - from +1) $ drop (from - 1) ls
          filteredTups = [ breakLineToTuples filteredLn | filteredLn <- filteredLs]

mapToDests:: [(Destination, Source, Step)] -> Source -> Destination
mapToDests [] src = src
mapToDests (x:xs) src = case mapToDest x src of
    Nothing -> mapToDests xs src
    Just y -> y


type Destination = Int
type Source = Int
type Step = Int
type Seed = T.Text

type SourceToDestinationMap = (Source, Destination)


getSeeds :: T.Text -> [Seed]
getSeeds  = T.words . last . T.splitOn (T.pack ":")

breakLineToTuples :: String -> (Destination, Source, Step)
breakLineToTuples ln = case words ln of
    [dest, src, step] -> (read dest, read src, read step)
    _ -> error "ERROR"


mapToDest:: (Destination, Source, Step) -> Source -> Maybe Destination
mapToDest (dest, src, step) a 
    | a >= src && a < src + step = Just (dest + (a - src))
    | otherwise = Nothing
    

