module AdventOfCode.Day5.Part1.Solution where

import qualified Data.Text as T
import Data.Function ((&))


solution:: IO ()
solution = do
    content <- readFile "src/AdventOfCode/2023/Day5/Part1/input.txt"
    let ls = lines content
        seeds:: [Int]
        seeds = map read $ tail $ words $ head ls
        ranges = buildSeedRanges seeds
        seedToSoil = generateListFromLines ls 4 45
        soliToFertilizer = generateListFromLines ls 48 96
        fertilizerToWater = generateListFromLines ls 99 130
        waterToLight = generateListFromLines ls 133 179
        lightToTemperature = generateListFromLines ls 182 202
        temperatureToHumidity = generateListFromLines ls 205 241
        humidityToLocation = generateListFromLines ls 244 279

        locations :: [Range]
        locations = do
            range <- ranges
            return $ ([], [range]) & mapToDests seedToSoil
                          & mapToDests soliToFertilizer
                          & mapToDests fertilizerToWater
                          & mapToDests waterToLight
                          & mapToDests lightToTemperature
                          & mapToDests temperatureToHumidity
                          & mapToDests humidityToLocation

    print ranges


-- inclusive of being and end
data Range = Range Int Int deriving (Show)

-- FIXME: Create an instance for MonadRange

generateListFromLines :: [String] -> Int -> Int -> [(Source, Destination, Step)]
generateListFromLines ls from to = filteredTups
    where filteredLs = take (to - from +1) $ drop (from - 1) ls
          filteredTups = [ breakLineToTuples filteredLn | filteredLn <- filteredLs]

mapToDests:: [(Destination, Source, Step)] -> ([Range],[Range]) -> [Range]
mapToDests [] (xs,ys) = xs ++ ys
mapToDests (x:ss) (xs, ys) =
        let resultList = map (mapToDest x) ys in
            mapToDests ss (concatMap fst resultList ++ xs, concatMap snd resultList ++ ys)

mapToDest:: (Destination, Source, Step) -> Range -> ([Range], [Range])
mapToDest (dest, src, step) (Range start end)
    | (start < src && end < src) ||  (start > srcInclusiveEnd && end > srcInclusiveEnd) = ([],[Range start end])
    | start >= src && end <= srcInclusiveEnd = ([Range (dest + (start - src)) (dest + (end - src))], [])
    | start < src && end > srcInclusiveEnd  = ([Range dest destInclusiveEnd], [Range start (src -1), Range (srcInclusiveEnd + 1) end])
    | start < src && end <= srcInclusiveEnd  = ([Range dest (dest + end - src)], [Range start (src -1)])
    | start >= src && end > srcInclusiveEnd  = ([Range (dest + (start -src)) destInclusiveEnd], [Range (srcInclusiveEnd + 1) end])
    | otherwise = ([],[Range start end])
    where srcInclusiveEnd = src + step - 1
          destInclusiveEnd = dest + step - 1





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




buildSeedRange:: Int -> Int -> Range
buildSeedRange start step = Range start (start + step - 1)

buildSeedRanges :: [Int] -> [Range]
buildSeedRanges [] = []
buildSeedRanges [_] = error "bad input."
buildSeedRanges (x:y:ss) = buildSeedRange x y : buildSeedRanges ss
