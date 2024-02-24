{-# Language OverloadedStrings #-}
module AdventOfCode.Day14.Part2.Solution where

import qualified Data.Text as T
import qualified Data.Ord as O
import Data.List (sortBy, intercalate, transpose)
import qualified Data.Map as Map
import Data.Foldable (foldl')
import Control.Monad.State (State, get, put, runState, execState, evalState)
import Control.Monad.State.Lazy (modify)
import Data.Maybe (fromJust)




solution :: IO()
solution = do
    let
        inputFilePath = "src/AdventOfCode/2023/Day14/Part2/sample.txt"
        -- inputFilePath = "src/AdventOfCode/2023/Day14/Part2/input.txt"
        -- outputFilePath =  "src/AdventOfCode/2023/Day14/Part2/sampleOutputOnce.txt"
        -- outputFilePath =  "src/AdventOfCode/2023/Day14/Part2/sampleOutputTwice.txt"
        outputFilePath =  "src/AdventOfCode/2023/Day14/Part2/inputOutput.txt"


    content <- readFile inputFilePath
    let platform  = lines content
        loopIdx = hazeAndTurtleAlgo platform platform 100000 0

        numTimes =  1000000000 `mod` fromJust loopIdx
        combo = runState (spinPlatform' numTimes 0 platform) Map.empty
        spunPlatform = fst combo
        memo = snd combo
        load = calculateLoad spunPlatform

    print memo
    print loopIdx
    print spunPlatform
    print $ Map.lookup spunPlatform memo
    print "dumping"
    dumpTransformed spunPlatform outputFilePath
    -- print . unlines $ slidedToNorthPlatform
    print load

type Rocks = String
type Platform = [Rocks]

data TwoDimensionalDirection = North
                 | West
                 | South
                 | East
data OneDimensionalDirection = OneDLeft
                              | OneDRight


dumpTransformed :: Platform -> FilePath -> IO()
dumpTransformed p fp = writeFile fp (unlines p)

hazeAndTurtleAlgo:: Platform -> Platform -> Int -> Int -> Maybe Int
hazeAndTurtleAlgo turtleP hazeP maxRuns cnt =
    if maxRuns == cnt
        then Nothing
        else let turtleP' = spinPlatform turtleP
                 hazeP' = spinPlatform . spinPlatform $ hazeP in
             if turtleP' == hazeP'
                then return $ cnt + 1
                else hazeAndTurtleAlgo turtleP' hazeP' maxRuns (cnt+1)







spinPlatform' :: Int -> Int -> Platform -> State (Map.Map Platform (Platform, Int)) Platform
spinPlatform' spinNumTimes idx p
    | spinNumTimes < 0 = error "Number of Spins must be non-negative!"
    | spinNumTimes == 0 = return p
    | otherwise = do
        mp <- get
        case Map.lookup p mp of
            Just (x,_) -> spinPlatform' (spinNumTimes-1) (idx+1) x
            Nothing ->
                let newPlatform = spinPlatform p in do
                    modify (Map.insert p (newPlatform, idx+1))
                    spinPlatform' (spinNumTimes-1) (idx+1) newPlatform

spinPlatform :: Platform -> Platform
spinPlatform = rearrangePlatform East
    . rearrangePlatform South
    . rearrangePlatform West
    . rearrangePlatform North

rearrangePlatform :: TwoDimensionalDirection -> Platform -> Platform
rearrangePlatform North platform = transpose . map (rearrangeRocks OneDLeft) . transpose $ platform
rearrangePlatform South platform = transpose . map (rearrangeRocks OneDRight) . transpose $ platform
rearrangePlatform East platform = map (rearrangeRocks OneDRight) platform
rearrangePlatform West platform = map (rearrangeRocks OneDLeft) platform



rearrangeRocks :: OneDimensionalDirection -> Rocks -> Rocks
rearrangeRocks OneDLeft = intercalate "#" . map (sortBy (O.comparing O.Down) . T.unpack) . T.splitOn "#" . T.pack
rearrangeRocks OneDRight = intercalate "#" . map (sortBy (O.comparing id)  . T.unpack) . T.splitOn "#" . T.pack


calculateLoad :: Platform -> Int
calculateLoad p =
     let
          platformLength = length p
          rocksWithWeight = zip (reverse [1 .. platformLength]) p in
    foldr (
        \(weight, rocks) acc ->
            weight * (length . filter (=='O') $ rocks) + acc
    ) 0 rocksWithWeight