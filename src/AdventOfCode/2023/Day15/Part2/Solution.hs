{-# Language OverloadedStrings #-}
module AdventOfCode.Day14.Part2.Solution where

import qualified Data.Text as T
import qualified Data.Ord as O
import Data.List (sortBy, intercalate, transpose)
import qualified Data.Map as Map
import Control.Monad.State (State, get, runState)
import Control.Monad.State.Lazy (modify)
import Data.Maybe (fromJust)
import Control.Monad.Writer (Writer,  tell, runWriter, execWriter)


solution:: IO ()
solution = do
    platform  <- baseSolution
    let outputFilePath =  "src/AdventOfCode/2023/Day14/Part2/inputOutputPlatform.txt"
        loopIdx = fromJust $ hareAndTortoise platform platform 100000 0

        offset = loopIdx
        cycleLength = loopIdx
        numTimes = (1000000000  - offset) `mod` cycleLength + offset
        combo = runState (spinPlatform' numTimes 0 platform) Map.empty
        spunPlatform = fst combo
    writeFile outputFilePath (unlines spunPlatform)
    print (calculateLoad spunPlatform)

baseSolution :: IO Platform
baseSolution = do
    let inputFilePath = "src/AdventOfCode/2023/Day14/Part2/input.txt"
    content <- readFile inputFilePath
    return $ lines content

-- examine pattern of load
printFirst1000Loads:: IO ()
printFirst1000Loads = do
    platform  <- baseSolution
    let outputFilePath =  "src/AdventOfCode/2023/Day14/Part2/inputOutputLoad.txt"
        output = runWriter (spinPlatform'' 1000 platform)
    writeFile outputFilePath (unlines . map show . snd $ output)


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

hareAndTortoise:: Platform -> Platform -> Int -> Int -> Maybe Int
hareAndTortoise turtleP hazeP maxRuns cnt =
    if maxRuns == cnt
        then Nothing
        else let turtleP' = spinPlatform turtleP
                 hazeP' = spinPlatform . spinPlatform $ hazeP in
             if concat turtleP' == concat hazeP'
                then return $ cnt + 1
                else hareAndTortoise turtleP' hazeP' maxRuns (cnt+1)




spinPlatform'' :: Int -> Platform -> Writer [Int] Platform
spinPlatform'' spinNumTimes p
    | spinNumTimes < 0 = error "Number of Spins must be non-negative!"
    | spinNumTimes == 0 = return p
    | otherwise = let p' = spinPlatform p in
            do 
            tell [calculateLoad p']
            spinPlatform'' (spinNumTimes -1 ) p'




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