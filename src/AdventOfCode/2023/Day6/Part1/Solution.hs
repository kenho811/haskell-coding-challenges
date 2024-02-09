module AdventOfCode.Day5.Part1.Solution where



solution:: IO ()
solution = do
    content <- readFile "src/AdventOfCode/2023/Day6/Part1/input.txt"
    let num1 = numDistanceGreaterThan (Distance 434) . getDistances $ Time 58
        num2 = numDistanceGreaterThan (Distance 1041) . getDistances $ Time 81
        num3 = numDistanceGreaterThan (Distance 2219) . getDistances $ Time 96
        num4 = numDistanceGreaterThan (Distance 1218) . getDistances $ Time 76
    print $ num1*num2*num3*num4

newtype Distance = Distance {
    unDistance:: Int
} deriving Show

newtype Time = Time { 
    unTime:: Int
} deriving Show

numDistanceGreaterThan :: Distance -> [Distance] -> Int
numDistanceGreaterThan (Distance lowerBound) = length . filter (> lowerBound) . map unDistance


getDistances:: Time -> [Distance]
getDistances x = 
    let times = splitTime x in
        map (uncurry getDistance) times


splitTime:: Time -> [(Time, Time)]
splitTime (Time x) = [(Time i, Time (x-i)) | i <- [0..x]]


getDistance :: Time -> Time -> Distance
getDistance speedUpTime runTime =
    let speedUpMilliseconds =  unTime speedUpTime
        runTimeMilliseconds = unTime runTime in
    Distance (speedUpMilliseconds * runTimeMilliseconds)



