module AdventOfCode.Day5.Part1.Solution where



solution:: IO ()
solution = do
    content <- readFile "src/AdventOfCode/2023/Day6/Part1/input.txt"
    let filteredAwayLowerBound = getMinimumSpeedUpTimeGreaterThanDistance (Time 58819676) (Distance 434104122191218)
    let filteredAwayUpperBound = getMaximumSpeedUpTimeGreaterThanDistance (Time 58819676) (Distance 434104122191218)
    print filteredAwayLowerBound
    print filteredAwayUpperBound
    print ((58819676 - filteredAwayUpperBound) -filteredAwayLowerBound + 1)

newtype Distance = Distance {
    unDistance:: Int
} deriving Show

newtype Time = Time {
    unTime:: Int
} deriving Show


-- assume ascending order of the list
filterAwayCount:: (a -> Bool) -> [a] -> Int -> Int
filterAwayCount _ [] acc = acc
filterAwayCount p (x:xs) acc = if p x then acc else filterAwayCount p xs (acc + 1)


getMinimumSpeedUpTimeGreaterThanDistance:: Time -> Distance -> Int
getMinimumSpeedUpTimeGreaterThanDistance x (Distance d) =
    let times = splitTime x
        distances =  map (unDistance . uncurry getDistance) $ times in
        filterAwayCount (>d) distances 0

getMaximumSpeedUpTimeGreaterThanDistance:: Time -> Distance -> Int
getMaximumSpeedUpTimeGreaterThanDistance x (Distance d) =
    let times = splitTimeInverted x
        distances =  map (unDistance . uncurry getDistance) $ times in
        filterAwayCount (>d) distances 0



splitTime:: Time -> [(Time, Time)]
splitTime (Time x) = [(Time i, Time (x-i)) | i <- [0..x]]

splitTimeInverted:: Time -> [(Time, Time)]
splitTimeInverted (Time x) = [(Time (x-i), Time i) | i <- [0..x]]


getDistance :: Time -> Time -> Distance
getDistance speedUpTime runTime =
    let speedUpMilliseconds =  unTime speedUpTime
        runTimeMilliseconds = unTime runTime in
    Distance (speedUpMilliseconds * runTimeMilliseconds)



