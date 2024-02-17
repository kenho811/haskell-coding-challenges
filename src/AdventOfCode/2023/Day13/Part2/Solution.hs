{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module AdventOfCode.Day13.Part1.Solution where
import Data.List (groupBy, transpose)
import qualified Data.IntMap as IMap
import Data.Maybe (fromJust)

transposeDump :: IO()
transposeDump = do
    let readPath = "src/AdventOfCode/2023/Day13/Part2/testInput.txt"
        writePath = "src/AdventOfCode/2023/Day13/Part2/testOutput.txt"
    content <- readFile readPath
    let content' = unlines .transpose . lines $ content

    writeFile writePath content'


solution :: IO()
solution = do
    -- content <- readFile "src/AdventOfCode/2023/Day13/Part2/sample.txt"
    -- content <- readFile "src/AdventOfCode/2023/Day13/Part2/testInput.txt"
    content <- readFile "src/AdventOfCode/2023/Day13/Part2/input.txt"
    let valleys :: [Valley]
        valleys = map (filter (not . null)) . groupBy (\_ b -> not . null $ b) . lines $ content


        originalHorizontalReflections = map getHorizontalReflectionLines valleys
        originalVerticalReflections = map getVerticalReflectionLines valleys


        fixedValleys = map fixValley valleys

        newHorizontalReflections = map getHorizontalReflectionLines fixedValleys
        newVerticalReflections = map getVerticalReflectionLines fixedValleys
        scores = getScore (concat originalVerticalReflections) (concat originalHorizontalReflections)

        -- the old reflection lines do not count. Must be filtered away!!!
        newHorizontalReflections' = zipWith ( \as bs ->[b | b<- bs, b `notElem` as] ) originalHorizontalReflections newHorizontalReflections
        newVerticalReflections' = zipWith ( \as bs ->[b | b<- bs, b `notElem` as] ) originalVerticalReflections newVerticalReflections
        newScores = getScore (concat newVerticalReflections') (concat newHorizontalReflections')



    print "Horizontal Orig"
    print originalHorizontalReflections
    print "Horizontal Positions"
    print . map (getMirrorPairsInValleyWithDiffEqualTo 1) $  valleys


    print "Horizontal New"
    print  newHorizontalReflections'

    print "Vertical Orig"
    print originalVerticalReflections
    print "Vertical New"
    print newVerticalReflections'
    print newScores
    -- print . map (getMirrorPairsInValleyWithDiffEqualTo 1) $  valleys

    -- print . map (getMirrorPairsInValleyWithDiffEqualTo 1 . transpose) $   valleys
    -- print . map (getMirrorPairsInValleyWithDiffEqualTo 1) $  valleys





type Mirror = String
type Valley = [Mirror]
type ReflectionLine = Int
type VerticalReflectionLine = ReflectionLine
type HorizontalReflectionLine = ReflectionLine

getScore :: [VerticalReflectionLine] -> [HorizontalReflectionLine] -> Int
getScore vs hs = sum $ vs ++ map (*100) hs


getVerticalReflectionLines :: Valley -> [Int]
getVerticalReflectionLines = getReflectionLines . transpose

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




-- Part 2: Fix Smudge

fixValley :: Valley  -> Valley
fixValley v =
    let origHLns =  getHorizontalReflectionLines v
        origVLns =  getVerticalReflectionLines v in
    case getMirrorPairsInValleyWithDiffEqualTo 1 v of
    [] -> case getMirrorPairsInValleyWithDiffEqualTo 1 . transpose $ v of
        [] -> error "No Mirror with diff of 1 smudge"
        ys -> fromJust $ fixValley' ys origVLns Vertical v
    xs -> case fixValley' xs origHLns Horizontal v of
        Just x -> x
        Nothing ->  case getMirrorPairsInValleyWithDiffEqualTo 1 . transpose $ v of
                [] -> error "No Mirror with diff of 1 smudge"
                ys -> fromJust $ fixValley' ys origVLns Vertical v


data FixValleyDirection = Horizontal
                          | Vertical deriving Show

isValidReflectionLines :: [ReflectionLine] -> [ReflectionLine] -> Bool
isValidReflectionLines oldLns = any (`notElem` oldLns)


-- For mirror in valley, it is one-based
fixValley' :: [((Int, Int), [Int])] -> [ReflectionLine] -> FixValleyDirection -> Valley -> Maybe Valley
fixValley' (x:xs) origRLns Horizontal v =
    let mirrorPos1 = fst . fst $ x
        mirrorPos2 = snd . fst $ x
        charPos = head . snd $ x
        newValleyPos1 = take (mirrorPos1 - 1) v ++ [fixMirror charPos (v!!(mirrorPos1 -1))] ++ drop mirrorPos1 v
        newValleyPos2 = take (mirrorPos2 - 1) v ++ [fixMirror charPos (v!!(mirrorPos2 -1))] ++ drop mirrorPos2 v

        newHorizontalReflectionLinesPos1 = getHorizontalReflectionLines newValleyPos1
        newHorizontalReflectionLinesPos2 = getHorizontalReflectionLines newValleyPos2

         in
    (if isValidReflectionLines origRLns newHorizontalReflectionLinesPos1
        then Just newValleyPos1
        else if  isValidReflectionLines origRLns newHorizontalReflectionLinesPos2
            then Just newValleyPos2
            else fixValley' xs origRLns Horizontal v
    )

fixValley' (x:xs) origRLns Vertical v =
    let mirrorPos1 = fst . fst $ x
        mirrorPos2 = snd . fst $ x
        charPos = head . snd $ x

        v' = transpose v
        newValleyPos1 = take (mirrorPos1 - 1) v' ++ [fixMirror charPos (v'!!(mirrorPos1 -1))] ++ drop mirrorPos1 v'
        newValleyPos2 = take (mirrorPos2 - 1) v' ++ [fixMirror charPos (v'!!(mirrorPos2 -1))] ++ drop mirrorPos2 v'


        newValleyPos1' = transpose newValleyPos1
        newValleyPos2' = transpose newValleyPos2

        newVerticalReflectionLinesPos1 = getVerticalReflectionLines newValleyPos1'
        newVerticalReflectionLinesPos2 = getVerticalReflectionLines newValleyPos2'
         in

    (if isValidReflectionLines origRLns newVerticalReflectionLinesPos1
        then Just $ transpose newValleyPos1
        else if  isValidReflectionLines origRLns  newVerticalReflectionLinesPos2
            then Just $ transpose newValleyPos2
            else fixValley' xs origRLns Vertical v
    )


fixValley' [] _ d v = Nothing


-- For char in mirror, it is zero-based
fixMirror :: Int -> Mirror -> Mirror
fixMirror n m = take n m ++ [fixSmudge (m!!n)]  ++ drop (n+1) m

fixSmudge :: Char -> Char
fixSmudge '.' = '#'
fixSmudge '#' = '.'
fixSmudge x = error $ "Cannot fix " ++ show x


getMirrorPairsInValleyWithDiffEqualTo :: Int -> Valley -> [((Int, Int), [Int])]
getMirrorPairsInValleyWithDiffEqualTo numDiff v = foldr (
        \(xs, (str1, str2)) acc -> let result = posDiff str1 str2 0 [] in
            if length result == numDiff
                then  (xs, result): acc
                else acc
    ) [] combosMap
    where valleyLength = length v
          combos = [(x, y) | x <- [1.. valleyLength], y <- [1..valleyLength], x < y]
          valleyMap = IMap.fromList $ zip [1..] v
          combosMap = zip combos (map (\(x, y) -> (fromJust $ IMap.lookup x valleyMap, fromJust $ IMap.lookup y valleyMap)) combos)

posDiff :: Mirror -> Mirror -> Int -> [Int] -> [Int]
posDiff [] _ _ acc = reverse acc
posDiff _ [] _ acc = reverse acc
posDiff m1 m2 pt acc = if head m1 == head m2
      then posDiff (tail m1) (tail m2) (pt +1) acc
       else posDiff (tail m1) (tail m2) (pt +1) (pt:acc)
