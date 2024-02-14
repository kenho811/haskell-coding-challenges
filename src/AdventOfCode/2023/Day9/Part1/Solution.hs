module AdventOfCode.Day9.Part1.Solution where

solution :: IO()
solution = do
    content <- readFile "src/AdventOfCode/2023/Day9/Part1/input.txt"
    let seqs :: [[Int]]
        seqs = map (map read . words) . lines $ content
        newSeqs = map addOnePrediction seqs
    print seqs

    print . sum . map last $ newSeqs


addOnePrediction :: [Int] -> [Int] -- Int -> [[Int]] -> Int
addOnePrediction xs = xs ++ [newLastElement]
    where allSeqs = reverse $ xs : getAllSeqs xs
          newLastElement =  foldl (\ acc ys -> last ys + acc) 0 allSeqs


getAllSeqs :: [Int] -> [[Int]]
getAllSeqs xs = if not . all (==0) $ newSeq then newSeq : getAllSeqs newSeq else [newSeq]
    where newSeq = getNewSeq xs


getNewSeq :: [Int] -> [Int]
getNewSeq [x, y] = [y - x]
getNewSeq (x:y:zs) = (y -x): getNewSeq (y:zs)
getNewSeq _ = error "Sequence must have at 2 elements!"
