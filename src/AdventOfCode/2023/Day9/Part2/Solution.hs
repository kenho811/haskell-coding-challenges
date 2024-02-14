module AdventOfCode.Day9.Part2.Solution where

solution :: IO()
solution = do
    content <- readFile "src/AdventOfCode/2023/Day9/Part2/input.txt"
    let seqs :: [[Int]]
        seqs = map (map read . words) . lines $ content
        newSeqs = map addOnePredictionBackwards seqs
    print seqs

    print . sum . map head $ newSeqs


addOnePredictionBackwards :: [Int] -> [Int] 
addOnePredictionBackwards xs = newFirstElement : xs
    where allSeqs = reverse $ xs : getAllSeqs xs
          newFirstElement =  foldl (\ acc ys -> head ys - acc) 0 allSeqs


getAllSeqs :: [Int] -> [[Int]]
getAllSeqs xs = if not . all (==0) $ newSeq then newSeq : getAllSeqs newSeq else [newSeq]
    where newSeq = getNewSeq xs


getNewSeq :: [Int] -> [Int]
getNewSeq [x, y] = [y - x]
getNewSeq (x:y:zs) = (y -x): getNewSeq (y:zs)
getNewSeq _ = error "Sequence must have at 2 elements!"
