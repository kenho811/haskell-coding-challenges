module AdventOfCode.Day1.Part1.Solution where
import Data.Char (isDigit)


solution :: IO ()
solution =  do
    content <- readFile "src/AdventOfCode/2023/Day1/Part1/input.csv"
    let ls = lines content
    print $ getSum ls



getSum = sum . map getNumber

getNumber :: String -> Integer
getNumber line =
    read (fSt:[lSt])
    where l =  filter isDigit line
          fSt = head l
          lSt = l !! (length l - 1)



