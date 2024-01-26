module AdventOfCode.Day3.Part1.Solution where
import Data.Char (isDigit)
import Data.List (nub)
import System.IO (openFile)


type Position = (Row, Column)
type Pair = (Integer, Position)
type Column = Integer
type Row = Integer


solution :: IO ()
solution = do
    content <- readFile "src/AdventOfCode/2023/Day3/Part1/input.txt"
    let ls = lines content
    let symbolPos = symbolPositions ls
    let digitPos = digitPositions ls
    let digitCorners = [(x, getMultiCorners y)| (x, y) <- digitPos ]
    let filtered = [x | (x, ys) <- digitCorners, any (`elem` symbolPos) ys]
    print $ sum filtered


-- get symbols
symbolPositions :: [String] -> [Position]
symbolPositions = symbolPositionsFromStrings 0

symbolPositionsFromStrings :: Row -> [String] -> [Position]
symbolPositionsFromStrings _ [] = []
symbolPositionsFromStrings n (x:xs)= symbolPositionsFromString n x ++ symbolPositionsFromStrings (n+1) xs

symbolPositionsFromString :: Row -> String -> [Position]
symbolPositionsFromString row line = [(row,col)| (char,col) <- zip line [0..], not $ isDigit char, char /= '.']




-- includePosiiton 



-- get digits
digitPositions :: [String] -> [(Integer, [Position])]
digitPositions =  digitPositionsFromStrings 0 

digitPositionsFromStrings :: Row -> [String] -> [(Integer, [Position])]
digitPositionsFromStrings _ [] = []
digitPositionsFromStrings n (x:xs)= digitsToNumbers (mergeDigits $ digitWithPosition n x) ++ digitPositionsFromStrings (n+1) xs


digitWithPosition:: Row -> String -> [(String, [Position])]
digitWithPosition row line = [([char],[(row, col)])| (char,col) <- zip line [0..], isDigit char]

mergeDigits :: [(String, [Position])] -> [(String, [Position])]
mergeDigits [] = []
mergeDigits [(x, y)] = [(x,y)]
mergeDigits ((a, xs): (c,ys): ss)
    | rightColumn == leftColumn +1 = mergeDigits ((a ++ c,xs ++ ys) : ss)
    | otherwise = (a,xs): mergeDigits ((c,ys) : ss)
    where leftColumn = snd $ last xs
          rightColumn = snd $ head ys

digitsToNumbers :: [(String, [Position])] -> [(Integer, [Position])]
digitsToNumbers combos = [(read x,y) | (x, y) <- combos ]



-- get position corner
getSingleCorners:: Position -> [Position]
getSingleCorners (x, y) = [(x', y') |
                       x' <- [x - 1, x, x + 1], x' >= 0, y' <- [y - 1, y, y + 1], y' >= 0, (x',y') /= (x,y)]

getMultiCorners:: [Position] -> [Position]
getMultiCorners = nub . concatMap getSingleCorners