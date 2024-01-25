module AdventOfCode.Day3.Part1.Solution where
import Data.Char (isDigit)
import Data.List (nub)


type Position = (Row, Column)
type Pair = (Integer, Position)
type Column = Integer
type Row = Integer

solution :: [(Integer, [Position])]
solution = digitsNum'
    where digitsNum = digitsToNumbers $ mergeDigits $ digitWithPosition 1 "2..1.hj211"
          digitsNum' = [(x, getMultiCorners y)| (x, y) <- digitsNum ]


symbolPositions :: Column -> String -> [Position]
symbolPositions col line = [(col,y)| (x,y) <- zip line [0..], not $ isDigit x, x /= '.']


-- includePosiiton 



-- get digits
digitWithPosition:: Column -> String -> [(String, [Position])]
digitWithPosition col line = [([x],[(y, col)])| (x,y) <- zip line [0..], isDigit x]

mergeDigits :: [(String, [Position])] -> [(String, [Position])]
mergeDigits [] = []
mergeDigits [(x, y)] = [(x,y)]
mergeDigits ((a, xs): (c,ys): ss)
    | rightRow == leftRow +1 = mergeDigits ((a ++ c,xs ++ ys) : ss)
    | otherwise = (a,xs): mergeDigits ((c,ys) : ss)
    where leftRow = fst $ last xs
          rightRow = fst $ head ys

digitsToNumbers :: [(String, [Position])] -> [(Integer, [Position])]
digitsToNumbers combos = [(read x,y) | (x, y) <- combos ]



-- get position corner
getSingleCorners:: Position -> [Position]
getSingleCorners (x, y) = [(x', y') |
                       x' <- [x - 1, x, x + 1], x' >= 0, y' <- [y - 1, y, y + 1], y' >= 0, (x',y') /= (x,y)]

getMultiCorners:: [Position] -> [Position]
getMultiCorners = nub . concatMap getSingleCorners