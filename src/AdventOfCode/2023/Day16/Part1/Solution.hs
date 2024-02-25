module AdventOfCode.Day16.Part1.Solution where

import qualified Data.Map as Map
import Data.List (nub)
import Data.Maybe (maybeToList)
import qualified Data.Set as Set
import Data.Set (notMember)

solution :: IO ()
solution = do
    let
        inputFilePath = "src/AdventOfCode/2023/Day16/Part1/sample.txt"
        -- inputFilePath = "src/AdventOfCode/2023/Day16/Part1/input.txt"
        -- outputFilePath =  "src/AdventOfCode/2023/Day16/Part1/sampleOutput.txt"
        outputFilePath =  "src/AdventOfCode/2023/Day16/Part1/inputOutput.txt"

    content <- readFile inputFilePath

    let grid:: Grid
        grid = Map.fromList [((rowNum, colNum), t) | (rowNum, ln) <- zip [1.. ] (lines content), (colNum, t) <- zip [1..] ln]

        -- start position
        currentBeamStates :: CurrentBeamStates
        currentBeamStates = [((1,0), East)] -- start from outside the grid from the West

        trail = Set.toList $ runBeams grid Set.empty currentBeamStates
        numOfEnergizedTiles = (+ (-1)) . length . nub . map fst $ trail  -- remove the initial out-of-grid Coordinate



    -- print grid
    print trail
    print numOfEnergizedTiles


type Coordinate = (Int, Int)
data Direction = East
                | South
                | West
                | North deriving (Show, Eq, Ord)
type BeamState = (Coordinate, Direction)
type Trail = Set.Set BeamState
type CurrentBeamStates = [BeamState]

type Tile = Char
type Grid = Map.Map Coordinate Tile

-- | stop condition : All Current Beams
runBeams :: Grid -> Trail -> CurrentBeamStates -> Trail
runBeams _ _ [] = Set.empty
runBeams grid accTrail bss =
    let nextCurrentBeamStates = concat [filter (`Set.notMember` accTrail) $ advanceBeamState grid bs | bs <- bss]
        nextTrail = Set.union accTrail (Set.fromList bss) in
   Set.union nextTrail (runBeams grid nextTrail nextCurrentBeamStates)


calculateNumOfEnergizedTiles :: Trail -> Int
calculateNumOfEnergizedTiles = length . nub . map fst . Set.toList


nextCoordinate :: Direction -> Coordinate -> Coordinate
nextCoordinate North (x,y) = (x-1, y)
nextCoordinate East (x,y) = (x, y+1)
nextCoordinate South (x,y) = (x+1, y)
nextCoordinate West (x,y) = (x, y-1)

nextTile :: Grid -> BeamState -> [Tile]
nextTile grid (c, dir)= maybeToList $ Map.lookup (nextCoordinate dir c) grid


-- | both move and change direction of BeamState
advanceBeamState :: Grid -> BeamState -> [BeamState]
advanceBeamState grid bs = do
    tile <- nextTile grid bs
    bs' <- _moveBeamCoordinate grid bs
    _updateBeamDirection tile bs'


_moveBeamCoordinate :: Grid -> BeamState -> [BeamState]
_moveBeamCoordinate grid (c, dir )=
     let c' = nextCoordinate dir c in
        case Map.lookup c' grid of
        Nothing -> []
        Just _ -> [(c', dir)]

_updateBeamDirection :: Tile -> BeamState -> [BeamState]
_updateBeamDirection '.' x = [x]
_updateBeamDirection '|' x@(c, dir)
    | dir `elem` [North , South] = [x]
    | dir `elem` [East,West] = [(c, North), (c, South)]
_updateBeamDirection '-' x@(c, dir)
    | dir `elem` [North , South] = [(c, East), (c, West)]
    | dir `elem` [East,West] = [x]
_updateBeamDirection '/' (c, dir) = case dir of
        North -> [(c, East)]
        East -> [(c, North)]
        South -> [(c, West)]
        West -> [(c, South)]
_updateBeamDirection '\\' (c, dir) = case dir of
        North -> [(c, West)]
        East -> [(c, South)]
        South -> [(c, East)]
        West -> [(c, North)]
_updateBeamDirection t _ = error $ "unexpected tile " ++ show t



