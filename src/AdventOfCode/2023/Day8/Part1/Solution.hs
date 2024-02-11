module AdventOfCode.Day8.Part1.Solution where
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Char (isAlpha)
import qualified Data.Int as Map


solution :: IO()
solution = do
    content <- readFile "src/AdventOfCode/2023/Day8/Part1/input.txt"

    let lns = lines content
        directions = getDirections . head $ lns
        nodeMap = buildNodesMap . drop 2 $ lns
    print nodeMap
    print . getCountToZZZ nodeMap directions "AAA" $ 0 

type ParentNode = String
type LeftChildNode = String
type RightChildNode = String
type NodeMap =  Map.Map ParentNode (LeftChildNode, RightChildNode)


data Direction = DirectLeft
                | DirectRight deriving (Show)

getCountToZZZ :: NodeMap -> [Direction] -> ParentNode -> Int -> Int
getCountToZZZ mp (d:ds) parentNode acc 
    | parentNode == "ZZZ" = acc -- terminate
    | otherwise = case Map.lookup parentNode mp of
        Just (leftChild, rightChild) -> case d of
            DirectLeft -> getCountToZZZ mp ds leftChild (acc + 1)
            DirectRight -> getCountToZZZ mp ds rightChild (acc+ 1)
        Nothing -> error $ "ParentNode " ++ parentNode ++ "does NOT exist in Map"
getCountToZZZ _ [] _ _ = error "Empty Direction. Cannot go any further!!!"

buildNodesMap :: [String] ->  NodeMap
buildNodesMap = foldr (
        uncurry Map.insert . parseNodes
    ) Map.empty

parseDirection :: Char -> Direction
parseDirection 'L' = DirectLeft
parseDirection 'R' = DirectRight
parseDirection x = error $ "Unhandled direction " ++ [x]

getDirections :: String -> [Direction]
getDirections ln = fmap parseDirection (cycle ln)



parseNodes :: String -> (ParentNode, (LeftChildNode, RightChildNode))
parseNodes ln =
    let fullLs = T.splitOn (T.pack "=") (T.pack ln)
        parentNode = T.unpack . T.filter isAlpha . head $ fullLs
        rightLs = T.splitOn (T.pack ",") . last $ fullLs
        leftChildNode = T.unpack . T.filter isAlpha . head $ rightLs
        rightChildNode = T.unpack. T.filter isAlpha . last $ rightLs
        in
        (parentNode, (leftChildNode, rightChildNode))

