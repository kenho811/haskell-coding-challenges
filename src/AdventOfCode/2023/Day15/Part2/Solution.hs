{-# Language OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module AdventOfCode.Day15.Part2.Solution where

import qualified Data.Text as T
import qualified Data.IntMap as IMap
import Data.Char (ord)
import Data.Maybe (listToMaybe)





solution :: IO ()
solution = do
    let
        -- inputFilePath = "src/AdventOfCode/2023/Day15/Part2/sample.txt"
        inputFilePath = "src/AdventOfCode/2023/Day15/Part2/input.txt"
        -- outputFilePath =  "src/AdventOfCode/2023/Day15/Part2/sampleOutput.txt"
        outputFilePath =  "src/AdventOfCode/2023/Day15/Part2/inputOutput.txt"

    content <- readFile inputFilePath

    let strs = map T.unpack $ T.splitOn (T.pack ",") (T.pack content)
        lenses = map parseLens strs

        boxMap :: BoxMap
        boxMap = IMap.fromList $ map (, []) [0..255]

        updatedBoxMap = populateMap boxMap lenses
        focusingPower = calculateFocusingPower updatedBoxMap


    print  lenses
    -- print  boxMap
    print  updatedBoxMap
    print  focusingPower

data Lens = AddLens { letters:: String , focalLength :: Int }
           |MinusLens {letters:: String} deriving Show

type BoxMap = IMap.IntMap [Lens]

testBoxMap :: IMap.IntMap [Lens]
testBoxMap = IMap.fromList [(0,[AddLens {letters = "rn", focalLength = 1}]),(1,[AddLens {letters = "qp", focalLength = 3}]),(2,[]),(3,[]),(4,[]),(5,[]),(6,[]),(7,[]),(8,[]),(9,[]),(10,[]),(11,[]),(12,[]),(13,[]),(14,[]),(15,[]),(16,[]),(17,[]),(18,[]),(19,[]),(20,[]),(21,[]),(22,[]),(23,[]),(24,[]),(25,[]),(26,[]),(27,[]),(28,[]),(29,[]),(30,[]),(31,[]),(32,[]),(33,[]),(34,[]),(35,[]),(36,[]),(37,[]),(38,[]),(39,[]),(40,[]),(41,[]),(42,[]),(43,[]),(44,[]),(45,[]),(46,[]),(47,[]),(48,[]),(49,[]),(50,[]),(51,[]),(52,[]),(53,[]),(54,[]),(55,[]),(56,[]),(57,[]),(58,[]),(59,[]),(60,[]),(61,[]),(62,[]),(63,[]),(64,[]),(65,[]),(66,[]),(67,[]),(68,[]),(69,[]),(70,[]),(71,[]),(72,[]),(73,[]),(74,[]),(75,[]),(76,[]),(77,[]),(78,[]),(79,[]),(80,[]),(81,[]),(82,[]),(83,[]),(84,[]),(85,[]),(86,[]),(87,[]),(88,[]),(89,[]),(90,[]),(91,[]),(92,[]),(93,[]),(94,[]),(95,[]),(96,[]),(97,[]),(98,[]),(99,[]),(100,[]),(101,[]),(102,[]),(103,[]),(104,[]),(105,[]),(106,[]),(107,[]),(108,[]),(109,[]),(110,[]),(111,[]),(112,[]),(113,[]),(114,[]),(115,[]),(116,[]),(117,[]),(118,[]),(119,[]),(120,[]),(121,[]),(122,[]),(123,[]),(124,[]),(125,[]),(126,[]),(127,[]),(128,[]),(129,[]),(130,[]),(131,[]),(132,[]),(133,[]),(134,[]),(135,[]),(136,[]),(137,[]),(138,[]),(139,[]),(140,[]),(141,[]),(142,[]),(143,[]),(144,[]),(145,[]),(146,[]),(147,[]),(148,[]),(149,[]),(150,[]),(151,[]),(152,[]),(153,[]),(154,[]),(155,[]),(156,[]),(157,[]),(158,[]),(159,[]),(160,[]),(161,[]),(162,[]),(163,[]),(164,[]),(165,[]),(166,[]),(167,[]),(168,[]),(169,[]),(170,[]),(171,[]),(172,[]),(173,[]),(174,[]),(175,[]),(176,[]),(177,[]),(178,[]),(179,[]),(180,[]),(181,[]),(182,[]),(183,[]),(184,[]),(185,[]),(186,[]),(187,[]),(188,[]),(189,[]),(190,[]),(191,[]),(192,[]),(193,[]),(194,[]),(195,[]),(196,[]),(197,[]),(198,[]),(199,[]),(200,[]),(201,[]),(202,[]),(203,[]),(204,[]),(205,[]),(206,[]),(207,[]),(208,[]),(209,[]),(210,[]),(211,[]),(212,[]),(213,[]),(214,[]),(215,[]),(216,[]),(217,[]),(218,[]),(219,[]),(220,[]),(221,[]),(222,[]),(223,[]),(224,[]),(225,[]),(226,[]),(227,[]),(228,[]),(229,[]),(230,[]),(231,[]),(232,[]),(233,[]),(234,[]),(235,[]),(236,[]),(237,[]),(238,[]),(239,[]),(240,[]),(241,[]),(242,[]),(243,[]),(244,[]),(245,[]),(246,[]),(247,[]),(248,[]),(249,[]),(250,[]),(251,[]),(252,[]),(253,[]),(254,[]),(255,[])]

testLens = [AddLens {letters = "rn", focalLength = 100}]


calculateFocusingPower :: BoxMap -> Int
calculateFocusingPower = sum . map (\(boxNum, ls) -> (boxNum + 1) * sum (zipWith (*) [1..] (map focalLength ls)) ) . IMap.toList

populateMap :: BoxMap -> [Lens] -> BoxMap
populateMap = foldl f where f acc lens = IMap.insert (runString . letters $ lens) (updateList lens (findList acc lens)) acc

findList :: BoxMap -> Lens -> [Lens]
findList mp lens = let ltrs = letters lens
                       targetBox =  runString ltrs in
                   case IMap.lookup targetBox mp of
                    Nothing -> error $ "targetBox " ++ show targetBox ++ "for letters " ++ show ltrs ++ " NOT FOUND!"
                    Just xs -> xs


updateList :: Lens -> [Lens] -> [Lens]
updateList lens@(AddLens _ _) [] = [lens]
updateList lens@(AddLens ltrs _) xs = case listToMaybe $ searchLensByLetters ltrs xs of
    Nothing -> xs ++ [lens]
    Just idx -> take idx xs ++ [lens] ++ drop (idx + 1) xs
updateList (MinusLens _ ) [] = []
updateList (MinusLens lts) xs = case listToMaybe $ searchLensByLetters lts xs of
    Nothing -> xs
    Just idx -> take idx xs ++ drop (idx + 1) xs


searchLensByLetters :: String -> [Lens] -> [Int]
searchLensByLetters s lens = [idx | (idx, ltrs) <- zip [0..] (map letters lens), ltrs ==s ]

parseLens :: String -> Lens
parseLens s = if '=' `elem` s
    then AddLens {letters=T.unpack . head $ T.splitOn (T.pack "=") txt
               ,focalLength=read . T.unpack . last $ T.splitOn (T.pack "=") txt }
    else MinusLens {letters= T.unpack . head $ T.splitOn (T.pack "-") txt}
    where txt = T.pack s



runChar :: Char -> Int -> Int
runChar c acc = ((ord c + acc)*17) `mod` 256

runString :: String -> Int
runString = foldl (flip runChar) 0