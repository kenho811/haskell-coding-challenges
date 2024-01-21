{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module AdventOfCode.Day2.Part1.Solution where

import Text.ParserCombinators.Parsec

import Data.Either

solution :: IO ()
solution = do
    content <- readFile "src/AdventOfCode/2023/Day2/Part1/input.csv"
    let allGames = rights $ map (parse gameParser "error") (lines content)
    print $ sum $ map gamePower allGames



data Game = Game {
    gameId:: Integer
    ,sets :: [Set]
 } deriving Show

newtype Set = Set [Cube] deriving Show

data Cube = Red Integer
           | Blue Integer
           | Green Integer
           deriving Show


-- examples:
-- Game 1: 1 red, 3 blue, 11 green; 1 blue, 5 red; 3 blue, 5 green, 13 red; 6 red, 1 blue, 4 green; 16 red, 12 green
gameParser :: Parser Game
gameParser =  do
    _ <- string "Game "
    ds <- many1 digit
    _ <- string ": "
    sS <- sepBy1 setParser (string "; ")
    return $ Game {
        gameId = read ds
        , sets= sS
    }

-- Examples
-- red 1, green 2
-- blue 2
-- blue 2, red 3, green 10
setParser :: Parser Set
setParser = do
    st <- sepBy1 cubeParser (string ", ")
    return $ Set st

-- Examples
-- red 1 
-- green 2 
-- blue 3
cubeParser :: Parser Cube
cubeParser = do
    ds <- many1 digit
    spaces
    colour <- many1 letter
    return $ case colour of
        "red" -> Red (read ds)
        "blue" -> Blue (read ds)
        "green" -> Green (read ds)
        _ -> error "a Cube must be one of red, blue or green!!!"

gamePower :: Game -> Integer
gamePower game = case maxPerGame game of
     Set [Red r, Blue b, Green g] -> r * b * g
     _ -> undefined


maxPerGame:: Game -> Set
maxPerGame game = foldr maxPerSet (Set[Red 0, Blue 0, Green 0]) (sets game)

maxPerSet:: Set -> Set -> Set
maxPerSet (Set []) acc = acc
maxPerSet (Set (x:xs)) acc@(Set[Red r, Blue b, Green g]) =
    case x of
        Red y -> if y > r then maxPerSet (Set xs) (Set[Red y, Blue b, Green g]) else  maxPerSet (Set xs) acc
        Blue y -> if y > b then maxPerSet (Set xs) (Set [Red r, Blue y, Green g]) else  maxPerSet (Set xs) acc
        Green y -> if y > g then maxPerSet (Set xs) (Set [Red r, Blue b, Green y]) else  maxPerSet (Set xs) acc