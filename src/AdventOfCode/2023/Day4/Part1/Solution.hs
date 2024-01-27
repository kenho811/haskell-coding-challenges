module AdventOfCode.Day4.Part1.Solution where
import Text.ParserCombinators.Parsec
import Control.Monad (forM_)


solution:: IO ()
solution = do
    content <- readFile "src/AdventOfCode/2023/Day4/Part1/input.txt"
    let eithers = map parseLine $ lines content
    forM_ eithers printCard
    print $ sum $ map calcEitherCardPoints eithers

printCard :: Either ParseError Card -> IO()
printCard (Left err) = putStrLn $ "Parsing Failed: " ++ show err
printCard (Right card) = print card

calcEitherCardPoints :: Either ParseError Card -> Int
calcEitherCardPoints (Left _) = 0
calcEitherCardPoints (Right card) = calcCardPoints card


calcCardPoints:: Card -> Int
calcCardPoints (Card _ xs ys) =
    case length [x | x <- xs, y <- ys, x==y] of
        0 -> 0
        x -> 2^(x-1)


data Card = Card {
    cardId:: Int
    ,winningNumbers:: [Int]
    ,existingNumbers:: [Int]
} deriving Show


numberParser:: Parser Int
numberParser = read <$> many1 digit

numberListParser :: Parser [Int]
numberListParser = sepBy1 numberParser spaces



cardParser :: Parser Card
cardParser = do
    _ <- string "Card" >> spaces
    cardId <- numberParser
    _ <- string ":" >> spaces
    winningNumbers <- numberListParser
    _ <- string "|" >> spaces
    existingNumbers <- numberListParser
    return $ Card {
        cardId=cardId
        , winningNumbers=winningNumbers
        , existingNumbers=existingNumbers
    }

parseLine :: String -> Either ParseError Card
parseLine = parse cardParser "WRONG"


