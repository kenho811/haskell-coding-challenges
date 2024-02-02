module AdventOfCode.Day4.Part1.Solution where
import Text.ParserCombinators.Parsec
import Control.Monad (forM_)
import qualified Data.IntMap as M
import Data.Either (fromRight)
import Data.Functor.Classes (Show1(liftShowsPrec))


solution:: IO ()
solution = do
    content <- readFile "src/AdventOfCode/2023/Day4/Part1/input.txt"
    let eithers = map parseLine $ lines content
    forM_ eithers printCard
    let cards = getCards eithers
    let initialMap = initMap cards
    let finalMap = totalNumScratchCards cards initialMap
    print finalMap
    print $ M.foldl' (+) 0 finalMap

defaultCard :: Card
defaultCard = Card {
    cardId= -99999
    , winningNumbers=[-1]
    , existingNumbers=[-1]
    , numMatches=0
}


printCard :: Either ParseError Card -> IO()
printCard (Left err) = putStrLn $ "Parsing Failed: " ++ show err
printCard (Right card) = print card

getCards:: [Either ParseError Card] -> [Card]
getCards = map (fromRight defaultCard)



initMap :: [Card] -> M.IntMap Int
initMap cs = M.fromList [let cId = cardId c in (cId , 1)| c <- cs]


totalNumScratchCards :: [Card] -> M.IntMap Int -> M.IntMap Int
totalNumScratchCards [] m = m
totalNumScratchCards (x:xs) m =
     case M.lookup (cardId x) m of
        Nothing -> m
        Just num -> let newMap = addToMap m (cardId x + 1) (numMatches x) num in totalNumScratchCards xs newMap

addToMap :: M.IntMap Int -> Int -> Int -> Int -> M.IntMap Int
addToMap m curr cnt mul
    | cnt <= 0 = m
    | otherwise = let newMap = M.insertWith (+) curr mul m in addToMap newMap (curr + 1) (cnt -1) mul


data Card = Card {
    cardId:: Int
    ,winningNumbers:: [Int]
    ,existingNumbers:: [Int]
    ,numMatches:: Int
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
    let numMatches = length [x | x<- existingNumbers, y<-winningNumbers, x == y]
    return $ Card {
        cardId=cardId
        , winningNumbers=winningNumbers
        , existingNumbers=existingNumbers
        , numMatches=numMatches
    }

parseLine :: String -> Either ParseError Card
parseLine = parse cardParser "WRONG"


