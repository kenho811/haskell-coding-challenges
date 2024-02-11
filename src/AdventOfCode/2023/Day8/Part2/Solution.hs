{-# LANGUAGE InstanceSigs #-}
module AdventOfCode.Day7.Part1.Solution where
import qualified Data.Map as Map
import GHC.Exts (sortWith)


solution:: IO ()
solution = do
    -- content <- readFile "src/AdventOfCode/2023/Day7/Part1/sample.txt"
    content <- readFile "src/AdventOfCode/2023/Day7/Part1/input.txt"
    let handAndBids :: [(Hand, Bid )]
        handAndBids = map convertType . lines $ content
            where convertType :: String -> (Hand, Bid)
                  convertType l = (parseHand . head . words $ l , read (words l!!1))
        sortedHandAndBids = sortWith fst handAndBids

        -- debug
        convertedBackHandAndBids:: [(String, String, String)]
        convertedBackHandAndBids = zipThree (map (unparseHand . fst) sortedHandAndBids) (map (show . snd) sortedHandAndBids) (map (show . handType. fst) sortedHandAndBids)

        convertedBackHandAndBidsString :: String
        convertedBackHandAndBidsString = unlines $ concatMap (\(a, b, c) -> [a ++ " " ++ b ++ " " ++ c]) convertedBackHandAndBids


    writeFile "src/AdventOfCode/2023/Day7/Part2/output.txt" convertedBackHandAndBidsString

    print sortedHandAndBids
    -- print $  zip (map snd sortedHandAndBids) [1..]

    -- print . length  $ zip (map snd sortedHandAndBids) [1..]
    print . sum $ zipWith (*) (map snd sortedHandAndBids) [1..]

zipThree :: [a] -> [b] -> [c] ->[(a,b,c)]
zipThree (x:xs) (y:ys) (z:zs) = (x,y,z): zipThree xs ys zs
zipThree _ [] _ = []
zipThree [] _ _ = []
zipThree _ _ [] = []


type Bid = Int

data Hand = Hand {
    first:: Card
    , second:: Card
    , third:: Card
    , fourth:: Card
    , fifth:: Card
    , handType :: HandType
} deriving (Show, Eq)

instance Ord Hand where
    compare :: Hand -> Hand -> Ordering
    compare (Hand a1 b1 c1 d1 e1 ht1) (Hand a2 b2 c2 d2 e2 ht2) =
        compare ht1 ht2 <> compare a1 a2 <> compare b1 b2 <> compare c1 c2 <> compare d1 d2 <> compare e1 e2



data HandType = HighCard
                | OnePair
                | TwoPair
                | ThreeOfAKind
                | FullHouse
                | FourOfAKind
                | FiveOfAKind deriving (Show, Eq, Ord)


data Card =  LetterJ
           | NumberTwo
           | NumberThree
           | NumberFour
           | NumberFive
           | NumberSix
           | NumberSeven
           | NumberEight
           | NumberNine
           |  LetterT
           |  LetterQ
           |  LetterK
           |  LetterA
           deriving (Show, Eq, Ord)


parseHand:: [Char] -> Hand
parseHand [a, b, c, d, e] =
    let cardA = parseCard a
        cardB = parseCard b
        cardC = parseCard c
        cardD = parseCard d
        cardE = parseCard e  in
     Hand cardA cardB cardC cardD cardE (getHandType [cardA, cardB, cardC, cardD, cardE])
parseHand _ = error "A Hand must have 5 cards!!!"

unparseHand :: Hand -> [Char]
unparseHand (Hand c1 c2 c3 c4 c5 _) = [unparseCard c1, unparseCard c2, unparseCard c3, unparseCard c4, unparseCard c5]


parseCard :: Char -> Card
parseCard '2' = NumberTwo
parseCard '3' = NumberThree
parseCard '4' = NumberFour
parseCard '5' = NumberFive
parseCard '6' = NumberSix
parseCard '7' = NumberSeven
parseCard '8' = NumberEight
parseCard '9' = NumberNine
parseCard 'A' = LetterA
parseCard 'J' = LetterJ
parseCard 'K' = LetterK
parseCard 'Q' = LetterQ
parseCard 'T' = LetterT
parseCard _ = error "Unhandled Card!!"


unparseCard :: Card -> Char
unparseCard NumberTwo = '2'
unparseCard NumberThree = '3'
unparseCard NumberFour = '4'
unparseCard NumberFive ='5'
unparseCard NumberSix = '6'
unparseCard NumberSeven = '7'
unparseCard NumberEight = '8'
unparseCard NumberNine = '9'
unparseCard LetterT = 'T'
unparseCard LetterQ = 'Q'
unparseCard LetterK = 'K'
unparseCard LetterJ = 'J'
unparseCard LetterA = 'A'


getCardFrequency :: [Card] -> Map.Map Card Int
getCardFrequency = foldr func Map.empty
    where func card = Map.insertWith (+) card 1

optimizeCardFrequency :: Map.Map Card Int -> Map.Map Card Int
optimizeCardFrequency mp = case Map.lookup LetterJ mp of
    Just x -> case x of 
        5 -> mp -- don't convert when all 5 cards are J
        _ -> Map.insertWith (+) cardWithLargestFrequency x mapWithoutJ
    Nothing -> mp
    where mapWithoutJ = Map.fromList . filter (\(x, _) -> x /= LetterJ) . Map.toList $ mp
          cardWithLargestFrequency = fst . last . sortWith snd $ Map.toList mapWithoutJ


getFrequencyFrequency :: Map.Map a Int -> Map.Map Int Int
getFrequencyFrequency m = foldr (\num mp -> Map.insertWith (+) num 1 mp) Map.empty freq
    where tupList = Map.toList m
          freq = map snd tupList

getHandType:: [Card] -> HandType
getHandType cs
    | hasFive = FiveOfAKind
    | hasFour = FourOfAKind
    | hasThreeAndHasTwo = FullHouse
    | hasThree = ThreeOfAKind
    | hasTwo = TwoPair
    | hasOne = OnePair
    | allDifferent = HighCard
    | otherwise = error ("Unhandled combo! " ++ show cs ++ show freqFreqMap)
    where freqFreqMap = getFrequencyFrequency . optimizeCardFrequency . getCardFrequency $ cs
          hasFive = Map.lookup 5 freqFreqMap == Just 1
          hasFour = Map.lookup 4 freqFreqMap == Just 1 && Map.lookup 1 freqFreqMap == Just 1
          hasThreeAndHasTwo = Map.lookup 3 freqFreqMap == Just 1 && Map.lookup 2 freqFreqMap == Just 1
          hasThree = Map.lookup 3 freqFreqMap == Just 1 && Map.lookup 1 freqFreqMap == Just 2
          hasTwo = Map.lookup 2 freqFreqMap == Just 2 && Map.lookup 1 freqFreqMap == Just 1
          hasOne = Map.lookup 2 freqFreqMap == Just 1 && Map.lookup 1 freqFreqMap == Just 3
          allDifferent = Map.lookup 1 freqFreqMap == Just 5

