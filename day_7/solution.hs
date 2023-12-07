import Data.List (nub, sort, group, maximumBy, sortBy)
import Data.Char (ord)
newtype Card = Card [Int] deriving (Eq, Show)

instance Ord Card where
    compare c1 c2
        | isFiveOfAKind c1 && not (isFiveOfAKind c2) = GT
        | isFiveOfAKind c2 && not (isFiveOfAKind c1) = LT
        | isFourOfAKind c1 && not (isFourOfAKind c2) = GT
        | isFourOfAKind c2 && not (isFourOfAKind c1) = LT
        | isFullHouse c1 && not (isFullHouse c2) = GT
        | isFullHouse c2 && not (isFullHouse c1) = LT
        | isTriplets c1 && not (isTriplets c2) = GT
        | isTriplets c2 && not (isTriplets c1) = LT
        | isTwoPair c1 && not (isTwoPair c2) = GT
        | isTwoPair c2 && not (isTwoPair c1) = LT
        | isOnePair c1 && not (isOnePair c2) = GT
        | isOnePair c2 && not (isOnePair c1) = LT
        | otherwise = compareCards c1 c2

-- compareCards :: Card -> Card -> Ordering
-- compareCards (Card []) (Card []) = EQ
-- compareCards (Card (x:xs)) (Card (y:ys))
--     | x == y = compareCards (Card xs) (Card ys)
--     | otherwise = compare x y

isFiveOfAKind :: Card -> Bool
isFiveOfAKind (Card x) = (length (nub x) == 1)

isFourOfAKind :: Card -> Bool
isFourOfAKind (Card x) = c1 == c4 || c2 == c5
    where
        [c1,c2,c3,c4,c5] = sort x

isFullHouse :: Card -> Bool
isFullHouse (Card x) = c1 == c3 && c4 == c5 || c1 == c2 && c3 == c5
    where
        [c1, c2, c3, c4, c5] = sort x

isTriplets :: Card -> Bool
isTriplets c@(Card x) = (c1 == c3 || c2 == c4 || c3 == c5) && (not (isFullHouse c))
    where
        [c1, c2, c3, c4, c5] = sort x

isTwoPair :: Card -> Bool
isTwoPair c@(Card x) = ((c1 == c2 && (c3 == c4 || c4 == c5)) || (c2 == c3 && c4 == c5)) && (not (isFullHouse c))
    where
        [c1, c2, c3, c4, c5] = sort x

isOnePair :: Card -> Bool
isOnePair c@(Card x) = length (nub x) == 4

convertChar :: Char -> Int
convertChar 'A' = 14
convertChar 'T' = 10
convertChar 'J' = 11
convertChar 'Q' = 12
convertChar 'K' = 13
convertChar x = ord x - ord '0'

parseString :: String -> [(Card, Int)]
parseString xs = map parseLine (lines xs)

parseLine :: String -> (Card, Int)
parseLine xs = (Card (map convertChar (head ws)), read (last ws))
    where
        ws = words xs

-- solution :: String -> Int
-- solution xs = sum $ zipWith (*) (map snd $ sort (parseString xs)) [1..]

-- part 2

data NewCard = NewCard Card Card deriving Show

instance Eq NewCard where
    (NewCard c1 c2) == (NewCard c3 c4) = c1 == c3 && c2 == c4

instance Ord NewCard where
    compare (NewCard c1 c2) (NewCard c3 c4)
        | isFiveOfAKind c1 && not (isFiveOfAKind c3) = GT
        | isFiveOfAKind c3 && not (isFiveOfAKind c1) = LT
        | isFourOfAKind c1 && not (isFourOfAKind c3) = GT
        | isFourOfAKind c3 && not (isFourOfAKind c1) = LT
        | isFullHouse c1 && not (isFullHouse c3) = GT
        | isFullHouse c3 && not (isFullHouse c1) = LT
        | isTriplets c1 && not (isTriplets c3) = GT
        | isTriplets c3 && not (isTriplets c1) = LT
        | isTwoPair c1 && not (isTwoPair c3) = GT
        | isTwoPair c3 && not (isTwoPair c1) = LT
        | isOnePair c1 && not (isOnePair c3) = GT
        | isOnePair c3 && not (isOnePair c1) = LT
        | otherwise = compareCards c2 c4

compareCards :: Card -> Card -> Ordering
compareCards (Card []) (Card []) = EQ
compareCards (Card (x:xs)) (Card (y:ys))
    | x == y = compareCards (Card xs) (Card ys)
    | x == 11 = LT
    | y == 11 = GT
    | otherwise = compare x y

countJokers :: Card -> Int
countJokers (Card []) = 0
countJokers (Card (11:xs)) = 1 + countJokers (Card xs)
countJokers (Card (x:xs)) = countJokers (Card xs)

checkRepeats :: [Int] -> Int -> Bool
checkRepeats xs n = any (\x -> length x == n) grouped
    where
        grouped = group $ sort xs

generateNewCard :: Card -> NewCard
generateNewCard c@(Card [11,11,11,11,11]) = NewCard (Card [14,14,14,14,14]) c
generateNewCard c@(Card xs) = NewCard (Card (filtered ++ (take (jCount) (repeat (mostCommon filtered))))) c
    where
        jCount = countJokers c
        filtered = filter (/=11) xs

mostCommon :: Ord a => [a] -> a
mostCommon = head . maximumBy (\x y -> compare (length x) (length y)) . group . sort

sortCards :: [(Card, Int)] -> [(Card, Int)]
sortCards = sortBy (\(c1, x) -> \(c2, y) -> compare (generateNewCard c1) (generateNewCard c2))

solution :: String -> Int
solution xs = sum $ zipWith (*) (map snd (sortCards (parseString xs)))  [1..]

main :: IO ()
main = do
    x <- readFile "puzzle_input.txt"
    putStrLn $ show $ solution x

testString :: String
testString = "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483"