parseString :: String -> Int
parseString xs = length (filter (`elem` ws) cs)
    where
        (cardId, nums) = break (==':') xs
        (winning, card) = break (=='|') (drop 2 nums)
        ws :: [Int]
        ws = map read (words winning)
        cs :: [Int]
        cs = map read (words (tail card))

-- main :: IO ()
-- main = do
--     xs <- readFile "puzzle_input.txt"
--     let nums = map parseString (lines xs)
--     let numsFiltered = filter (/=0) nums
--     putStrLn (show (sum (map ((2^) . (+(-1))) numsFiltered)))


-- part 2
main :: IO ()
main = do
    xs <- readFile "puzzle_input.txt"
    let nums = map parseString (lines xs)
    let ans = sum $ countCopies nums (take (length nums) (repeat 1))
    putStrLn (show ans)


countCopies :: [Int] -> [Int] -> [Int]
countCopies [] [] = []
countCopies (x:xs) (y:ys) = y:(countCopies xs (map (+y) ((take x ys)) ++ (drop x ys)))


test :: String
test = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"