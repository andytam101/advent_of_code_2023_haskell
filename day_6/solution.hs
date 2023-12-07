parseString :: String -> [(Int, Int)]
parseString xs = zip ((parseLine . head) ls) ((parseLine . last) ls)
    where
        ls = lines xs

parseLine :: String -> [Int]
parseLine = (map read . tail . words)

countWays :: Int -> Int -> Int
countWays t d = length [x | x <- [1 .. (t - 1)], (calcDistance t x) > d]

calcDistance :: Int -> Int -> Int
calcDistance t x = (t - x) * x

-- solution :: String -> Int
-- solution = (product . map (uncurry (countWays)) . parseString)

-- part 2

parseStringPt2 :: String -> (Int, Int)
parseStringPt2 xs = (parseLinePt2 (head ls), parseLinePt2 (last ls))
    where
        ls = lines xs

parseLinePt2 :: String -> Int
parseLinePt2 = (read . concat . tail . words)

countWaysPt2 :: Int -> Int -> Int
countWaysPt2 t d = n - m
    where
        (m, n) = solveQuadratic (-1) t (-d)
 
solveQuadratic :: Int -> Int -> Int -> (Int, Int)
solveQuadratic a b c = (floor (((-b') + delta) / (2 * a')), floor (((-b') - delta) / (2 * a')))
    where
        [a', b', c'] = map (fromIntegral) [a, b, c]
        delta = sqrt (b' ^ 2 - 4 * a' * c')

solution :: String -> Int
solution = ((uncurry countWaysPt2) . parseStringPt2)

main :: IO ()
main = do
    xs <- readFile "puzzle_input.txt"
    putStrLn (show (solution xs))