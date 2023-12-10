calculateNext :: [Int] -> Int
calculateNext xs
    | all (==0) xs = 0
    | otherwise = last xs + (calculateNext . calcDiff) xs

calcDiff :: [Int] -> [Int]
calcDiff xs = zipWith (-) (tail xs) (take (length xs - 1) xs)

parseString :: String -> [[Int]]
parseString xs = map ((map read) . words) (lines xs)

-- solution :: String -> Int
-- solution = (sum . map calculateNext . parseString)

-- part 2

calculatePrevious :: [Int] -> Int
calculatePrevious xs
    | all (==0) xs = 0
    | otherwise = (head xs) - (calculatePrevious (calcDiff xs))

solution :: String -> Int
solution = (sum . map calculatePrevious . parseString)

main :: IO ()
main = do
    xs <- readFile "puzzle_input.txt"
    putStrLn $ show $ solution xs

