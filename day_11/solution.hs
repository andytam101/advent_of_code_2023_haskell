import Data.List (transpose, findIndex)    

parseString :: String -> [[Int]]
parseString xs = map (map transform) (lines xs)

transform :: Char -> Int 
transform '.' = 0
transform '#' = 1

expand :: [[Int]] -> [[Int]]
expand = expandCol . expandRow

expandRow :: [[Int]] -> [[Int]]
expandRow [] = []
expandRow (xs:xss)
    | 1 `elem` xs = xs:(expandRow xss)
    | otherwise = xs:xs:(expandRow xss)

expandCol :: [[Int]] -> [[Int]]
expandCol = transpose . expandRow . transpose

findGalaxies :: [[Int]] -> [(Int, Int)]
findGalaxies xs = concatMap (\(x, ys) -> map (\k -> (x, k)) ys) (zip [0..] (map (`findGalaxiesRow` 0) xs))

findGalaxiesRow :: [Int] -> Int -> [Int]
findGalaxiesRow [] _ = []
findGalaxiesRow (1:xs) n = n:(findGalaxiesRow xs (n + 1))
findGalaxiesRow (0:xs) n = findGalaxiesRow xs (n + 1)

calcDistance :: (Int, Int) -> (Int, Int) -> Int
calcDistance (x, y) (m, n) = abs (x - m) + abs (y - n)

calcAllDistance :: [(Int, Int)] -> Int
calcAllDistance [] = 0
calcAllDistance (x:xs) = (sum . map (calcDistance x)) xs + (calcAllDistance xs)


-- solution :: String -> Int
-- solution = calcAllDistance . findGalaxies . expand . parseString


-- part 2

getEmptyRow :: [[Int]] -> [Int]
getEmptyRow xs = filter (\x -> not (1 `elem` (xs!!x)) ) [0..(length xs - 1)]

getEmptyCol :: [[Int]] -> [Int]
getEmptyCol = getEmptyRow . transpose

calcDistancePt2 :: (Int, Int) -> (Int, Int) -> [Int] -> [Int] -> Int
calcDistancePt2 xy@(x, y) mn@(m, n) rows cols = calcDistance xy mn + 999999 * (countEmpty rows x m + countEmpty cols y n)
    where
        countEmpty :: [Int] -> Int -> Int -> Int
        countEmpty nums x y = length $ filter (\k -> k > min x y && k < max x y) nums

calcAllDistancePt2 :: [(Int, Int)] -> [Int] -> [Int] -> Int
calcAllDistancePt2 [] _ _ = 0
calcAllDistancePt2 (x:xs) rows cols = (sum . map (\k -> calcDistancePt2 x k rows cols)) xs + (calcAllDistancePt2 xs rows cols)

solution :: String -> Int
solution xs = calcAllDistancePt2 galaxies rows cols
    where
        grid = parseString xs
        galaxies = findGalaxies grid 
        rows = getEmptyRow grid
        cols = getEmptyCol grid



main :: IO ()
main = do
    xs <- readFile "puzzle_input.txt"
    putStrLn $ show $ solution xs


test :: String
test = "...#......\n.......#..\n#.........\n..........\n......#...\n.#........\n.........#\n..........\n.......#..\n#...#....."