import Data.Char (isDigit, ord)
import Data.List (nub)
import System.IO

isCounted :: [String] -> [(Int, Int)] -> Bool
isCounted xs ys = foldr ((||) . isCountedSingle xs ) False ys

isCountedSingle :: [String] -> (Int, Int) -> Bool
isCountedSingle cs (x, y)  = foldr ((||) . isSymbol . (\(x, y) -> cs !! x !! y)) False (getAdjacentCoords cs (x, y))

getAdjacentCoords :: [String] -> (Int, Int) -> [(Int, Int)]
getAdjacentCoords cs (x, y) = filter (\(x, y) -> (x >= 0 && x < m && y >= 0 && y < n)) adjacent 
    where
        adjacent = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1), (x + 1, y + 1), (x + 1, y - 1), (x - 1, y + 1), (x - 1, y - 1)]
        (m, n) = (length cs, length (cs !! 0))

isSymbol :: Char -> Bool
isSymbol x = not (isDigit x) && (x /= '.')

getNums :: Int -> [String] -> [[(Int, Int)]]
getNums _ [] = []
getNums n (xs:xss) = (getNumsFromLine xs n) ++ (getNums (n + 1) xss)

getNumsFromLine :: String -> Int -> [[(Int, Int)]]
getNumsFromLine xs n = map (map ((,) n)) (filter ((/=0) . length) (getNumsFromLineHelper [] [] 0 xs))

getNumsFromLineHelper :: [[Int]] -> [Int] -> Int -> String -> [[Int]]
getNumsFromLineHelper css cs n xs
    | length xs == n = cs:css
    | isDigit (xs!!n) = getNumsFromLineHelper css (n:cs) (n + 1) xs
    | otherwise = getNumsFromLineHelper (cs:css) [] (n + 1) xs

coordsToNum :: [String] -> [(Int, Int)] -> Int
coordsToNum _ [] = 0
coordsToNum cs ((x, y):xys) = toDigit (cs !! x !! y) + 10 * coordsToNum cs xys

toDigit :: Char -> Int
toDigit c = ord c - ord '0'

-- solution :: String -> Int
-- solution xs = sum $ map (coordsToNum ls) (filter (isCounted ls) (getNums 0 ls))
--     where
--         ls = lines xs

-- main :: IO ()
-- main = do
--     xs <- readFile "puzzle_input.txt"
--     print (solution xs)


-- part 2

locateStars :: Int -> [String] -> [(Int, Int)]
locateStars n [] = []
locateStars n (x:xs) = map ((,) n) (locateStarsHelper x) ++ locateStars (n + 1) xs

locateStarsHelper :: String -> [Int]
locateStarsHelper "" = []
locateStarsHelper ('*':xs) = 0 : (map (+1) (locateStarsHelper xs))
locateStarsHelper (x:xs) = map (+1) (locateStarsHelper xs)

starsAdjacent :: [String] -> (Int, Int) -> [Int]
starsAdjacent cs xy@(x, y) = map (toNum cs) ((nub . map (shiftRight cs)) (starsAdjacentCoords cs xy))

starsAdjacentCoords :: [String] -> (Int, Int) -> [(Int, Int)]
starsAdjacentCoords cs (x, y) = filter (\(x, y) -> isDigit(cs !! x !! y)) (getAdjacentCoords cs (x, y))

shiftRight :: [String] -> (Int, Int) -> (Int, Int)
shiftRight xs (r, n)
    | length (xs !! r) == n = (r, n - 1)
    | isDigit (xs !! r !! n) = shiftRight xs (r, n + 1)
    | otherwise = (r, n - 1)

toNum :: [String] -> (Int, Int) -> Int
toNum xs (r, -1) = 0
toNum xs (r, n) 
    | isDigit (xs !! r !! n) = toDigit (xs !! r !! n) + 10 * (toNum xs (r, n - 1))
    | otherwise = 0

gearRatio :: [Int] -> Int
gearRatio [x, y] = x * y
gearRatio _ = 0

solution :: String -> Int
solution xs = sum $ map (gearRatio . (starsAdjacent ls)) (locateStars 0 ls)
    where
        ls = lines xs

main :: IO ()
main = do
    xs <- readFile "puzzle_input.txt"
    print (solution xs)

a :: String
a = "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......7554\n...$.*....\n.664.598.."
