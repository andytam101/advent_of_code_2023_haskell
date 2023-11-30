import Data.Char (ord, isDigit)
import Data.Maybe


-- part 1


extractString :: String -> [Int] -> [Int]
extractString xs ys = foldr extractStringHelper ys xs

extractStringHelper :: Char -> [Int] -> [Int]
extractStringHelper x ys
    | isDigit x = case (length ys) of
        0 -> [(charToNum) x]
        1 -> (charToNum) x:ys
        2 -> (charToNum) x:(tail ys)
    | otherwise = ys

charToNum :: Char -> Int
charToNum x = ord x - ord '0'

combine :: [Int] -> Int
combine [x] = 10 * x + x
combine [x, y] = 10 * x + y


-- part 2

strToNum :: String -> Maybe Int
strToNum "one" = Just 1
strToNum "two" = Just 2
strToNum "three" = Just 3
strToNum "four" = Just 4
strToNum "five" = Just 5
strToNum "six" = Just 6
strToNum "seven" = Just 7
strToNum "eight" = Just 8
strToNum "nine" = Just 9
strToNum _ = Nothing

getFirstNum :: String -> Maybe Int
getFirstNum xs = (safeHead . (mapMaybe strToNum)) [take 3 xs, take 4 xs, take 5 xs]

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

extractStringPart2 :: String -> [Int] -> [Int]
extractStringPart2 "" ys = ys
extractStringPart2 xxs@(x:xs) ys
    | isDigit x = extractStringPart2 xs (combineList (charToNum x) ys)
    | isJust (getFirstNum xxs) = extractStringPart2 xs (combineList ((fromJust . getFirstNum) xxs) ys)
    | otherwise = extractStringPart2 xs ys
 
combineList :: Int -> [Int] -> [Int]
combineList x [] = [x]
combineList x [y] = [y, x]
combineList x [y, z] = [y, x]

main :: IO ()
main = do
    xs <- readFile "puzzle_input.txt"
    putStrLn ((show . sum) (map (combine . (`extractStringPart2` [])) (lines xs)))