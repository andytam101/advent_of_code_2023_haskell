import Data.Maybe (fromJust)
import Data.List (findIndex)

parseString :: String -> (String, [(String, (String, String))])
parseString xs = (head ls, map parseLine (drop 2 ls))
    where
        ls = lines xs

parseLine :: String -> (String, (String, String))
parseLine xs = (front f, (l, front (drop 2 r)))
    where
        (f, b) = break (=='=') xs
        back = drop 3 b
        (l, r) = break (==',') back

front :: [a] -> [a]
front [x] = []
front (x:xs) = x:(front xs)

move :: (String, String) -> Char -> String
move (xs, ys) 'L' = xs
move (xs, ys) 'R' = ys

getNextString :: [(String, (String, String))] -> String -> Char -> String
getNextString m xs = move ((fromJust . lookup xs) m)

countSteps :: [(String, (String, String))] -> Int -> String -> String -> String -> Int
countSteps m n current (s:ss) target
    | current == target = n
    | otherwise = countSteps m (n + 1) (getNextString m current s) (ss ++ [s]) target

-- solution :: String -> Int
-- solution xs = countSteps m 0 "AAA" steps "ZZZ"
--     where
--         (steps, m) = parseString xs

-- part 2
getAllA :: [(String, (String, String))] -> [String]
getAllA = (map fst) . (filter ((=='A') . last . fst)) 

countAllSteps :: [(String, (String, String))] -> Int -> [String] -> String -> Int
countAllSteps m n xs (s:ss)
    | all ((=='Z') . last) xs = n
    | otherwise = countAllSteps m (n + 1) (map (\x -> getNextString m x s) xs)  (ss ++ [s])

getLoop :: [(String, (String, String))] -> [String] -> String -> String -> (Int, Int, Int)
getLoop m xs current (s:ss)
    | current `elem` xs = (idx, length xs - idx, fromJust (findIndex ((=='Z') . last) xs))
    | otherwise = getLoop m (xs ++ [current]) (getNextString m current s) (ss ++ [s])
    where
        idx = fromJust $ findIndex (==current) xs


findPathLength :: Int -> [(String, (String, String))] -> String -> String -> Int
findPathLength n m target (s:ss)
    | last target == 'Z' = n
    | otherwise = findPathLength (n + 1) m (getNextString m target s) (ss ++ [s])

solution :: String -> Int
solution xs = foldr (lcm) 1 (map (\x -> findPathLength 0 m x steps) (getAllA m))
    where
        (steps, m) = parseString xs

main :: IO ()
main = do
    xs <- readFile "puzzle_input.txt"
    putStrLn $ show $ solution xs
    