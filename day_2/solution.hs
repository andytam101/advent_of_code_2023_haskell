splitId :: String -> (Int, String)
splitId xs = ((read . last . words) y, tail zs)
    where
        (y, zs) = break (==':') xs

splitRounds :: String -> [String]
splitRounds xs 
    | length ns > 0 = (tail t):(splitRounds ys)
    | otherwise = [tail t] 
    where
        (t, ns) = break (==';') xs
        ys = tail ns

splitColours :: String -> [String]
splitColours xs
    | length ns > 0 = t:(splitColours (drop 2 ns))
    | otherwise = [t]
    where
        (t, ns) = break (==',') xs

calculateColours :: [String] -> (Int, Int, Int)
calculateColours xs = foldr (\x -> \y -> addColours y (splitColourNum x)) (0, 0, 0) xs 

splitColourNum :: String -> (Int, String)
splitColourNum xs = (read n, tail c)
    where
        (n, c) = break (==' ') xs

addColours :: (Int, Int, Int) -> (Int, String) -> (Int, Int, Int)
addColours (r, g, b) (n, "red") = (r + n, g, b)
addColours (r, g, b) (n, "green") = (r, g + n, b)
addColours (r, g, b) (n, "blue") = (r, g, b + n)

parseString :: String -> (Int, [(Int, Int, Int)])
parseString xs = (id, cs)
    where
        (id, rounds) = splitId xs
        rs = splitRounds rounds
        css = map splitColours rs
        cs = map calculateColours css

isValidRound :: (Int, Int, Int) -> Bool
isValidRound (r, g, b) = r <= 12 && g <= 13 && b <= 14

isValid :: (Int, [(Int, Int, Int)]) -> Int
isValid (id, rs)
    | all isValidRound rs = id
    | otherwise = 0


-- part 2
calculateMax :: [(Int, Int, Int)] -> (Int, Int, Int) -> (Int, Int, Int)
calculateMax [] (r, g, b) = (r, g, b) 
calculateMax ((r1, g1, b1):xs) (r, g, b) = calculateMax xs (max r r1, max g g1, max b b1)

powerSet :: (Int, Int, Int) -> Int
powerSet (r, g, b) = r * g * b

main :: IO ()
main = do
    file <- readFile "puzzle_input.txt"
    let games = lines file
    let result = map parseString games
    -- let solution = (sum . map isValid) result
    let solution = sum $ map (\(x, ys) -> (powerSet . (`calculateMax` (0, 0, 0))) ys) result
    putStrLn (show solution)
