data Map = Map Int Int Int deriving Show
type Mappings = [Map]

convertNum :: Int -> Mappings -> Int
convertNum x [] = x
convertNum x ((Map d s r):ms)
    | x >= s && x <= (s + r) = d + (x - s)
    | otherwise = convertNum x ms

findLocation :: [Mappings] -> Int -> Int
findLocation mss x = foldl convertNum x mss

parseString :: String -> ([Int], [Mappings])
parseString xs = (parseSeeds (head ls), parseMappings (drop 2 ls))
    where
        ls = lines xs

parseSeeds :: String -> [Int]
parseSeeds xs = map read (words (drop 2 ss))
    where
        (_, ss) = break (==':') xs
    
parseMappings :: [String] -> [Mappings]
parseMappings xs
    | length nextMaps > 0 = (parseMaps (tail thisMap)):(parseMappings (tail nextMaps))
    | otherwise = [parseMaps (tail thisMap)]
    where
        (thisMap, nextMaps) = break (=="") xs

parseMaps :: [String] -> Mappings
parseMaps = map parseMap

parseMap :: String -> Map
parseMap xs = Map (nums !! 0) (nums !! 1) (nums !! 2)
    where
        nums = map read (words xs)

-- solution :: String -> Int
-- solution xs = (minimum . map (findLocation maps)) seeds
--     where
--         (seeds, maps) = parseString xs


-- part 2
data Range = Range Int Int deriving Show

parseSeedsPt2 :: [Int] -> [Range]
parseSeedsPt2 [] = []
parseSeedsPt2 (x:y:xs) = (Range x y):(parseSeedsPt2 xs)

minOfRanges :: [Range] -> Int
minOfRanges = minimum . map (\(Range x _) -> x)

getFinalRanges :: [Mappings] -> Range -> [Range]
getFinalRanges mss rs = foldl foldlHelper [rs] mss

foldlHelper :: [Range] -> Mappings -> [Range]
foldlHelper rs ms = concatMap (`getNextRange` ms) rs

getNextRange :: Range -> Mappings -> [Range]
getNextRange r [] = [r]
getNextRange r (m:ms) = completed ++ (concatMap (`getNextRange` ms) incomplete)
    where
        (min, max) = extractRange m
        unfiltered = [getSmallerThan r min, getGreaterThan r max, getInBetween r m]
        filtered = filter (\(Range _ a, _) -> a > 0) unfiltered
        completed = map fst (filter snd filtered)
        incomplete = map fst (filter (not . snd) filtered)

extractRange :: Map -> (Int, Int)
extractRange (Map _ src r) = (src, src + r - 1)

getSmallerThan :: Range -> Int -> (Range, Bool)
getSmallerThan (Range s r) x
    | s + r < x = (Range s r, False)
    | otherwise = (Range s (x-s), False)

getGreaterThan :: Range -> Int -> (Range, Bool)   
getGreaterThan (Range s r) x
    | s > x = (Range s r, False)
    | otherwise = (Range (x + 1) (s + r - x - 1), False)

getInBetween :: Range -> Map -> (Range, Bool)
getInBetween (Range s r) (Map des src length) = (Range (start + diff) range, True)
    where
        diff = des - src
        start = max s src
        range = min (s + r) (src + length) - start

solution :: String -> Int
solution xs = minOfRanges (concatMap (getFinalRanges maps) ranges)
    where
        (seeds, maps) = parseString xs
        ranges = parseSeedsPt2 seeds

main :: IO ()
main = do
    xs <- readFile "puzzle_input.txt"
    putStrLn (show (solution xs))    