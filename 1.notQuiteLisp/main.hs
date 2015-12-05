main = do
    content <- readFile "data.txt"
    putStrLn $ show $ last $ floors content
    putStrLn $ show $ basement content

-- Simplest list comprehension solution
finalFloor :: String -> Int
finalFloor str = sum [if x == '(' then 1 else -1 | x <- str]

floors :: String -> [Int]
floors str = scanl newFloor 0 str

newFloor :: Int -> Char -> Int
newFloor prevFloor c
    | c == '(' = prevFloor + 1
    | otherwise = prevFloor -1

basement :: String -> Int
basement str = length $ fst $ span (>= 0) (floors str)