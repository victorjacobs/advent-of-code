main = do
    content <- readFile "data.txt"
    putStrLn $ show $ notQuiteLisp content

notQuiteLisp :: String -> Int
notQuiteLisp xs = sum [if x == '(' then 1 else -1 | x <- xs]