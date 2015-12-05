import Data.List.Split

main = do
    content <- readFile "data.txt"
    let result = sum $ map calculateSurfaceFromString (lines content)
    putStrLn $ show result

calculateSurfaceFromString :: String -> Int
calculateSurfaceFromString str = calculateSurface $ map read (splitOn "x" str)

calculateSurface :: [Int] -> Int
calculateSurface (l:w:h:_) = 2 * a + 2 * b + 2 * c + (min3 a b c)
    where a = l * w
          b = w * h
          c = h * l

min3 :: Int -> Int -> Int -> Int
min3 a b c = min (min a b) c