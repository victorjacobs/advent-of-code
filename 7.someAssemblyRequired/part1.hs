import Data.Bits

main = do
    content <- readFile "data.txt"
    let lights = foldl' processLine Map.empty (lines content)
    putStrLn $ show $ Map.foldl (+) 0 lights