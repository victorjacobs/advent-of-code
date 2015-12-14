import qualified Data.Map.Strict as Map

type State = (Map.Map String Int, Int) -- (pairs, repeats)

main = do
    content <- readFile "data.txt"
    let niceStringsInFile = niceStrings $ lines content
    putStrLn $ show $ length $ niceStringsInFile

niceStrings :: [String] -> [String]
niceStrings strs = filter (isNiceString (Map.empty, 0)) strs

isNiceString :: State -> String -> Bool
-- Base case: at least 3 vowels and at least one occurence of 1 letter twice in a row
isNiceString (pairs, nbOfRepeats) []
    | (Map.size $ Map.filter (>= 2) pairs) >= 1 && nbOfRepeats >= 1 = True
    | otherwise = False
-- Repeats separated by a letter
isNiceString (pairs, nbOfRepeats) str@(sa:sb:sc:_)
    | sa == sc && sa == sb = isNiceString (newPairs, nbOfRepeats + 1) (tail str)
    | sa == sc = isNiceString (pairs, nbOfRepeats + 1) (tail str)
    | sa == sb = isNiceString (newPairs, nbOfRepeats) (tail str)
        where newPairs = Map.alter updateMap (take 2 str) pairs
isNiceString state str = isNiceString state (tail str)

updateMap :: Maybe Int -> Maybe Int
updateMap Nothing = Just 1
updateMap (Just n) = Just $ n + 1