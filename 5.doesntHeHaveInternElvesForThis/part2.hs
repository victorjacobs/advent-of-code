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
    | (Map.size $ Map.filter (>= 2) pairs) == 1 && nbOfRepeats >= 1 = True
    | otherwise = False
-- Repeats separated by a letter
isNiceString (pairs, nbOfRepeats) str@(sa:_:sc:_)
    | sa == sc = isNiceString (newPairs, nbOfRepeats + 1) (tail str)
    | otherwise = isNiceString (newPairs, nbOfRepeats) (tail str)
        where newPairs = Map.alter updateMap (take 2 str) pairs
isNiceString (pairs, nbOfRepeats) str@(_:_:_) = isNiceString (newPairs, nbOfRepeats) (tail str)
    where newPairs = Map.alter updateMap (take 2 str) pairs
isNiceString state str@(_:_) = isNiceString state (tail str)

updateMap :: Maybe Int -> Maybe Int
updateMap Nothing = Just 1
updateMap (Just n) = Just $ n + 1