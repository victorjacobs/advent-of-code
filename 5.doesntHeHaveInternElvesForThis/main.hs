type State = (Int, Int) -- (nbOfVowels, twiceInARow)

main = do
    content <- readFile "data.txt"
    let nbNiceStrings = niceStrings $ lines content
    putStrLn $ show $ length $ nbNiceStrings

niceStrings :: [String] -> [String]
niceStrings strs = filter (isNiceString (0, 0)) strs

isNiceString :: State -> String -> Bool
-- Base case: at least 3 vowels and at least one occurence of 1 letter twice in a row
isNiceString (nbOfVowels, twiceInARow) []
    | nbOfVowels >= 3 && twiceInARow >= 1 = True
    | otherwise = False
-- Any sequence of ab cd pq or xy is bad string
isNiceString _ ('a':'b':_) = False
isNiceString _ ('c':'d':_) = False
isNiceString _ ('p':'q':_) = False
isNiceString _ ('x':'y':_) = False
-- Double letters
isNiceString (vowels, twiceInARow) str@(sa:sb:_)
	| sa == sb && elem sa "aeiou" = isNiceString (vowels + 1, twiceInARow + 1) (tail str)
    | sa == sb        = isNiceString (vowels, twiceInARow + 1) (tail str)
-- Vowels
isNiceString state@(vowels, twiceInARow) str@(sa:_)
    | elem sa "aeiou" = isNiceString (vowels + 1, twiceInARow) (tail str)
    | otherwise       = isNiceString state (tail str)