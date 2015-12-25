import Data.Bits
import qualified Data.Map.Strict as Map
import Data.List.Split
import Text.Read
import Data.Maybe

type Rules = Map.Map String Expression

data Expression =
    And Expression Expression |
    Or Expression Expression |
    Not Expression |
    Lshift Expression Int |
    Rshift Expression Int |
    Value Int |
    Variable String
    deriving (Show)

main = do
    content <- readFile "data.txt"
    let rules = foldr parseRule Map.empty (lines content)
    putStrLn $ show $ evaluate (fromJust $ Map.lookup "a" rules) rules

parseRule :: String -> Rules -> Rules
parseRule str =
    Map.insert name expression
        where (expressionString:name:[]) = splitOn " -> " str
              expression = parseExpression $ words expressionString

parseExpression :: [String] -> Expression
parseExpression (a:"AND":b:[]) = And (parseExpression $ words a) (parseExpression $ words b)
parseExpression (a:"OR":b:[]) = Or (parseExpression $ words a) (parseExpression $ words b)
parseExpression (a:"LSHIFT":int:[]) = Lshift (parseExpression $ words a) (read int)
parseExpression (a:"RSHIFT":int:[]) = Rshift (parseExpression $ words a) (read int)
parseExpression ("NOT":a:[]) = Not (parseExpression $ words a)
parseExpression (str:[])
    | isJust $ (readMaybe str :: Maybe Int) = Value $ read str
    | otherwise = Variable str

evaluate :: Expression -> Rules -> Int
evaluate (Value int) _ = int
evaluate (Variable str) rules = evaluate (fromJust $ Map.lookup str rules) rules
evaluate (And a b) rules = (evaluate a rules) .&. (evaluate b rules)
evaluate (Or a b) rules = (evaluate a rules) .|. (evaluate b rules)
evaluate (Not a) rules = complement $ evaluate a rules
evaluate (Lshift a int) rules = shift (evaluate a rules) int
evaluate (Rshift a int) rules = shift (evaluate a rules) (-int)