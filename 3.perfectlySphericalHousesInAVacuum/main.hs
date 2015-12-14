import qualified Data.Map.Strict as Map

type Position = (Int, Int)
type State = (Position, Map.Map Position Int)

main = do
    content <- readFile "data.txt"
    let initialState = ((0, 0), Map.empty)
    let (_, result) = foldl step initialState content
    putStrLn $ show $ Map.size result

step :: State -> Char -> State
step (pos, map) c = (newPos, newMap)
    where
        newPos = nextPosition pos c
        newMap = Map.alter updateMap newPos map

updateMap :: Maybe Int -> Maybe Int
updateMap Nothing = Just 1
updateMap (Just count) = Just $ count + 1

-- List of all positions visited
positions :: String -> [Position]
positions str = scanl nextPosition (0, 0) str

nextPosition :: Position -> Char -> Position
nextPosition (x, y) '^' = (x, y+1)
nextPosition (x, y) '>' = (x+1, y)
nextPosition (x, y) '<' = (x-1, y)
nextPosition (x, y) 'v' = (x, y-1)