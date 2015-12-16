import qualified Data.Map.Strict as Map
import Data.List.Split
import Data.List

type State = Map.Map Pos Int
type Pos = (Int, Int)

main = do
    content <- readFile "data.txt"
    let lights = foldl' processLine Map.empty (lines content)
    putStrLn $ show $ Map.foldl + 0 lights

processLine :: State -> String -> State
processLine state instruction = processInstruction (words instruction) state

processInstruction :: [String] -> State -> State
processInstruction ("turn":value:from:_:to:_) state
    | value == "on" = turnOnRectangle fromPos toPos state
    | value == "off" = turnOffRectangle fromPos toPos state
        where fromPos = posFromString from
              toPos = posFromString to
processInstruction ("toggle":from:_:to:_) state =
    toggleRectangle (posFromString from) (posFromString to) state

turnOnRectangle :: Pos -> Pos -> State -> State
turnOnRectangle pos1 pos2 state = executeOnRectangle turnOnLight pos1 pos2 state

turnOffRectangle :: Pos -> Pos -> State -> State
turnOffRectangle pos1 pos2 state = executeOnRectangle turnOffLight pos1 pos2 state

toggleRectangle :: Pos -> Pos -> State -> State
toggleRectangle pos1 pos2 state = executeOnRectangle toggleLight pos1 pos2 state

executeOnRectangle :: (Maybe Int -> Maybe Int) -> Pos -> Pos -> State -> State
executeOnRectangle f (x1, y1) (x2, y2) state =
    foldl' (curry $ mapAlter f) state [(x, y) | x <- [x1..x2], y <- [y1..y2]]

mapAlter :: (Maybe Int -> Maybe Int) -> (State, Pos) -> State
mapAlter f (state, pos) = Map.alter f pos state

turnOnLight :: Maybe Int -> Maybe Int
turnOnLight Nothing = Just 1
turnOnLight (Just b) = Just (b + 1)

turnOffLight :: Maybe Int -> Maybe Int
turnOffLight Nothing = Just (-1)
turnOffLight (Just b) = Just (b - 1)

toggleLight :: Maybe Int -> Maybe Int
toggleLight Nothing = Just 2
toggleLight (Just b) = Just (b + 2)

posFromString :: String -> Pos
posFromString str = (x, y)
    where [x, y] = map read $ splitOn "," str