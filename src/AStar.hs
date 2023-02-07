import Data.List
import Data.Maybe

data Node = Node { x :: Int, y :: Int }
  deriving (Eq, Ord, Show)

data State = State { node :: Node, fScore :: Int, gScore :: Int }
  deriving (Eq, Ord, Show)

type Grid = [String]

heuristicCost :: Node -> Node -> Int
heuristicCost a b = abs (x a - x b) + abs (y a - y b)

validMove :: Grid -> Node -> Bool
validMove grid (Node x y) = 
  x >= 0 && x < length grid && y >= 0 && y < length (head grid) && (grid !! x !! y) /= '#'

neighbors :: Node -> [Node]
neighbors (Node x y) = 
  filter (validMove grid . (\(a, b) -> (x + a, y + b))) [(0, 1), (0, -1), (1, 0), (-1, 0)]

updateState :: Grid -> State -> Node -> State
updateState grid state node = 
  let gScore = gScore state + 1
      fScore = gScore + heuristicCost node end
  in State node fScore gScore

updateNeighbors :: Grid -> State -> [State]
updateNeighbors grid state = map (updateState grid state . node) (neighbors . node $ state)

findPath :: Grid -> State -> [State] -> Maybe [Node]
findPath grid state seenStates = 
  if node state == end
  then Just [end]
  else case find (\x -> node x == end) (sortOn fScore . updateNeighbors grid $ state) of
    Just finalState -> Just (end : (node . head . dropWhile (/= finalState) $ seenStates))
    Nothing -> Nothing

astar :: Grid -> Node -> Node -> Maybe [Node]
astar grid start end = 
  let initialState = State start (heuristicCost start end) 0
      states = iterate (sortOn fScore . concatMap (updateNeighbors grid)) [initialState]
  in findPath grid (head states) [initialState]