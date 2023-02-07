module MapLib (exampleMap, printMap, Coord) where

type Coord = (Int, Int)
data Node = Node { coord :: Coord, isObstacle :: Bool }
type Map = [[Node]] -- list of lists 2d map

createMap :: Int -> Int -> [(Coord, Bool)] -> Map
-- in lists | means to satisfy. instead of else
createMap rows cols obstacles = [[Node (x, y) (isObstacleAt (x, y)) | x <- [0..cols-1]] | y <- [0..rows-1]]
  where
    isObstacleAt coord = coord `elem` map fst obstacles

exampleMap :: Map
exampleMap = createMap 5 5 [( (1,1), True), ((3,3), True)]

printMap :: Map -> IO ()
printMap = mapM_ (putStrLn . rowToString)
  where
    rowToString = concatMap nodeToString
    nodeToString (Node _ True) = "X "
    nodeToString (Node _ False) = "- "
