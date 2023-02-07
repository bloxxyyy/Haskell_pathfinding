--module Main where

import qualified MapLib (exampleMap, printMap, Coord)

cost :: MapLib.Coord -> MapLib.Coord -> Int
cost c1 c2 = abs (fst c1 - fst c2) + abs (snd c1 - snd c2)

evaluateCoord :: MapLib.Coord -> Int
evaluateCoord coord = cost coord (0, 0)

getNeighborCoords :: Map -> MapLib.Coord -> Maybe [MapLib.Coord]
getNeighborCoords map startCoord =
  let
    neighbors = [(fst startCoord + x, snd startCoord + y) | x <- [-1, 0, 1], y <- [-1, 0, 1], x /= 0 || y /= 0]
    isValidCoord (x, y) = x >= 0 && x < MapLib.rows map && y >= 0 && y < MapLib.columns map && not (MapLib.obstructionAt map (x, y) [( (1,1), True), ((3,3), True)])
  in Just (filter isValidCoord neighbors)

--findPath :: Map -> MapLib.Coord -> MapLib.Coord -> Maybe [MapLib.Coord]
--findPath MapLib.exampleMap startCoord endCoord =
    
--main :: IO ()
--main = do
  --  print (getNeighborCoords MapLib.exampleMap (4, 4)) -- 4, 4 in the start coord
   -- MapLib.printMap MapLib.exampleMap
    

    -- https://www.youtube.com/watch?v=-L-WgKMFuhE
    -- http://learnyouahaskell.com/chapters
    -- https://www.seas.upenn.edu/~cis1940/spring13/lectures.html