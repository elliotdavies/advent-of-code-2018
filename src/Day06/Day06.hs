module Day06.Day06 where

import Data.List as List
import qualified Data.Map as Map


type Coord = (Int, Int)

data Input = Input [Coord] Int Int Int Int


getInput :: IO Input
getInput = do
  coords <- map parse . lines <$> readFile "input.txt"

  let xs = map fst coords
      ys = map snd coords

  pure $ Input coords (minimum xs) (minimum ys) (maximum xs) (maximum ys)     
  where 
    parse :: String -> Coord
    parse s =
      let x = read (takeWhile (/=',') s) :: Int
          y = read (drop 1 $ dropWhile (/=',') s) :: Int
      in  (x,y)


manhattanDist :: Coord -> Coord -> Int
manhattanDist (x,y) (x',y') = abs (x - x') + abs (y - y')


solution1 :: IO ()
solution1 = do
  (Input locs minX minY maxX maxY) <- getInput

  let grid = [ (x,y) | x <- [minX..maxX], y <- [minY..maxY] ]
      onEdge (x,y) = x == minX || x == maxX || y == minY || y == maxY
      results = map (calcArea onEdge grid locs) locs

  putStrLn $ "Largest area: " <> (show $ maximum results)

  where
    calcArea :: (Coord -> Bool) -> [Coord] -> [Coord] -> Coord -> Maybe Int
    calcArea onEdge grid locs loc
      = go (Just 0) grid
      where
        otherLocs = filter (/= loc) locs

        -- Return Nothing for an infinite area, else `Just` the area
        go :: Maybe Int -> [Coord] -> Maybe Int
        go acc [] = acc
        go acc (coord:cs)
          = let closest = minimum $ map (manhattanDist coord) otherLocs
                dist = manhattanDist coord loc
            in  if dist < closest
                  then
                    if not (onEdge coord)
                      then go ((+1) <$> acc) cs
                      else Nothing
                  else go acc cs


solution2 :: IO ()
solution2 = do
  (Input locs minX minY maxX maxY) <- getInput
  
  let grid = [ (x,y) | x <- [minX..maxX], y <- [minY..maxY] ]
      result = length . filter (< 10000) $ map (calcTotalDist locs) grid

  putStrLn $ "Region size: " <> show result
  
  where
    calcTotalDist :: [Coord] -> Coord -> Int
    calcTotalDist locs c
      = sum $ map (manhattanDist c) locs 

