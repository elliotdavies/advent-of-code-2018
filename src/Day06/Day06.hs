module Day06.Day06 where

import Data.List as List
import qualified Data.Map as Map


type Coord = (Int, Int)


getInput :: IO [Coord]
getInput = map parse . lines <$> readFile "input.txt"
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
  locs <- getInput

  let xs = map fst locs
      ys = map snd locs

      minX = minimum xs
      minY = minimum ys
      maxX = maximum xs
      maxY = maximum ys

      grid = [ (x,y) | x <- [minX..maxX], y <- [minY..maxY] ]

      onEdge (x,y) = x == minX || x == maxX || y == minY || y == maxY

      results = map (calcArea onEdge grid locs) locs

  putStrLn $ show results
  putStrLn $ "Largest area: " <> (show $ maximum results)

  where
    calcArea :: (Coord -> Bool) -> [Coord] -> [Coord] -> Coord -> Maybe Int
    calcArea onEdge grid locs loc
      = go (Just 0) grid
      where
        locs' = filter (/= loc) locs

        go :: Maybe Int -> [Coord] -> Maybe Int
        go acc [] = acc
        go acc (coord:cs)
          = let closest = minimum $ map (manhattanDist coord) locs'
                dist = manhattanDist coord loc
            in  if dist < closest
                  then
                    if not (onEdge coord)
                      then go ((+1) <$> acc) cs
                      else Nothing
                  else go acc cs

