module Day11.Day11 where

newtype Coord = Coord (Int, Int)


input :: Int
input = 8
-- input = 8772


cellPower :: Coord -> Int
cellPower (Coord (x,y))
  = let rackId = x + 10
    in  hundreds ((y * rackId + input) * rackId) - 5


hundreds :: Int -> Int
hundreds i
  = case drop 2 . take 3 . reverse . show $ i of
      [] -> 0
      x  -> read x :: Int

