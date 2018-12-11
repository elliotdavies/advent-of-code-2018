module Day11.Day11 where

import Data.List (maximumBy)


newtype Coord = Coord (Int, Int)
  deriving (Show)


data Res = Res Coord Int
  deriving (Show)

instance Eq Res where
  (Res _ a) == (Res _ b) = a == b

instance Ord Res where
  (Res _ a) `compare` (Res _ b) = a `compare` b


input :: Int
input = 18
--input = 8772


cellPower :: Coord -> Int
cellPower (Coord (x,y))
  = let rackId = x + 10
    in  hundreds ((y * rackId + input) * rackId) - 5


hundreds :: Int -> Int
hundreds i
  = case drop 2 . take 3 . reverse . show $ i of
      [] -> 0
      x  -> read x :: Int


genGrid :: Int -> [Coord]
genGrid size =
  Coord <$> [(x,y) | x <- [0 .. size - 3], y <- [0 .. size - 3]]


gridSquarePower :: Coord -> Res
gridSquarePower (Coord (x,y))  
  = let total = sum . map cellPower $
             [ Coord (x,y), Coord (x+1,y), Coord (x+2,y)
             , Coord (x,y+1), Coord (x+1,y+1), Coord (x+2,y+1)
             , Coord (x,y+2), Coord (x+1,y+2), Coord (x+2,y+2)
             ]
    in  Res (Coord (x,y)) total


solution1 :: IO ()
solution1 = do
  let grid = genGrid 300
      res  = maximum $ map gridSquarePower grid
  putStrLn $ show res


solution2 :: IO ()
solution2 = do
  let grids = map genGrid [3..300]
      res   = maximum . map (maximum . map gridSquarePower) $ grids
  putStrLn $ show res
