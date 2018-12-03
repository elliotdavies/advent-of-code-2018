module Day03.Day03 where

import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Prelude


type Id = Int
type Coord = (Int, Int)

data Claim
  = Claim Id [Coord]
  deriving (Show)

getInput :: IO [Claim]
getInput = (map parseClaim . lines) <$> readFile "input.txt"

-- I really should learn to use a parser library
parseClaim :: String -> Claim
parseClaim s
  = let ((_:id) : _ : leftTop : widthHeight : _) = splitOn " " s
        (left : top': _) = splitOn "," leftTop
        top = takeWhile (/= ':') top'
        (width : height : _) = splitOn "x" widthHeight
    in  Claim
          (toInt id)
          (mkCoords (toInt left) (toInt top) (toInt width) (toInt height))
    where
      toInt :: String -> Int
      toInt = read

      mkCoords :: Int -> Int -> Int -> Int -> [Coord]
      mkCoords left top width height
        = let xs = [left .. left + width - 1]
              ys = [top .. top + height - 1]
          in  [ (x,y) | x <- xs, y <- ys]

solution1 :: IO ()
solution1 = do
  claims <- getInput
  let coordinates = foldr go Map.empty claims
      conflicts = Map.filter ((>1) . length) coordinates
  putStrLn $ "There are " <> show (Map.size conflicts) <> " squares with overlapping claims"
  where
    go :: Claim -> Map.Map Coord [Id] -> Map.Map Coord [Id]
    go (Claim id coords) map
      = foldr (Map.alter (Just . maybe [id] ((:) id))) map coords
