module Day03.Day03 where

import Data.Char (isDigit)
import Data.List (find)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Prelude


type Id = Int
type Coord = (Int, Int)

data Claim
  = Claim Id [Coord]


getInput :: IO [Claim]
getInput = (map parseClaim . lines) <$> readFile "input.txt"


-- I really should learn to use a parser library
parseClaim :: String -> Claim
parseClaim s
  = let [id, left, top, width, height] = map toInt . words $ map digitOrEmpty s
    in  Claim id (mkCoords left top width height)
    where
      toInt :: String -> Int
      toInt = read

      digitOrEmpty c = if isDigit c then c else ' '

      mkCoords :: Int -> Int -> Int -> Int -> [Coord]
      mkCoords left top width height
        = let xs = [left .. left + width - 1]
              ys = [top .. top + height - 1]
          in  [ (x,y) | x <- xs, y <- ys ]


findConflicts :: [Claim] -> Map.Map Coord [Id]
findConflicts = Map.filter ((>1) . length) . foldr go Map.empty
  where
    go :: Claim -> Map.Map Coord [Id] -> Map.Map Coord [Id]
    go (Claim id coords) map
      = foldr (Map.alter (Just . maybe [id] ((:) id))) map coords


solution1 :: IO ()
solution1 = do
  conflicts <- findConflicts <$> getInput
  putStrLn $ "There are " <> show (Map.size conflicts) <> " squares with overlapping claims"


solution2 :: IO ()
solution2 = do
  claims <- getInput
  let allIds = map (\(Claim id _) -> id) claims
      conflictingIds = concat . Map.elems $ findConflicts claims
      result = Set.difference (Set.fromList allIds) (Set.fromList conflictingIds)
  putStrLn $ "IDs with no conflicts: " <> show result
