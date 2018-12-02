module Day02.Day02 where

import           Data.List       (find)
import qualified Data.Map.Strict as Map
import           Prelude

getInput :: IO [String]
getInput = lines <$> readFile "input.txt"

-- Pt 1

solution1 :: IO ()
solution1 = do
  input <- getInput
  
  let checksum = (*) <$> fst <*> snd $ scanBoxes input
  putStrLn $ "Solution 1: Checksum was " ++ show checksum


scanBoxes :: [String] -> (Int, Int)
scanBoxes = foldr go (0, 0)
  where
    go :: String -> (Int, Int) -> (Int, Int)
    go s (twos, threes)
      = let counts  = Map.elems $ countChars s
            twos'   = if any (==2) counts then twos + 1   else twos
            threes' = if any (==3) counts then threes + 1 else threes
        in  (twos', threes')


countChars :: String -> Map.Map Char Int
countChars = foldr (Map.alter (Just . maybe 1 (+1))) Map.empty


-- Pt 2

solution2 :: IO ()
solution2 = do
  input <- getInput

  let maybeId = findSimilarBoxes input
  putStrLn $ "Solution 2: Common letters were " ++ show maybeId


findSimilarBoxes :: [String] -> Maybe String
findSimilarBoxes = diff Nothing
  where
    diff (Just acc) _ = Just acc
    diff Nothing (id : ids)
      = case find ((== length id - 1) . length) (map (commonChars id) ids) of
          Just res -> Just res
          Nothing  -> diff Nothing ids


commonChars :: String -> String -> String
commonChars = go ""
  where
    go acc []     _      = acc
    go acc _      []     = acc
    go acc (a:as) (b:bs) = go (if a == b then acc ++ [a] else acc) as bs
