module Day08.Day08 where

import Control.Arrow (first)
import Data.List (elemIndex, null) 
import Data.Tree 


getInput :: IO [Int]
getInput = map (read :: String -> Int) . words <$> readFile "input.txt"


newtype Metadata
  = Metadata [Int]
  deriving Show


parseTree :: [Int] -> (Tree Metadata, [Int])
parseTree (childCount : metaCount : rest)
  = let (children, rest') = parseChildren childCount rest
        (meta, rest'')    = parseMeta metaCount rest'
    in  (Node meta children, rest'')
  where
    parseMeta = (first Metadata .) . splitAt


parseChildren :: Int -> [Int] -> (Forest Metadata, [Int])
parseChildren n input
  = if n > 0 then foldr step ([], input) [0..(n-1)] else ([], input)
  where
    step _ (forest, [])    = (forest, [])
    step _ (forest, input) = first (\t -> forest ++ [t]) $ parseTree input


solution1 :: IO ()
solution1 = do
  input <- getInput
  putStrLn . show . sum . map sumMeta . flatten . fst $ parseTree input
  where
    sumMeta (Metadata ms) = sum ms


solution2 :: IO ()
solution2 = do
  input <- getInput
  putStrLn . show . foldTree calcValue . fst $ parseTree input
  where
    calcValue (Metadata ms) childValues
      = if null childValues then sum ms else foldr lookupChild 0 ms
      where
        lookupChild i acc = maybe acc (+ acc) $ elemAt (i-1) childValues

        elemAt _ []     = Nothing
        elemAt i (x:xs)
          | i < 0     = Nothing
          | i == 0    = Just x
          | otherwise = elemAt (i-1) xs

