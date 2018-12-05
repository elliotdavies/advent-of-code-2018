module Day05.Day05 where

import Data.Char (isUpper, toLower)
import Data.List (nub)

type Polymer = String


getInput :: IO Polymer
getInput = readFile "input.txt"


-- Reduce a polymer by folding it
-- https://en.wikipedia.org/wiki/Crystallization_of_polymers
crystalise :: Polymer -> Polymer
crystalise = foldr go ""
  where
    go :: Char -> Polymer -> Polymer
    go c []     = [c]
    go c (p:ps) = if reactive c p then ps else (c:p:ps)

    reactive :: Char -> Char -> Bool
    reactive a b = a /= b && toLower a == toLower b


solution1 :: IO ()
solution1 = do
  result <- crystalise <$> getInput
  putStrLn $ "Units remaining: " <> show (length result)


solution2 :: IO ()
solution2 = do
  input <- getInput
  let shortest
        = minimum
        . map (length . crystalise . removeUnit input)
        . nub
        . filter isUpper
        $ input

  putStrLn $ "Shortest chain: " <> show shortest
  where
    removeUnit :: Polymer -> Char -> Polymer
    removeUnit p c = filter (/= toLower c) $ filter (/=c) p 
