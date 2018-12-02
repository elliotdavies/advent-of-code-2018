module Day01.Day01 where

import qualified Data.Set   as Set
import           Prelude

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let actions  = parse input
      start    = 0
      solution1  = foldr ($) start actions

  putStrLn $ "Solution pt 1 = " ++ show solution1

  let solution2  = findFirstDuplicate start $ cycle actions

  putStrLn $ "Solution pt 2 = " ++ show solution2

type Action = Int -> Int

parse :: [String] -> [Action]
parse = map parse'
  where
    parse' (x:xs)
      = let i = read xs :: Int
        in  if x == '+' then (+i) else (subtract i)

findFirstDuplicate :: Int -> [Action] -> Int
findFirstDuplicate start actions
  = go actions start (Set.singleton start)
  where
    go (f : fs) acc seen =
      let res = f acc
      in  if Set.member res seen
            then res
            else go fs res (Set.insert res seen)
