module Day07.Day07 where

import qualified Data.Map  as Map
import           Data.List (elemIndex, isSubsequenceOf, nub, partition, sort)


data Dependency
  = Char `DependsOn` Char


getInput :: IO [Dependency]
getInput = map (parse . words) <$> (lines <$> readFile "input.txt")
  where
    parse ["Step", step, "must", "be", "finished", "before", "step", step', "can", "begin."]
      = head step' `DependsOn` head step


buildDependencyMap :: [Dependency] -> Map.Map Char [Char]
buildDependencyMap deps
  = foldr buildMap (initMap $ findAllSteps deps) deps
  where
    initMap = foldr (\c acc -> Map.insert c [] acc) Map.empty
    
    buildMap (a `DependsOn` b) = Map.adjust (b :) a

    findAllSteps = nub . foldr (\(a `DependsOn` b) acc -> (a:b:acc)) ""


candidateSteps :: [Char] -> Map.Map Char [Char] -> [Char]
candidateSteps done 
  = sort
  . Map.keys
  . Map.filterWithKey (\k deps -> deps `within` done && not (k `elem` done))
  where
    a `within` b = sort a `isSubsequenceOf` sort b


solution1 :: IO ()
solution1 = do
  deps <- getInput
  putStrLn $ orderSteps Nothing "" (buildDependencyMap deps)
  where
    orderSteps :: Maybe Char -> [Char] -> Map.Map Char [Char] -> [Char]
    orderSteps maybeCurrent done depMap
      = let done'      = maybe done (\c -> done ++ [c]) maybeCurrent
            candidates = candidateSteps done' depMap

        in  if length candidates == 0
              then done'
              else orderSteps (Just $ head candidates) done' depMap


solution2 :: IO ()
solution2 = do
  deps <- getInput
  putStrLn . show $ orderSteps [] "" 0 (buildDependencyMap deps)
  where
    orderSteps :: [(Char, Int)] -> [Char] -> Int -> Map.Map Char [Char] -> Int
    orderSteps current done count depMap
      = let (newlyFinished, inProgress) = partition ((==1) . snd) current
            
            done' = done ++ map fst newlyFinished
            
            candidates
              = map init
              . take (5 - length inProgress)
              . filter (\c -> not $ c `elem` (map fst inProgress))
              $ candidateSteps done' depMap
            
            current' = map (\(c,t) -> (c,t-1)) inProgress ++ candidates

        in  if length current' == 0
              then count
              else orderSteps current' done' (count + 1) depMap

    init c = (c, maybe 0 (+61) (elemIndex c ['A'..'Z']))

