module Day07.Day07 where


import qualified Data.Map  as Map
import           Data.List (isSubsequenceOf, sort, nub)


data Dependency
  = Char `DependsOn` Char

instance Show Dependency where
  show (a `DependsOn` b) = [a] ++ " depends on " ++ [b]


getInput :: IO [Dependency]
getInput = map (parse . words) <$> (lines <$> readFile "input.txt")
  where
    parse ["Step", step, "must", "be", "finished", "before", "step", step', "can", "begin."]
      = head step' `DependsOn` head step


solution1 :: IO ()
solution1 = do
  deps <- getInput

  let allSteps = nub . foldr (\(a `DependsOn` b) acc -> (a:b:acc)) "" $ deps
      dependencyMap = foldr buildMap (initMap allSteps) deps
      
  putStrLn $ orderSteps Nothing "" dependencyMap

  where
    initMap = foldr (\c acc -> Map.insert c [] acc) Map.empty

    buildMap (a `DependsOn` b) = Map.adjust (b :) a

    a `within` b = sort a `isSubsequenceOf` sort b
    
    orderSteps :: Maybe Char -> [Char] -> Map.Map Char [Char] -> [Char]
    orderSteps maybeCurrent done map
      = let done' = maybe done (\c -> done ++ [c]) maybeCurrent
            candidates
              = sort
              . Map.keys
              $ Map.filterWithKey
                  (\k deps -> deps `within` done' && not (k `elem` done'))
              map

        in  if length candidates == 0
              then done'
              else orderSteps (Just $ head candidates) done' map

