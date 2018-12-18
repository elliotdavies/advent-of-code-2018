module Day12.Day12 where

import           Data.List.Index (indexed)
import qualified Data.IntSet as S


type State = S.IntSet
type Rule = [Bool]


hash :: Char -> Bool
hash = (=='#')


getInput :: IO (State, [Rule])
getInput = do
  (stateInput:_:rulesInput) <- lines <$> readFile "input.txt"
  let state = parseState . head . drop 2 . words $ stateInput
      rules = map parseRule . filter (hash . last) $ rulesInput
  pure (state, rules)
  where
    parseState = S.fromList . map fst . filter (hash . snd) . indexed

    parseRule = map hash . head . words


stepGen :: [Rule] -> State -> State
stepGen rules state
  = let min = S.findMin state - 2
        max = S.findMax state + 2
    in  foldl step S.empty $ [min..max]
  where
    step acc i
      = if any (== getSequence i) rules then S.insert i acc else acc

    getSequence i
      = map (flip S.member $ state) [i-2 .. i+2]


solution1 :: IO ()
solution1 = do
  (state, rules) <- getInput
  putStrLn . show . sumState . (!! 20) $ iterate (stepGen rules) state
  where
    sumState = S.foldr (+) 0


solution2 :: IO ()
solution2 = do
  putStrLn "?"
