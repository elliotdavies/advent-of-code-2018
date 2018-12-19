module Day12.Day12 where

import           Data.List.Index (indexed)
import qualified Data.IntSet     as S
import qualified Data.Map        as M


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


runFor :: Int -> [Rule] -> State -> State
runFor n rules
  = (!! n) . iterate (stepGen rules)


sumState :: State -> Int
sumState = S.foldr (+) 0


solution1 :: IO ()
solution1 = do
  (state, rules) <- getInput
  putStrLn . show . sumState $ runFor 20 rules state


solution2 :: IO ()
solution2 = do
  (state, rules) <- getInput

  let (loopStart, loopSize, offset) = go state rules M.empty 1
      (iters, remainder) = (50000000000 - loopStart) `divMod` loopSize

  putStrLn $ "Repeats every "
          ++ show loopSize
          ++ " from "
          ++ show loopStart
          ++ " with offset "
          ++ show offset

  putStrLn $ "Iterations: " ++ show iters
  putStrLn $ "Remainder: " ++ show remainder

  let res
        = sumState
        . runFor remainder rules
        . S.map (+ (offset * iters))
        $ runFor loopStart rules state

  putStrLn $ show res

  where
    go :: State -> [Rule] -> M.Map State (Int,Int) -> Int -> (Int,Int,Int)
    go state rules seen iter
      = let (offset, normState) = normalise state
            state' = stepGen rules state
        in  case M.lookup normState seen of
              Just (seenIter, seenOffset) ->
                (seenIter, iter - seenIter, offset - seenOffset)
              
              Nothing ->
                go state' rules (M.insert normState (iter, offset) seen) (iter + 1)

    normalise state
      = let min = S.findMin state
        in  (min, S.map (subtract min) state)
