module Day23.Day23 where

import Data.Char (isDigit)
import Data.List (sortOn)
import Linear (V3(..))


type Pos = V3 Int


data Bot
  = Bot Pos Int
  deriving (Eq, Show)


getInput :: IO [Bot]
getInput = do
  input <- (map (filter keep) . lines) <$> readFile "input.txt"
  pure $ map (mkBot . parse []) input
  where
    keep c = isDigit c || c == ',' || c == '-'

    parse acc "" = acc
    parse acc s
      = let (n, rest) = break (== ',') s
        in  read n : parse acc (drop 1 rest)

    mkBot [x,y,z,r] = Bot (V3 x y z) r


widestSignal :: [Bot] -> Bot
widestSignal = head . reverse . sortOn radius
  where
    radius (Bot _ r) = r


numInRange :: [Bot] -> Bot -> Int
numInRange bots (Bot srcPos srcR)
  = length $ foldr step [] bots
  where
    step bot@(Bot pos _) acc
      = if manhattanDistance srcPos pos <= srcR then bot : acc else acc


manhattanDistance a b
  = sumComponents $ abs (a - b)
  where
    sumComponents (V3 x y z) = x + y + z


solution1 :: IO ()
solution1 = do
  bots <- getInput
  let strongest = widestSignal bots
  print $ numInRange bots strongest
