module Day23.Day23 where

import Data.Char (isDigit)
import Data.List (foldl', sortOn)
import Linear (V3(..))


type Pos = V3 Int


data Bot
  = Bot { _pos :: Pos, _radius :: Int }
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
widestSignal = head . reverse . sortOn _radius


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


-- @TODO Find a way to optimise this
solution2 :: IO ()
solution2 = do
  bots <- getInput
 
  let ps = map _pos bots
      xs = map (\(V3 x _ _) -> x) ps 
      ys = map (\(V3 _ y _) -> y) ps 
      zs = map (\(V3 _ _ z) -> z) ps 
      coordsToTry = [ V3 x y z
                    | x <- [ minimum xs .. maximum xs ]
                    , y <- [ minimum ys .. maximum ys ]
                    , z <- [ minimum zs .. maximum zs ]
                    ]

  let origin = V3 0 0 0
      bestPos = snd $ foldl' (step bots) (0, origin) coordsToTry
  
  print bestPos
  print $ manhattanDistance bestPos origin 
  where
    step bots acc@(bestNumInRange, bestPos) pos
      = let n = numCanReach pos bots
        in  if n > bestNumInRange then (n, pos) else acc


numCanReach :: Pos -> [Bot] -> Int
numCanReach pos
  = length . filter (canReach pos)
  where
    canReach pos bot
      = manhattanDistance (_pos bot) pos <= _radius bot
