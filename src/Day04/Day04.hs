{-#LANGUAGE GeneralizedNewtypeDeriving #-}

module Day04.Day04 where

import           Data.List        (group, sort, sortOn)
import qualified Data.Map  as     Map
import           Prelude   hiding (id, min)


newtype Min = Min Int
  deriving (Show, Eq, Ord)

newtype Id = Id Int
  deriving (Show, Eq, Ord)

data Event
  = Start Id
  | Sleep Min
  | Wake Min


-- Guard ID mapped to the minutes they're asleep
type SleepMap = Map.Map Id [Min]


getInput :: IO SleepMap
getInput = do
  events <- sort . lines <$> readFile "input.txt"
  let (Start initialId) = parse $ head events -- Assume we always start with a Start
  pure . snd $ go (initialId, Map.empty) events
    where
    go :: (Id, SleepMap) -> [String] -> (Id, SleepMap)
    go acc [] = acc
    go (id, sleepMap) (s:s':ss)
      = case parse s of
          -- If we see a new guard starting a shift, change the current ID
          Start newId -> 
            go (newId, sleepMap) (s':ss)

          -- If a guard falls asleep, note the minute. Assume the next event will be
          -- the guard waking up, and look ahead
          Sleep (Min sleepMin) ->
            let Wake (Min wakeMin) = parse s'
                mins = Min <$> [sleepMin .. wakeMin - 1]
                sleepMap' = Map.alter (Just . maybe mins (++ mins)) id sleepMap
            in  go (id, sleepMap') ss

          -- Because the Sleep case looks ahead, this shouldn't happen
          Wake _ ->
            error "This shouldn't happen if the data is consistent"
    

-- I /still/ really need to learn to use a parsing library
parse :: String -> Event
parse s
  = let (_:hourMin:_:event:_) = words s
        min = Min (read (take 2 $ drop 3 hourMin) :: Int)
        (tag:rest) = event
    in  case tag of
          '#' -> Start $ Id (read rest :: Int)
          'u' -> Wake min
          'a' -> Sleep min


sleepiestMinute :: [Min] -> (Min, Int)
sleepiestMinute mins
  = let sleepiest = head . reverse . sortOn length . group . sort $ mins
    in  (head sleepiest, length sleepiest)


solution1 :: IO ()
solution1 = do
  input <- getInput
  let (Id id, mins)
        = head
        . reverse
        . sortOn (length . snd)
        $ Map.assocs input
      
      (Min min, _) = sleepiestMinute mins
  
  putStrLn $ "Sleepiest guard: " <> show id
  putStrLn $ "Sleepiest minute: " <> show min
  putStrLn $ "Solution: " <> show (min * id)


solution2 :: IO ()
solution2 = do
  input <- getInput
  let (Id id, (Min min, count))
        = head
        . reverse
        . sortOn (snd . snd)
        . Map.assocs
        $ Map.map sleepiestMinute input
      
  putStrLn $ "Guard: " <> show id
  putStrLn $ "Minute and count: " <> show min <> " " <> show count
  putStrLn $ "Solution: " <> show (min * id)

