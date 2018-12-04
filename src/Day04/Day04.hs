{-#LANGUAGE DerivingStrategies         #-}
{-#LANGUAGE GeneralizedNewtypeDeriving #-}

module Day04.Day04 where


import Data.List (group, sort, sortOn)
import qualified Data.Map as Map
import Prelude

newtype Min = Min Int
  deriving Show
  deriving newtype (Eq, Ord)

newtype Id = Id Int
  deriving Show
  deriving newtype (Eq, Ord)

data Event
  = Event EvType Id Min
  deriving Show

evType (Event ty _ _) = ty
evId (Event _ id _) = id
evMin (Event _ _ min) = min

data EvType
  = Start
  | Wake
  | Sleep
  deriving Show


getInput :: IO [Event]
getInput = do
  events <- sort . lines <$> readFile "input.txt"
  -- This is 'OK' assuming the first event is always "Guard <id> begins shift"
  pure . fst $ foldl go ([], undefined) events
    where
      go :: ([Event], Id) -> String -> ([Event], Id)
      go (events, lastId) s
        = let ev = parse s lastId
          in  (events ++ [ev], evId ev)


parse :: String -> Id -> Event
parse s lastId
  = let (ymd:hm:_:evStr:_) = words s
        min = Min (read (take 2 $ drop 3 hm) :: Int)
        (tag:idStr) = evStr
        id = if tag == '#' then Id (read idStr :: Int) else lastId
    in  case tag of
          '#' -> Event Start id min
          'u' -> Event Wake id min
          'a' -> Event Sleep id min


sumEvents :: [Event] -> Map.Map Id [Min]
sumEvents = go Map.empty
  where
    go :: Map.Map Id [Min] -> [Event] -> Map.Map Id [Min]
    go acc [] = acc
    go acc (ev:ev':evs)
      = case evType ev of
          Start ->
            go acc (ev':evs)

          Sleep ->
            let (Min sleepMin) = evMin ev
                (Min wakeMin) = evMin ev'
                mins = Min <$> [sleepMin .. wakeMin - 1]
                acc' = Map.alter (Just . maybe mins (++ mins)) (evId ev) acc
            in  go acc' evs

          Wake ->
            error "This shouldn't happen if the data is consistent"
          

sleepiestMinute :: [Min] -> (Min, Int)
sleepiestMinute mins
  = let sleepiest = head . reverse . sortOn length . group . sort $ mins
    in  (head sleepiest, length sleepiest)


solution1 :: IO ()
solution1 = do
  tallies <- sumEvents <$> getInput
  let (Id id, mins)
        = head
        . reverse
        . sortOn (length . snd)
        $ Map.assocs tallies
      
      (Min m, _) = sleepiestMinute mins
  
  putStrLn $ "Sleepiest guard: " <> show id
  putStrLn $ "Sleepiest minute: " <> show m
  putStrLn $ "Solution: " <> show (m * id)


solution2 :: IO ()
solution2 = do
  tallies <- sumEvents <$> getInput
  let (Id id, (Min m, count))
        = head
        . reverse
        . sortOn (snd . snd)
        . Map.assocs
        $ Map.map sleepiestMinute tallies
      
  putStrLn $ "Guard: " <> show id
  putStrLn $ "Minute and count: " <> show m <> " " <> show count
  putStrLn $ "Solution: " <> show (m * id)

