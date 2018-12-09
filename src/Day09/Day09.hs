{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Day09.Day09 where

import           Data.List   (elemIndex, sort)
import qualified Data.Map    as Map
import           Data.Maybe  (fromMaybe)


newtype Marble
  = Marble Int
  deriving (Show, Eq, Ord)

mVal (Marble m) = m
mNext (Marble m) = Marble $ m + 1

newtype Id
  = Id Int
  deriving (Show, Eq, Ord)

newtype Score
  = Score Int
  deriving (Show, Num, Ord, Eq)

mkScore (Marble m) = Score m

data GameState
  = GameState
      { _marbles    :: [Marble]
      , _current    :: Marble
      , _lastPlaced :: Marble
      , _scores     :: Map.Map Id Score
      }
  deriving (Show)

placeMarble :: Id -> Marble -> GameState -> GameState
placeMarble id m (GameState marbles current last scores)
  = if mVal m `mod` 23 == 0 then placeMult23 else placeNormal
  where
    placeNormal
      = let marbles' = insertClockwise 1 current m marbles
        in GameState marbles' m m scores

    placeMult23
      = let (removed, marbles') = removeCounterClockwise 7 current marbles
            score    = mkScore m + mkScore removed
            scores'  = Map.alter (Just . maybe score (+ score)) id scores 
            current' = findClockwise 0 removed marbles
        in  GameState marbles' current' m scores'


playGame :: Int -> Marble -> GameState
playGame numPlayers end
  = go initGameState (cycle $ Id <$> [1..numPlayers])
  where
    initGameState = GameState [Marble 0] (Marble 0) (Marble 0) Map.empty

    go :: GameState -> [Id] -> GameState
    go state (p:ps)
      = let m = _lastPlaced state
        in  if m == end
              then state
              else go (placeMarble p (mNext m) state) ps

solution1 :: IO ()
solution1 = do
  let game = playGame 439 (Marble 71307)
  putStrLn $ "Last marble: " ++ show (_lastPlaced game)
  putStr "High score: "
  putStrLn . show . head . reverse . sort . map snd . Map.assocs $ _scores game


pos :: Marble -> [Marble] -> Int
pos = (fromMaybe 0 .) . elemIndex


findClockwise :: Int -> Marble -> [Marble] -> Marble
findClockwise n current marbles
  = let idx = (pos current marbles + n + 1) `mod` length marbles
    in  head . snd $ splitAt idx marbles

insertClockwise :: Int -> Marble -> Marble -> [Marble] -> [Marble]
insertClockwise n current new marbles
  = let insertionIndex = (pos current marbles + n + 1) `mod` length marbles
    in  insert new insertionIndex marbles
  where
    insert m i ms
      = let (before, after) = splitAt i ms
        in  before ++ [m] ++ after

removeCounterClockwise :: Int -> Marble -> [Marble] -> (Marble, [Marble])
removeCounterClockwise n current marbles
  = let removalIndex = (pos current marbles - n) `mod` length marbles
    in  remove removalIndex marbles
  where
    remove i ms
      = let (before, after) = splitAt i ms
            removed = head after
        in  (removed, before ++ drop 1 after)
