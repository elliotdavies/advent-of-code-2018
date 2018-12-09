{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Day09.Day09 where

import qualified Data.List.PointedList.Circular as PL
import           Data.List                      (foldl')
import qualified Data.Map                       as Map
import           Data.Maybe                     (fromJust)


newtype Marble = Marble { unMarble :: Int }
  deriving (Show, Eq, Ord, Enum)

newtype Id = Id Int
  deriving (Show, Eq, Ord)

newtype Score = Score { unScore :: Int }
  deriving (Show, Eq, Ord, Num)

data GameState
  = GameState
    { _marbles    :: PL.PointedList Marble
    , _lastPlaced :: Marble
    , _scores     :: Map.Map Id Score
    }
  deriving Show


fromMarble (Marble m) = Score m


placeMarble :: Id -> Marble -> GameState -> GameState
placeMarble id m (GameState marbles last scores)
  = if unMarble m `mod` 23 == 0 then placeMult23 else placeNormal
  where
    placeNormal
      = let marbles' = PL.insertRight m $ PL.moveN 1 marbles
        in  GameState marbles' m scores

    placeMult23
      = let marbles'  = PL.moveN (-7) marbles
            removed   = PL._focus marbles'
            score     = fromMarble m + fromMarble removed
            scores'   = Map.insertWith (+) id score scores
        in  GameState (fromJust $ PL.deleteRight marbles') m scores'


playGame :: Int -> Marble -> GameState
playGame numPlayers end
  = foldl' (\game (id,m) -> placeMarble id m game) initGame moves
  where
    initGame = GameState (PL.singleton $ Marble 0) (Marble 0) Map.empty
    
    moves = zip (cycle $ Id <$> [1 .. numPlayers]) [(Marble 1) .. end]


highScore :: GameState -> Int
highScore = maximum . map (unScore . snd) . Map.assocs . _scores


solution1 :: IO ()
solution1 = do
  let game = playGame 439 (Marble 71307)
  putStrLn $ "Last marble: " ++ show (_lastPlaced game)
  putStrLn $ "High score: " ++ show (highScore game)


solution2 :: IO ()
solution2 = do
  let game = playGame 439 (Marble 7130700)
  putStrLn $ "Last marble: " ++ show (_lastPlaced game)
  putStrLn $ "High score: " ++ show (highScore game)

