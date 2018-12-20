module Day18.Day18 where

import           Data.List.Index (ifoldl)
import qualified Data.Map        as M
import           Data.Maybe      (mapMaybe)
import           Linear          (V2(..))


newtype Pos = Pos (V2 Int)
  deriving (Show, Eq)

instance Ord Pos where
  Pos (V2 x y) `compare` Pos (V2 x' y')
    = y `compare` y' <> x `compare` x'


adjacentAcres :: Grid -> Pos -> [Acre]
adjacentAcres g (Pos p)
  = mapMaybe (flip M.lookup g . Pos . (+p))
  $ [ V2   0  (-1)
    , V2   1  (-1)
    , V2   1    0
    , V2   1    1
    , V2   0    1
    , V2 (-1)   1
    , V2 (-1)   0
    , V2 (-1) (-1)
    ]


data Acre
  = Open
  | Lumberyard
  | Trees
  deriving (Eq, Ord, Show)


type Grid = M.Map Pos Acre


getInput :: IO Grid
getInput = do
  input <- lines <$> readFile "input.txt"
  pure $ ifoldl parseLine M.empty input
  where
    parseLine acc y line
      = M.union acc $ ifoldl (parseAcre y) M.empty line
    
    parseAcre y acc x acre
      = let pos = Pos (V2 x y)
        in  case acre of
              '.' -> M.insert pos Open       acc
              '|' -> M.insert pos Trees      acc
              '#' -> M.insert pos Lumberyard acc


tick :: Grid -> Grid
tick g = M.mapWithKey tickAcre g
  where
    tickAcre :: Pos -> Acre -> Acre
    tickAcre pos a
      = case a of
          Open ->
            if nextTo 3 Trees then Trees else Open
          
          Trees ->
            if nextTo 3 Lumberyard then Lumberyard else Trees

          Lumberyard ->
            if nextTo 1 Lumberyard && nextTo 1 Trees then Lumberyard else Open
        
      where
        nextTo n t = (>= n) . length . filter (== t) $ adjacentAcres g pos


resourceValue :: Grid -> Int
resourceValue g
  = findNum Trees * findNum Lumberyard
  where
    findNum t = length . filter (== t) $ M.elems g


solution1 :: IO ()
solution1 = do
  grid <- getInput
  let res = iterate tick grid !! 10
  print $ resourceValue res


solution2 :: IO ()
solution2 = do
  grid <- getInput

  let (loopStart, loopSize) = go grid M.empty 1
      remainder = (1000000000 - loopStart) `mod` loopSize


  putStrLn $ "Repeats every " ++ show loopSize ++ " from " ++ show loopStart
  putStrLn $ "Remainder: " ++ show remainder

  let res = iterate tick grid !! (loopStart + remainder)
  print $ resourceValue res

  where
    go :: Grid -> M.Map Grid Int -> Int -> (Int, Int)
    go g seen iter
      = case M.lookup g seen of
          Just seenIter -> (seenIter, iter - seenIter)
          Nothing       -> go g' (M.insert g iter seen) (iter + 1)
      where
        g' = tick g