module Day15.Day15 where

import Data.List.Index (ifoldl)
import qualified Data.Map as M
import Data.Maybe (isNothing)
import qualified Data.Set as S
import qualified Data.Tree as T
import Linear (V2(..))


type Pos = V2 Int

data UnitType
  = Elf
  | Goblin
  deriving (Show)

newtype AP
  = AP Int
  deriving (Show)

newtype HP
  = HP Int
  deriving (Show)

data Unit
  = Unit UnitType AP HP
  deriving (Show)

data Object
  = Wall
  | OUnit Unit
  deriving (Show)

type Grid = M.Map Pos Object


adjacentSquares :: Pos -> [Pos]
adjacentSquares p
  = [ p + V2 1 0
    , p - V2 1 0
    , p + V2 0 1
    , p - V2 0 1
    ]


free :: Grid -> Pos -> Bool
free g p = isNothing $ M.lookup p g


freeAdjacentSquares :: Grid -> Pos -> [Pos]
freeAdjacentSquares g = filter (free g) . adjacentSquares


buildPathTree :: Grid -> Pos -> Pos -> T.Tree Pos
buildPathTree g s d
  = T.unfoldTree step (s, S.singleton s)
  where
    step :: (Pos, S.Set Pos) -> (Pos, [(Pos, S.Set Pos)])
    step (p, seen)
      = if p == d then (d, [])
        else
          let adj = freeAdjacentSquares g p
              notSeen = filter (not . (flip S.member) seen) adj
              seen' = S.insert p seen
          in  (p, map (\pos -> (pos, seen')) notSeen)


type Route = [Pos]


foldTree :: T.Tree Pos -> [Route]
foldTree (T.Node pos children)
  = if null children
      then [[pos]]
      else map (pos :) $ concat $ map foldTree children


findPaths :: Grid -> Pos -> Pos -> [Route]
findPaths g s d
  = filter endsAtDest $ foldTree $ buildPathTree g s d
  where
    endsAtDest :: Route -> Bool
    endsAtDest r = not (null r) && last r == d


parseLine :: String -> Int -> Grid
parseLine s y = ifoldl parse M.empty s
  where
    parse m x c
      = case c of
          '#' -> ins Wall
          'G' -> ins $ mkUnit Goblin
          'E' -> ins $ mkUnit Elf
          _   -> m
      where
        ins unit = M.insert (V2 x y) unit m
        mkUnit u = OUnit $ Unit u (AP 3) (HP 200)


parseInput :: String -> Grid
parseInput = ifoldl parse M.empty . lines
  where
    parse m y line = M.union m $ parseLine line y


getInput :: IO Grid
getInput = parseInput <$> readFile "test.txt"
