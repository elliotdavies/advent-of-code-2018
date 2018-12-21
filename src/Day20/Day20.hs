module Day20.Day20 where

import qualified  Data.Set          as S
import            Linear            (V2(..))
import qualified  Text.Parsec       as P
import qualified  Text.Parsec.Char  as P.C


type Pos = V2 Int

type Parser = P.Parsec String Pos (S.Set Route)


data Route
  = Route Pos Pos
  deriving (Show, Ord, Eq)


parseRegex :: Parser
parseRegex
  = P.between (P.C.char '^') (P.C.char '$') parseSteps


parseSteps :: Parser
parseSteps
  = fmap S.unions . P.many $ P.try parseStep P.<|> parseBranch


parseStep :: Parser
parseStep = do
  pos <- P.getState
  dir <- P.C.oneOf "NESW"
  let pos' = case dir of
              'N' -> pos + V2  0 (-1)
              'E' -> pos + V2  1   0
              'S' -> pos + V2  0   1
              'W' -> pos + V2 (-1) 0
  P.setState pos'
  pure $ S.singleton (Route pos pos')


parseBranch :: Parser
parseBranch
  = P.between (P.C.char '(') (P.C.char ')') $ do
    pos <- P.getState
    fmap S.unions . (flip P.sepBy $ P.C.char '|') $ do
      P.setState pos
      parseSteps


farthestPos :: S.Set Route -> Int
farthestPos routes
  = go 0 S.empty (V2 0 0)
  where
    go dist seen pos
      = let nextSteps = findNext pos routes seen
        in  if null nextSteps
            then dist
            else maximum $ map (go (dist + 1) (S.insert pos seen)) nextSteps


findNext :: Pos -> S.Set Route -> S.Set Pos -> [Pos]
findNext pos routes seen
  = filter ((`S.member` routes) . Route pos)
  . filter (`S.notMember` seen)
  $ (+ pos)
  <$> [ V2 x y
      | x <- [-1 .. 1]
      , y <- [-1 .. 1]
      , not (x == 0 && y == 0)
      ]


solution1 :: IO ()
solution1 = do
  input <- readFile "input.txt"
  case P.runParser parseRegex (V2 0 0) "" input of
    Left e        -> print e
    Right routes  -> print $ farthestPos routes


findRooms :: S.Set Route -> [Int]
findRooms routes
  = go 0 S.empty (V2 0 0)
  where
    go dist seen pos
      = let nextSteps = findNext pos routes seen
        in  (dist :)
            . concat
            $ map (go (dist + 1) (S.insert pos seen)) nextSteps


solution2 :: IO ()
solution2 = do
  input <- readFile "input.txt"
  case P.runParser parseRegex (V2 0 0) "" input of
    Left e        -> print e
    Right routes  -> print . length . filter (>= 1000) $ findRooms routes

