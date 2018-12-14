module Day13.Day13 where

import           Control.Arrow   (first, second)
import           Data.List       (sortBy)
import           Data.List.Index (ifoldr)
import qualified Data.Map        as Map
import           Data.Maybe      (fromJust)


newtype Pos = Pos (Int, Int)
  deriving (Show, Eq, Ord)


sortByPos :: [Cart] -> [Cart]
sortByPos = sortBy comp
  where
    (Cart _ _ pos) `comp` (Cart _ _ pos') = pos `compare` pos'


sortByGrid :: [Cart] -> [Cart]
sortByGrid = sortBy comp
  where
    (Cart _ _ pos) `comp` (Cart _ _ pos') = gridSort pos pos'


gridSort :: Pos -> Pos -> Ordering
gridSort (Pos (x,y)) (Pos (x',y'))
  = case y `compare` y' of
      EQ -> x `compare` x'
      GT -> GT
      LT -> LT



alterPos :: ((Int, Int) -> (Int, Int)) -> Pos -> Pos
alterPos f (Pos p) = Pos $ f p

incX = alterPos $ first (+1)
decX = alterPos $ first (subtract 1)
incY = alterPos $ second (+1)
decY = alterPos $ second (subtract 1)


data Direction
  = U
  | D
  | L
  | R
  deriving (Show)


data Turn
  = TLeft
  | TStraight
  | TRight
  deriving (Show)


data Track
  = Vertical      -- |
  | Horizontal    -- -
  | CurveBSlash   -- \
  | CurveFSlash   -- /
  | Intersection  -- +
  deriving (Show)


type Tracks
  = Map.Map Pos Track


data Cart
  = Cart Direction Turn Pos
  deriving (Show)

instance Eq Cart where
  (Cart _ _ pos) == (Cart _ _ pos') = pos == pos'


getPos :: Cart -> Pos
getPos (Cart _ _ pos) = pos


getInput :: IO (Tracks, [Cart])
getInput = do
  ls <- lines <$> readFile "input.txt"
  pure $ ifoldr parseLine (Map.empty, []) ls
  where
    parseLine y l acc = ifoldr (parseTrack y) acc l

    parseTrack y x c (tracks, carts)
      = case c of
          '|'  -> (ins Vertical, carts)
          '-'  -> (ins Horizontal, carts)
          '\\' -> (ins CurveBSlash, carts)
          '/'  -> (ins CurveFSlash, carts)
          '+'  -> (ins Intersection, carts)
          '>'  -> app R Horizontal
          '<'  -> app L Horizontal
          '^'  -> app U Vertical
          'v'  -> app D Vertical
          _    -> (tracks, carts)
      where
        ins t = Map.insert (Pos (x,y)) t tracks
        app d t = (ins t, Cart d TLeft (Pos (x,y)) : carts)


stepCart :: Tracks -> Cart -> Cart
stepCart tracks (Cart d turn pos)
  = let currentTrack = fromJust $ Map.lookup pos tracks
        (d', turn', pos') = getNext currentTrack d turn pos
    in  Cart d' turn' pos'
  where
    getNext track d turn pos
      = case (track, d) of
          (Vertical, U)     -> (U, turn, decY pos)
          (Vertical, D)     -> (D, turn, incY pos)
          (Horizontal, L)   -> (L, turn, decX pos)
          (Horizontal, R)   -> (R, turn, incX pos)
          (CurveBSlash, R)  -> (D, turn, incY pos)
          (CurveBSlash, U)  -> (L, turn, decX pos)
          (CurveBSlash, L)  -> (U, turn, decY pos)
          (CurveBSlash, D)  -> (R, turn, incX pos)
          (CurveFSlash, L)  -> (D, turn, incY pos)
          (CurveFSlash, U)  -> (R, turn, incX pos)
          (CurveFSlash, R)  -> (U, turn, decY pos)
          (CurveFSlash, D)  -> (L, turn, decX pos)
          (Intersection, _) -> let (turn', d', pos') = handleIntersection turn d pos in (d', turn', pos')
          _                 -> (d, turn, pos)

    handleIntersection turn d pos
      = case (turn, d) of
          (TLeft, U) -> (TStraight, L, decX pos)
          (TLeft, R) -> (TStraight, U, decY pos)
          (TLeft, D) -> (TStraight, R, incX pos)
          (TLeft, L) -> (TStraight, D, incY pos)
          (TStraight, U) -> (TRight, U, decY pos)
          (TStraight, R) -> (TRight, R, incX pos)
          (TStraight, D) -> (TRight, D, incY pos)
          (TStraight, L) -> (TRight, L, decX pos)
          (TRight, U) -> (TLeft, R, incX pos)
          (TRight, R) -> (TLeft, D, incY pos)
          (TRight, D) -> (TLeft, L, decX pos)
          (TRight, L) -> (TLeft, U, decY pos)


step :: Tracks -> [Cart] -> ([Pos], [Cart])
step tracks =
  foldl go ([], []) . sortByPos
  where
    go (colls, cs) c
      = let c' = stepCart tracks c
            pos = getPos c'
        in  if pos `elem` map getPos cs
              then (pos:colls, filter ((/= pos) . getPos) cs)
              else (colls, c':cs)


solution1 :: IO ()
solution1 = do
  (tracks, carts) <- getInput
  let findCollisionsPos = go tracks carts
  putStrLn $ show findCollisionsPos
  where
  go ts cs
    = let (colls, cs') = step ts cs
      in  if null colls
            then go ts cs'
            else head colls


solution2 :: IO ()
solution2 = do
  (tracks, carts) <- getInput
  cart <- go tracks carts
  putStrLn . show $ getPos cart
  where
    go ts cs
      = let (colls, cs') = step ts cs
        in do
          putStrLn $ show cs'
          if length cs' == 1
              then pure $ head cs'
              else go ts cs'

