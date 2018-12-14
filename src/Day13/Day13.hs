module Day13.Day13 where

import           Control.Arrow   (first, second)
import           Data.List       (sort)
import           Data.List.Index (ifoldr)
import qualified Data.Map        as Map
import           Data.Maybe      (fromJust)


type Pos = (Int, Int)

incX = first (+1)
decX = first (subtract 1)
incY = second (+1)
decY = second (subtract 1)


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
  = Map.Map (Int, Int) Track


data Cart
  = Cart Direction Turn (Int, Int)
  deriving (Show)

instance Eq Cart where
  (Cart _ _ a) == (Cart _ _ b) = a == b

instance Ord Cart where
  (Cart _ _ a) `compare` (Cart _ _ b) = a `compare` b


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
        ins t = Map.insert (x,y) t tracks
        app d t = (ins t, Cart d TLeft (x,y) : carts)


step :: Tracks -> [Cart] -> [Cart]
step tracks
  = foldr step' []
  where
    step' (Cart d turn pos) acc
      = let currentTrack = fromJust $ Map.lookup pos tracks
            (d', turn', pos') = getNext currentTrack d turn pos
        in  Cart d' turn' pos' : acc

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



findCollisions :: [Cart] -> ([Pos], [Cart])
findCollisions = go ([], []) . sort
  where
    go res [] = res
    go (ps, cs) [c] = (ps, c:cs)
    go (ps, cs) (c:c':rest)
      = if c == c'
          then let Cart _ _ pos = c in go (pos:ps, cs) rest
          else go (ps, c:cs) (c':rest)


solution1 :: IO ()
solution1 = do
  (tracks, carts) <- getInput
  let findCollisionsPos = go tracks carts
  putStrLn $ show findCollisionsPos
  where
  go ts cs
    = let cs'     = step ts cs
          (ps, _) = findCollisions cs'
      in  if null ps
            then go ts cs'
            else head ps


solution2 :: IO ()
solution2 = do
  (tracks, carts) <- getInput
  let Cart _ _ pos = go tracks carts
  putStrLn $ show pos
  where
    go ts cs
      = let (ps, cs') = findCollisions $ step ts cs
        in  if length cs' == 1
              then head cs'
              else go ts cs'

