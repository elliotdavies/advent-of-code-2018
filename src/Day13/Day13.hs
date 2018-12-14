module Day13.Day13 where

import           Control.Arrow   (first, second)
import           Data.List.Index (ifoldr)
import qualified Data.Map        as Map
import           Data.Maybe      (fromJust)


type Pos = (Int, Int)

incX = first (+1)
decX = first ((-)1)
incY = second (+1)
decY = second ((-)1)


data Direction
  = U
  | D
  | L
  | R
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
  = Cart Direction (Int, Int)
  deriving (Show)


getInput :: IO (Tracks, [Cart])
getInput = do
  ls <- lines <$> readFile "input.txt"
  pure $ ifoldr parseLine (Map.empty, []) ls
  where
    parseLine y l acc = ifoldr (parseTrack y) acc l

    parseTrack y x c (tracks, carts)
      = case c of
          '|'  -> ins Vertical
          '-'  -> ins Horizontal
          '\\' -> ins CurveBSlash
          '/'  -> ins CurveFSlash
          '+'  -> ins Intersection
          '>'  -> app R
          '<'  -> app L
          '^'  -> app U
          'v'  -> app D
          _    -> (tracks, carts)
      where
        ins t = (Map.insert (x,y) t tracks, carts)
        app d = (tracks, Cart d (x,y) : carts)


step :: Tracks -> [Cart] -> [Cart]
step tracks
  = foldr step' []
  where
    step' (Cart d pos) acc
      = let currentTrack = fromJust $ Map.lookup pos tracks
            (d', pos') = getNext currentTrack d pos
        in  Cart d' pos' : acc

    getNext t d pos
      = case (t, d) of
          (Vertical, U) -> (U, decY pos)
          (Vertical, D) -> (U, incY pos)
          (Horizontal, L) -> (L, decX pos)
          (Horizontal, R) -> (L, incX pos)
          (CurveBSlash, R) -> (D, incY pos)
          (CurveBSlash, U) -> (L, decX pos)
          (CurveFSlash, L) -> (D, incY pos)
          (CurveFSlash, U) -> (R, incX pos)
          (Intersection, _) -> (d, pos) -- TODO
          _ -> (d, pos)


solution1 :: IO ()
solution1 = do
  (tracks, carts) <- getInput
  putStrLn $ show carts

  let carts' = step tracks carts
  putStrLn $ show carts'

