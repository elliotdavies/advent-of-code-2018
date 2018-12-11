module Day11.Day11 where

import           Data.Foldable (traverse_)
import qualified Data.Vector   as V


input = 8772
gridSize = 300


cellPower :: Int -> Int -> Int
cellPower x y
  = let rackId = x + 10
    in  hundreds ((y * rackId + input) * rackId) - 5
  where
    hundreds i
      = case drop 2 . take 3 . reverse . show $ i of
          [] -> 0
          x  -> read x :: Int


grid :: V.Vector (V.Vector Int)
grid = V.generate gridSize genXs
  where
    genXs i = V.generate gridSize (cellPower i)


coords :: [(Int, Int)]
coords = [(x,y) | x <- [1..gridSize], y <- [1..gridSize]]


findSquare :: Int -> ((Int, Int), Int)
findSquare boxSize = foldr step ((0,0),0) coords
  where
    step (x,y) res
      | x + boxSize > gridSize || y + boxSize > gridSize = res
      | otherwise =
          let total
                = V.sum
                . V.map (V.sum . V.slice x boxSize)
                $ (V.slice y boxSize grid)
          
          in  if total > snd res then ((x,y), total) else res


solution1 :: IO ()
solution1 = putStrLn . show $ findSquare 3


-- Have to eyeball this one...
solution2 :: IO ()
solution2 = do
  traverse_ findSquare' [1..300]
  where
    findSquare' i = do
      let r = findSquare i
      putStrLn $ show (r, i)
      pure r

