module Day10.Day10 where

import Data.Char (isDigit)
import Data.Foldable (for_)
import qualified Data.Set as Set


type Coord    = (Int, Int)
type Velocity = (Int, Int)
type Point    = (Coord, Velocity)


getInput :: IO [Point]
getInput = do
  map parse <$> (lines <$> readFile "input.txt")
  where
    parse :: String -> (Coord, Velocity)
    parse s
      = let [x,y,vx,vy] = map readInt . words . map keep $ s
        in  ((x, y), (vx, vy))
      where
        keep c = if isDigit c || c == '-' then c else ' '

        readInt :: String -> Int
        readInt = read


step :: [Point] -> ([Point], Int, Int, Int, Int)
step = foldr go ([], 0, 0, 0, 0)
  where
    go (cs, vs) (ps, minX, maxX, minY, maxY)
      = let cs'@(x', y') = (fst cs + fst vs, snd cs + snd vs)
            ps' = (cs', vs) : ps
        in (ps', min minX x', max maxX x', min minY y', max maxY y')


run :: [Point] -> Int -> Int -> ([Point], Int)
run = go 0
  where
    go secs points xLen yLen
      = let (points', minX, maxX, minY, maxY) = step points
            xLen' = maxX - minX
            yLen' = maxY - minY
        in  if xLen' > xLen && yLen' > yLen
              then (points, secs)
              else go (secs + 1) points' xLen' yLen'


extremes :: [Coord] -> (Int, Int, Int, Int)
extremes cs
  = let xs = map fst cs
        ys = map snd cs
    in  (minimum xs, maximum xs, minimum ys, maximum ys)
  

printGrid :: [Coord] -> IO ()
printGrid cs = do
  let (minX, maxX, minY, maxY) = extremes cs
  
  for_ [minY..maxY] $ \y -> do
    for_ [minX..maxX] $ \x -> printLine x y (Set.fromList cs)
    putStrLn ""

  where
    printLine :: Int -> Int -> Set.Set Coord -> IO ()
    printLine x y cs
      = putStr $ if Set.member (x,y) cs then "#" else "."


getResult :: IO ([Coord], Int)
getResult = do
  input <- getInput

  let (minX, maxX, minY, maxY) = extremes $ map fst input
      (points, t) = run input (maxX - minX) (maxY - minY)

  pure (map fst points, t)


solution1 :: IO ()
solution1 = getResult >>= printGrid . fst


solution2 :: IO ()
solution2 = getResult >>= putStrLn . show . snd
