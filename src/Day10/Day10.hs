module Day10.Day10 where

import Data.Char (isDigit)
import Data.Foldable (for_)
import qualified Data.Set as Set


type Coord    = (Int, Int)
type Velocity = (Int, Int)


getInput :: IO [(Coord, Velocity)]
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


step :: [(Coord, Velocity)] -> ([(Coord, Velocity)], Int, Int, Int, Int)
step = foldr go ([], 0, 0, 0, 0)
  where
    go (cs, vs) (acc, minX, maxX, minY, maxY)
      = let cs'@(x', y') = (fst cs + fst vs, snd cs + snd vs)
        in ((cs', vs):acc, min minX x', max maxX x', min minY y', max maxY y')


run :: [(Coord, Velocity)] -> Int -> Int -> ([(Coord, Velocity)], Int)
run = go 0
  where
    go secs xs xLen yLen
      = let (xs', minX, maxX, minY, maxY) = step xs
            xLen' = maxX - minX
            yLen' = maxY - minY
        in  if xLen' > xLen && yLen' > yLen then (xs, secs) else go (secs+1) xs' xLen' yLen'


printGrid :: [Coord] -> IO ()
printGrid cs = do
  let xs = map fst cs
      ys = map snd cs
      minX = minimum xs
      maxX = maximum xs
      minY = minimum ys
      maxY = maximum ys

      set = Set.fromList cs
  
  for_ [minY..maxY] $ \y -> do
    for_ [minX..maxX] $ \x -> printLine x y set
    putStrLn ""

  where
    printLine :: Int -> Int -> Set.Set Coord -> IO ()
    printLine x y cs
      = putStr $ if Set.member (x,y) cs then "#" else "."


getResult :: IO ([Coord], Int)
getResult = do
  input <- getInput

  let cs = map fst input
      xs = map fst cs
      ys = map snd cs
      minX = minimum xs
      maxX = maximum xs
      minY = minimum ys
      maxY = maximum ys

  let (csvs, t) = run input (maxX - minX) (maxY - minY)
  pure (map fst csvs, t)


solution1 :: IO ()
solution1 = getResult >>= printGrid . fst

solution2 :: IO ()
solution2 = getResult >>= putStrLn . show . snd
