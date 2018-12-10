module Day10.Day10 where


type Coord    = (Int, Int)
type Velocity = (Int, Int)

getInput :: IO [(Coord, Velocity)]
getInput = do
  map parse <$> (lines <$> readFile "input.txt")
  where
    parse :: String -> (Coord, Velocity)
    parse s
      = let [_,x,y,_,vx,vy] = map (filter keep) . words $ s
        in  ((read x, read y), (read vx, read vy))
      where
        keep c = 
        readInt = (read :: Int)

