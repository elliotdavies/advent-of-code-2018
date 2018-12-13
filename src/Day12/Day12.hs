module Day12.Day12 where

import qualified Data.Vector as V


data Rule = String :=> Char
  deriving (Show)


parseRule :: String -> Rule
parseRule s = let [a,_,b:_] = words s in a :=> b


rules :: [Rule]
rules
  = parseRule <$>
    [ "..... => ."
    , "#.... => ."
    , "..### => ."
    , "##..# => #"
    , ".###. => #"
    , "...## => ."
    , "#.#.. => ."
    , "..##. => ."
    , "##.#. => #"
    , "..#.. => ."
    , ".#... => #"
    , "##.## => ."
    , "....# => ."
    , ".#.#. => ."
    , "#..#. => #"
    , "#.### => ."
    , ".##.# => #"
    , ".#### => ."
    , ".#..# => ."
    , "####. => #"
    , "#...# => #"
    , ".#.## => #"
    , "#..## => ."
    , "..#.# => #"
    , "#.##. => ."
    , "###.. => ."
    , "##### => #"
    , "###.# => #"
    , "...#. => #"
    , "#.#.# => #"
    , ".##.. => ."
    , "##... => #"
    ]


extraBefore = 5
extraAfter = 25

input = "#.......##.###.#.#..##..##..#.#.###..###..##.#.#..##....#####..##.#.....########....#....##.#..##"

initialState
  = pad extraBefore V.++ V.fromList input V.++ pad extraAfter


getWindow :: Int -> V.Vector Char -> V.Vector Char
getWindow i v
  | i < 2
      = let diff = (2 - i)
        in  pad diff V.++ V.slice i (5 - diff) v
  | end > lastIdx
      = let diff = end - lastIdx
        in  V.slice start (5 - diff) v V.++ pad diff 
  | otherwise
      = V.slice start 5 v
  where
    lastIdx = V.length v - 1
    start   = i - 2
    end     = i + 2


pad :: Int -> V.Vector Char
pad n = V.replicate n '.'



applyRules :: V.Vector Char -> V.Vector Char
applyRules v
  = V.imap (\i _ -> applyRule . V.toList $ getWindow i v) v
  where
    applyRule window = foldr step '.' rules
      where
        step (r :=> res) c = if r == window then res else c



solution1 :: IO ()
solution1 = do
  --let res = foldr go initialState [1..20]

  putStrLn $ fmt 0 ++ " " ++ show initialState
  
  finalState <- foldl go (pure initialState) [1..20]
  let total = V.sum $ V.imap (\i c -> if c == '#' then i - extraBefore else 0) finalState
  putStrLn $ "Sum: " ++ show total
  pure mempty

  where
    go state i = do
      s <- state
      let s' = applyRules s
      putStrLn $ fmt i ++ " " ++ show s'
      pure s'
    
    fmt i = if i < 10 then " " ++ show i else show i






