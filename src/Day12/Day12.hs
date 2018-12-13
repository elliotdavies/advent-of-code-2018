module Day12.Day12 where

import           Data.Char   (intToDigit)
import qualified Data.Vector as V


data Rule = String :=> Char
  deriving (Show)


data Pot = Pot Int Char
  deriving (Show)


parseRule :: String -> Rule
parseRule s = let [a,_,b:_] = words s in a :=> b


rules :: [Rule]
rules
  = parseRule <$>
    [ "...## => #"
    , "..#.. => #"
    , ".#... => #"
    , ".#.#. => #"
    , ".#.## => #"
    , ".##.. => #"
    , ".#### => #"
    , "#.#.# => #"
    , "#.### => #"
    , "##.#. => #"
    , "##.## => #"
    , "###.. => #"
    , "###.# => #"
    , "####. => #"
    ]


initialState :: V.Vector Pot
initialState
  = V.imap Pot $ V.fromList "#..#.#..##......###...###..........."


getWindow :: Int -> V.Vector Pot -> V.Vector Pot
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

    pad n = V.replicate n (Pot (-3) '.') -- TODO



applyRules :: V.Vector Pot -> V.Vector Pot
applyRules v
  = V.imap (\i pot -> applyRule pot . windowToList $ getWindow i v) v
  where
    windowToList = V.toList . V.map (\(Pot i c) -> c)

    applyRule (Pot i c) window = Pot i $ foldr step c rules
      where
        step (r :=> res) c = if r == window then res else c



solution1 :: IO ()
solution1 = do
  --let res = foldr go initialState [1..20]

  putStrLn $ "   " ++ show (V.imap (\i _ -> intToDigit $ i `mod` 10) initialState)
  putStrLn $ printPots 0 initialState
  
  foldl go (pure initialState) [1..20]
  
  pure mempty

  where
    go state i = do
      s <- state
      let s' = applyRules s
      putStrLn $ printPots i s'
      pure s'

    printPots i ps = fmt i ++ " " ++ show (V.imap (\_ (Pot _ c) -> c) ps)
    
    fmt i = if i < 10 then " " ++ show i else show i

