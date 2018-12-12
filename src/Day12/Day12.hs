module Day12.Day12 where


import qualified Data.Vector as V

{-
class Queue q where
  empty :: q a
  push  :: a -> q a -> q a
  pop   :: q a -> Maybe (a, q a)
  peek  :: q a -> Maybe a

newtype LIFO a = LIFO [a]

instance Queue LIFO where
  empty = LIFO []

  push x (LIFO xs) = LIFO (x:xs)

  pop (LIFO [])     = Nothing
  pop (LIFO (x:xs)) = Just (x, LIFO xs)

  peek (LIFO [])     = Nothing
  peek (LIFO (x:xs)) = Just x
-}

-----

data Rule
  = String :=> Char
  deriving (Show)


parseRule :: String -> Rule
parseRule s = let [a,_,b:_] = words s in a :=> b


initialState = V.fromList "#..##...."


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


-- Apply a rule to a list of 5 characters
applyRule :: [Char] -> [Char]
applyRule cs
  = tryMatch rules
  where
    tryMatch :: [Rule] -> [Char]
    tryMatch [] = cs
    tryMatch ((r :=> res) : rules)
      = if r == cs then ".." ++ [res] ++ ".." else tryMatch rules
