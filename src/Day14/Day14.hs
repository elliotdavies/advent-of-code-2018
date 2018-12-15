module Day14.Day14 where

import           Data.List     (isPrefixOf, tails)
import qualified Data.Sequence as Seq


genScores :: [Int]
genScores = [0, 1] ++ go 0 1 (Seq.fromList [3,7])
  where
    go :: Int -> Int -> Seq.Seq Int -> [Int]
    go idx1 idx2 scores
      = let score1 = Seq.index scores idx1
            score2 = Seq.index scores idx2
            digits = toDigits $ score1 + score2
            scores' = scores Seq.>< (Seq.fromList digits)
            idx1' = (1 + idx1 + score1) `mod` Seq.length scores'
            idx2' = (1 + idx2 + score2) `mod` Seq.length scores'
        in  digits ++ go idx1' idx2' scores'


toDigits :: Int -> [Int]
toDigits i
  = let (a,b) = i `divMod` 10
    in  if a == 0 then [b] else [a,b]


solution1 :: IO ()
solution1
  = putStrLn . show . take 10 . drop 990941 $ genScores


solution2 :: IO ()
solution2
  = putStrLn
  . show
  . length
  . takeWhile (not . ([9,9,0,9,4,1] `isPrefixOf`))
  . tails
  $ genScores
