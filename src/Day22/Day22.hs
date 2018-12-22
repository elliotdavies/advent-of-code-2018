{-# LANGUAGE LambdaCase #-}

module Day22.Day22 where

import           Data.List (sort)
import qualified Data.Map        as M
import           Linear          (V2(..))


newtype Pos = Pos (V2 Int)
  deriving (Show, Eq)

instance Ord Pos where
  Pos (V2 x y) `compare` Pos (V2 x' y')
    = y `compare` y' <> x `compare` x'


data RegionType
  = Rocky
  | Wet
  | Narrow
  deriving (Enum, Show)


type Grid = M.Map Pos (Int, Int, RegionType)


depth :: Int
depth = 11739


target :: Pos
target = Pos $ V2 11 718


genGrid :: Grid
genGrid
  = let Pos (V2 targetX targetY) = target
        ps = Pos <$>
              [ V2 x y
              | x <- [0 .. targetX]
              , y <- [0 .. targetY]
              ]
    in foldl makeRegion M.empty (sort ps)
  where
    makeRegion :: Grid -> Pos -> Grid
    makeRegion g pos
      = let idx = geologicIndex g pos
            el  = erosionLevel idx
            rt  = regionType el
          in  M.insert pos (idx, el, rt) g


geologicIndex :: Grid -> Pos -> Int
geologicIndex g pos@(Pos (V2 x y))
  | x == 0 && y == 0  = 0
  | pos == target     = 0
  | y == 0            = x * 16807
  | x == 0            = y * 48271
  | otherwise =
      let (_, el1, _) = g M.! (Pos (V2 (x-1) y))
          (_, el2, _) = g M.! (Pos (V2 x (y-1)))
      in  el1 * el2


erosionLevel :: Int -> Int
erosionLevel idx = (idx + depth) `mod` 20183


regionType :: Int -> RegionType
regionType el = [Rocky ..] !! (el `mod` 3)


solution1 :: IO ()
solution1 = do
  let grid = genGrid
  print $ riskLevel grid
  where
    riskLevel = sum . M.elems . M.map (\(_, _, rt) -> risk rt)

    risk = \case
      Rocky   -> 0
      Wet     -> 1
      Narrow  -> 2
