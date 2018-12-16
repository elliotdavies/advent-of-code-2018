module Day16.Day16 where

import           Data.Bits       ((.&.), (.|.))
import qualified Data.IntMap     as M
import           Data.List       (sortOn)
import           Data.List.Index (indexed)


type Registers = M.IntMap Int

data OpCode
  = AddR
  | AddI
  | MulR
  | MulI
  | BAnR
  | BAnI
  | BOrR
  | BOrI
  | SetR
  | SetI
  | GtiR
  | GtrI
  | GtrR
  | EqiR
  | EqrI
  | EqrR
  deriving (Enum, Eq, Show)

allOpCodes :: [OpCode]
allOpCodes = [AddR .. EqrR]


data Instruction
  = Instruction Int Int Int Int
  deriving (Show)


execute :: Instruction -> OpCode -> Registers -> Registers
execute (Instruction _ a b c) code rs
  = ins $ case code of
      AddR -> _a + _b
      AddI -> _a + b
      MulR -> _a * _b
      MulI -> _a * b
      BAnR -> _a .&. _b
      BAnI -> _a .&. b
      BOrR -> _a .|. _b
      BOrI -> _a .|. b
      SetR -> _a
      SetI -> a
      GtiR -> if a > _b then 1 else 0
      GtrI -> if _a > b then 1 else 0
      GtrR -> if _a > _b then 1 else 0
      EqiR -> if a == _b then 1 else 0
      EqrI -> if _a == b then 1 else 0
      EqrR -> if _a == _b then 1 else 0
  where
    _a = rs M.! a
    _b = rs M.! b
    ins v = M.insert c v rs


matchingOpCodes :: Instruction -> Registers -> Registers -> [OpCode]
matchingOpCodes i rs rs' = foldr exec [] allOpCodes
  where
    exec code acc = if execute i code rs == rs' then code:acc else acc


getInputSamples :: IO [(Registers, Instruction, Registers)]
getInputSamples = do
  input <- readFile "input-pt1.txt"
  pure . map parseGroup . group [] . filter (/= "") . lines $ input
  where
    group acc [] = acc
    group acc xs = group (acc ++ [take 3 xs]) (drop 3 xs)

    parseGroup [rs,i,rs'] = (parseReg rs, parseInstr i, parseReg rs')
    
    parseReg
      = M.fromList
      . indexed
      . read
      . drop 2
      . dropWhile (/= ':')


getInputProgram :: IO [Instruction]
getInputProgram = do
  input <- readFile "input-pt2.txt"
  pure . map parseInstr $ lines input


parseInstr = mkInstruction . map read . words
  where
    mkInstruction [a,b,c,d] = Instruction a b c d


solution1 :: IO ()
solution1
  = getInputSamples
  >>= putStrLn
  . show
  . length
  . filter ((>= 3) . length)
  . map getMatches
  where
    getMatches (rs, i, rs') = matchingOpCodes i rs rs'


buildMapping :: [(Int, [OpCode])] -> M.IntMap OpCode
buildMapping
  = go M.empty . sortOn (length . snd)
  where
    go mapping matches
      = let mapping' = foldl step mapping matches
        in  if M.size mapping' == length allOpCodes
              then mapping'
              else go mapping' matches
    
    step m (i, codes)
      = let codes' = filter notInMap codes
        in  if length codes' == 1 then M.insert i (head codes') m
            else m
      where
        notInMap code = not $ code `elem` (M.elems m)


runProgram :: M.IntMap OpCode -> [Instruction] -> Registers
runProgram mapping = foldl step (M.fromList [(0,0),(1,0),(2,0),(3,0)])
  where
    step rs instr@(Instruction i _ _ _)
      = let code = mapping M.! i
        in  execute instr code rs


solution2 :: IO ()
solution2 = do
  matches <- map getMatches <$> getInputSamples
  program <- getInputProgram
  let mapping = buildMapping matches  
  putStrLn . show $ runProgram mapping program
  where
    getMatches (rs, instr@(Instruction i _ _ _), rs')
      = (i, matchingOpCodes instr rs rs')

