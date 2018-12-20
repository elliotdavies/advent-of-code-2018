{-# LANGUAGE LambdaCase #-}

module Day19.Day19 where

import            Day16.Day16   (Registers, OpCode(..))
import qualified  Day16.Day16   as D16
import qualified  Data.IntMap   as M
import qualified  Data.Vector   as V


data State
  = State Int Int Registers -- register ip is bound to; value of ip
  deriving (Show)


data Instruction
  = Instruction OpCode Int Int Int
  deriving (Show)


type Instructions = V.Vector Instruction


initRegisters :: Registers
initRegisters = M.fromList [(0,0),(1,0),(2,0),(3,0),(4,0),(5,0)]


getInput :: Registers -> IO (State, Instructions)
getInput rs = do
  (ip:instrs) <- lines <$> readFile "input.txt"
  let state = State (read $ words ip !! 1) 0 rs
      instructions = V.fromList $ map parseInstr instrs
  
  pure (state, instructions)
  where
    parseInstr s
      = let [code,a,b,c] = words s
        in  Instruction (parseOpCode code) (read a) (read b) (read c)
    
    parseOpCode = \case
      "addr" -> AddR
      "addi" -> AddI
      "mulr" -> MulR
      "muli" -> MulI
      "banr" -> BAnR
      "bani" -> BAnI
      "borr" -> BOrR
      "bori" -> BOrI
      "setr" -> SetR
      "seti" -> SetI
      "gtir" -> GtiR
      "gtri" -> GtrI
      "gtrr" -> GtrR
      "eqir" -> EqiR
      "eqri" -> EqrI
      "eqrr" -> EqrR


-- @TODO Refactor Day 16 so that the code reuse is nicer
execute :: Instruction -> State -> State
execute (Instruction code a b c) (State ipReg ipVal rs)
  = let rs' = D16.execute (D16.Instruction 0 a b c) code (M.insert ipReg ipVal rs)
        ipVal' = (rs' M.! ipReg) + 1
    in  State ipReg ipVal' rs'


runProgram :: Instructions -> State -> State
runProgram instrs state@(State _ ipVal _)
  = case flip execute state <$> instrs V.!? ipVal of
      Just state' -> runProgram instrs state'
      Nothing     -> state


solution1 :: IO ()
solution1 = do
  (state, instrs) <- getInput initRegisters
  let State _ _ rs = runProgram instrs state
  print $ rs M.! 0


solution2 :: IO ()
solution2 = do
  (state, instrs) <- getInput $ M.insert 0 1 initRegisters
  go instrs state
  -- print $ rs M.! 0
  where
    go :: Instructions -> State -> IO ()
    go instrs state@(State _ ipVal _) =
      case instrs V.!? ipVal of
        Just instr -> do
          putStrLn "-------------"
          putStrLn $ "Instr: " ++ show instr
          putStrLn $ "State:  " ++ show state

          let state' = optimise instr state

          -- let state' = execute instr state
          putStrLn $ "State': " ++ show state'

          go instrs state'
        
        Nothing -> pure mempty


optimise :: Instruction -> State -> State
optimise instr state@(State ipIdx ipVal rs)
  = case (instr, ipVal) of
      (Instruction MulR 5 2 4, 3) ->
        State ipIdx 3 (M.adjust (+1) 2 rs)

      _ -> execute instr state


-- Loops from (set 2 6 1) at idx 11 to (mulr 5 2 4) at idx 3
-- at same time reg #1 is reset from 10 to 2
-- during loop, (addi 2 1 2) causes reg #2 to increment