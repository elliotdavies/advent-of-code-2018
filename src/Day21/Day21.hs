{-# LANGUAGE BangPatterns #-}

module Day21.Day21 where

import            Day16.Day16         (Registers, OpCode(..))
import qualified  Day16.Day16 as      D16
import            Day19.Day19 hiding  (solution1, solution2, runProgram)
import            Data.Foldable       (for_)
import qualified  Data.IntMap as      M
import qualified  Data.Set    as      S
import qualified  Data.Vector as      V



solution1 :: IO ()
solution1 = do
  (state, instrs) <- getInput initRegisters
  let State _ _ rs = runProgram instrs state
  putStrLn $ "Register 4: " ++ show (rs M.! 4)
  where
    runProgram instrs state@(State _ ipVal _)
      -- Line 30 is the first time register 0 is actually used, for
      -- `eqrr 4 0 2`. So we can look at register 4 at that point
      -- to tell us what value of register 0 will cause termination
      = if ipVal == 30
          then  state
          else  case flip execute state <$> instrs V.!? ipVal of
                  Just state' -> runProgram instrs state'
                  Nothing     -> state


-- This should work but presumably isn't efficient enough :(
solution2 :: IO ()
solution2 = do
  (state, instrs) <- getInput initRegisters
  let res = runProgram instrs state (S.empty, mempty)
  putStrLn $ "Repeated value: " ++ show res
  where
    runProgram instrs !state@(State _ ipVal rs) !(seen, last)
      = if S.member rs seen
        then  last -- We want the last non-repeated value
        else  case flip execute state <$> instrs V.!? ipVal of
                Just state' ->
                  runProgram instrs state' (S.insert rs seen, rs)
                
                Nothing ->
                  error "Shouldn't happen"
