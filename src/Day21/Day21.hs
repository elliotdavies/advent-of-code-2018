module Day21.Day21 where

import            Day16.Day16         (Registers, OpCode(..))
import qualified  Day16.Day16 as      D16
import            Day19.Day19 hiding  (solution1, solution2, runProgram)
import            Data.Foldable       (for_)
import qualified  Data.IntMap as      M
import qualified  Data.Vector as      V


runProgram :: Instructions -> State -> (State, Int)
runProgram instrs state
  = go instrs (state, 0)
  where
    go instrs (state@(State _ ipVal _), count)
      -- Line 30 is the first time register 0 is actually used, for an
      -- `eqrr 4 0 2`. So we need to look at register 4 at that point
      -- to tell us what value of register 0 will cause termination
      = if ipVal == 30 then (state, count)
        else
          case flip execute state <$> instrs V.!? ipVal of
            Just state' -> go instrs (state', count + 1)
            Nothing     -> (state, count)


solution1 :: IO ()
solution1 = do
  (state, instrs) <- getInput initRegisters
  let (State _ _ rs, count) = runProgram instrs state
  putStrLn $ "Register 4: " ++ show (rs M.! 4)
