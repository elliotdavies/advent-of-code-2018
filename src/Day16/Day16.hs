module Day16.Day16 where

import qualified Data.IntMap as M


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
  deriving (Show)

data Instruction
  = Instruction Int Int Int Int

execute :: Instruction -> OpCode -> Registers -> Registers
execute (Instruction i a b c) code rs
  = case code of
      AddR -> M.insert c (_a + _b) rs
      AddI -> undefined
      MulR -> undefined
      MulI -> undefined
      BAnR -> undefined
      BAnI -> undefined
      BOrR -> undefined
      BOrI -> undefined
      SetR -> undefined
      SetI -> undefined
      GtiR -> undefined
      GtrI -> undefined
      GtrR -> undefined
      EqiR -> undefined
      EqrI -> undefined
      EqrR -> undefined
  where
    _a = rs M.! a
    _b = rs M.! b
    _c = rs M.! c
