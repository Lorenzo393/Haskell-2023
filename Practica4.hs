module Practica4 where

import Data.List
import Data.Char (ord)

-- 1)
data Nat = Zero | Succ Nat deriving (Show)
-- El tipo de Succ es Succ :: Nat -> Nat

int2Nat:: Int->Nat
int2Nat 0 = Zero
int2Nat x = Succ(int2Nat (x-1))

nat2Int:: Nat->Int
nat2Int Zero = 0
nat2Int (Succ x) = 1 + nat2Int x
