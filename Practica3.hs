module Practica3 where

import Data.List
import Data.Char (ord)

-- 1)
data Color = Color {red::Int , green::Int , blue::Int} deriving (Show)

mezclar x y = Color (div (red x + red y) 2) (div (green x + green y) 2) (div (blue x + blue y) 2)
--mezclar:: Color->Color->Color

-- 2)
data Linea = Linea {cad::String , pos::Int} deriving(Show)

vacia:: Linea->Linea
vacia x = Linea "" 0
moverIzq:: Linea->Linea
moverIzq x = Linea (cad x) (pos x-1) 
moverDer:: Linea->Linea
moverDer x = Linea (cad x) (pos x+1)
moverIni:: Linea->Linea
moverIni x = Linea (cad x) 0
moverFin:: Linea->Linea
moverFin x = Linea (cad x) (length (cad x))
insertar:: Char->Linea->Linea
insertar c x = Linea (take (pos x) (cad x) ++ [c] ++ drop (pos x) (cad x)) (pos x+1)
borrar::Linea -> Linea
borrar x = Linea (take (pos x-1) (cad x) ++ drop (pos x) (cad x)) (pos x-1)

-- 3)
data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving(Show)

headCL (CUnit x) = x
headCL (Consnoc x xs y) = x

tailCL (CUnit x) = EmptyCL
tailCL (Consnoc x EmptyCL y) = (CUnit y)
tailCL (Consnoc x xs y) = (Consnoc (headCL xs) (tailCL(xs)) y)

isEmptyCL EmptyCL = True
isEmptyCL (CUnit x) = False
isEmptyCL (Consnoc x xs y) = False

isCUnit EmptyCL = False
isCUnit (CUnit x) = True
isCunit (Consnoc x xs y) = False

reverseCL EmptyCL = EmptyCL
reverseCL (CUnit x) = (CUnit x)
reverseCL (Consnoc x EmptyCL y) = (Consnoc y EmptyCL x)
reverseCL ( Consnoc x xs y) = (Consnoc y (reverseCL xs) x)

-- 4)
data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp deriving(Show)

eval:: Aexp->Int
eval (Num x) = x
eval (Prod x y) = (eval x) * (eval y)
eval (Div x y) = div (eval x) (eval y)
-- *** Exception: divide by zero

seval:: Aexp->Maybe Int
seval (Num 0) = Nothing
seval (Num x) = (Just x)
seval (Prod x y) = seval((Num (eval x * eval y)))
seval (Div x y) = seval((Num (div (eval x) (eval y))))
-- *** Exception: divide by zero

-- 5)
data BST a = H | N (BST a) a (BST a) deriving(Show)

maximun:: BST a -> a
maximun (N l x H) = x
maximun (N l x r) = maximun r 

--checkBST:: BST a -> Bool

--checkBST (N l x r) | x < (checkBST l) = False | x > (checkBST r) = False | otherwise = True