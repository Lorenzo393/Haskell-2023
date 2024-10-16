import qualified Data.Map as Map
import qualified Data.Set as S
import Data.Char (isAlpha)
import Data.Char (ord)
import GHC.Exts (IsList(..))
import Data.List
import Data.List (nub, sort)

--3)
data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving (Show)


headCL (CUnit a) = a
headCL (Consnoc x xs z) = x

tailCL (CUnit a) = EmptyCL
tailCL (Consnoc x EmptyCL z) = (CUnit z)
tailCL (Consnoc x xs z) = (Consnoc (headCL xs) (tailCL xs) z)

isEmptyCL EmptyCL = True
isEmptyCL (CUnit a) = False
isEmptyCL (Consnoc x xs z) = False

isCUnit EmptyCL = False
isCUnit (CUnit a) = True
isCUnit (Consnoc x xs z) = False

reverseCL EmptyCL = EmptyCL
reverseCL (CUnit a) = (CUnit a)
reverseCL (Consnoc x EmptyCL z) = (Consnoc z EmptyCL x)
reverseCL (Consnoc x xs z) = (Consnoc z (reverseCL xs) x)

--concatCL (Consnoc a b c) (Consnoc x y z) = Consnoc

--4) 
data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp deriving (Show)

eval (Num a) = a
eval (Prod (Num a) (Num b)) = a*b
eval (Div (Num a) (Num b)) = div a b

data Nat = Zero | Succ Nat deriving (Show)

int2Nat 0 = Zero
int2Nat x = Succ(int2Nat (x-1))

nat2Int Zero = 0
nat2Int (Succ x) = 1+ nat2Int x 



--PRUEBA)
--1)
--a)
--insertar u [] = [(u,1)]
--insertar u ((x,y):xs) = foldr (\(x,y) acc -> if u == x then [(x,y+1)]++acc else [(x,y)]++acc) [] xs

insertar u [] = [(u, 1)]
insertar u xs = foldr (\(x, y) acc -> if x == u then (x, y + 1) : acc else (x, y) : acc) [] xs
--b)
menosFrec [(a,b)] = a 
menosFrec ((a,b):(c,d):xs) | b < d = menosFrec ((a,b):xs) | otherwise = menosFrec ((c,d):xs) 

--c)
cant a 0 = []
cant a b = a:cant a (b-1)

expandir' [] = []
expandir' ((x,y):xs) = cant x y ++ expandir' xs

expandirXD xs = [x|(x,k)<-xs,n<-[1..k]]

--2)
type Ocurrencia = Int
data Bolsa a = E | Elem Ocurrencia a (Bolsa a) deriving (Show)
--a)
bolsa = (Elem 2 1 (Elem 1 2 (Elem 3 3 (E))))
--b)
--Elem:: Ocurrencia -> a -> Bolsa a -> Bolsa a
--c)
bol2list E = []
bol2list (Elem 0 b c) = bol2list c
bol2list (Elem a b c) = b:bol2list(Elem (a-1) b c) 
--3)
--a)
plie f e [] = e
plie f e (x:xs) = plie f (f e x) xs
--plie:: (b -> a -> b) -> b -> [a] -> b
--b)
rota f x y z = (x y) 'f' z
--rota::(a -> b -> c) -> (b -> d -> e) -> a -> b -> d -> c
--4)
--a)
{-
Las invariantes que debe cumplir RBT son primero la invariante de BST y luego 
otras 2 mas, estas son primero que todo nodo rojo tiene padre negro(Local) y
tambien que todos los caminos desde la raiz a cualquiera de las hojas tiene
la misma cantidad de nodos negros(Global), altura negra;
-}
--b)
{-
Balance soluciona la invariante local, es decir soluciona la primera invariante
-}
--c)
