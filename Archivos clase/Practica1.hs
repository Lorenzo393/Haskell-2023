import Data.Char (ord)
import Data.List


-- PRACTICA 1)

reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

-- 2)
-- a) 
five x = 5
-- b)
apply f = f
-- c)
identidad x = x
-- d)
first x y = x
-- e)
--derive x = (derive (x+0.1) - derive x)/2*0.1
-- f)
sign x | x > 0 = 1 | x < 0 = -1 | otherwise = 0
-- g)
vabs x | x >= 0 = x | otherwise = -x
-- h)
pot x y = y ^ x
-- i)
xor x y | x /= y = True | otherwise = False
-- j)
max3 x y z | x >= y && x >= z = x | y >= x && y >= z = y | otherwise = z
-- k)
swap x y = (y,x)

--3)
bisiesto x | (mod x 4 == 0) && (mod (div x 100) 4 /= 0) = True | otherwise = False

-- 4)


-- 5)
-- a)
--divisors x = [ys| ys <- [1..x], mod x ys == 0]
-- b)
--matches x xs = [ys| ys <- xs, ys /= x]
-- c)
--cuadrupla x = [(a,b,c,d)| a <- [0..x] , b <- [0..x] , c <- [0..x] , d <- [0..x], (a^2) + (b^2) == (c^2) + (d^2)]
-- d)


-- 6)


-- 7)
-- a)
suma [] = 0
suma (x:xs) = x + suma xs
-- b)
alguno [] = False
alguno (x:xs) | x == True = True | otherwise = alguno xs
-- c)
--todos [] = False
--todos (x:xs) | x /= False = True | x == False = False |otherwise = todos xs
-- d)
codes [] = []
codes (x:xs) = ord x : codes xs
-- e)
restos a []     = []
restos a (x:xs) = mod x a : restos a xs
-- f)
cuadrados [] = []
cuadrados (x:xs) = x^2 : cuadrados xs
-- g)
longitudes [] = []
longitudes (x:xs) = length x : longitudes xs
-- h)
orden [] = []
orden ((x,y):xs) | x > (y*3) = (x,y) : orden xs | otherwise = orden xs
-- i)
pares [] = []
pares (x:xs) | mod x 2 == 0 = x : pares xs | otherwise = pares xs
-- j)
letras [] = []
letras (x:xs) | x >= 'a' && x <= 'z' = x : letras xs | x >= 'A' && x <= 'Z' = x : letras xs | otherwise = letras xs
-- k)
masde n [] = []
masde n (x:xs) | length x > n = x : masde n xs | otherwise = masde n xs

-- 8)
-- a)
suma' xs = foldr (+) 0 xs
-- b)
alguno' xs | (filter (\r -> r /= False) xs) == [] = False | otherwise = True
-- c)
todos' xs | (filter (\r -> r /= True) xs) == [] = True | otherwise = False
-- d)
codes' xs = map (ord) xs
-- e)
--restos' n xs = map (mod n n) xs 
-- f)
cuadrados' xs = map (^2) xs
-- g)
longitudes' xs = map (length) xs
-- h)
orden' xs = filter (\(x,y) -> x > (y*3)) xs
-- i)
pares' xs = filter even xs
-- j)
letras' xs = filter (\r -> elem r ['a'..'z'] || elem r ['A'..'Z']) xs
-- k)
--masde' n [] = []
--masde' n (x:xs) = masde' n (filter (\r -> n < (length x)) xs)


-- PRACTICA 2)
-- 1)
-- a)
borrartake xs = take (length xs -1) xs
--b)

-- c)
--serie [] = [[]]
--serie xs =     xs
-- d)
paresIguales w x y z | w == x && y == z = True | w == y && x == z = True | w == z && x == y = True | otherwise = False
-- e)
isosceles x y z | x == y = True | x == z = True | y == z = True | otherwise = False 
-- f)
-- dtror n xs = (drop n xs) ++ (take n xs)

-- g)
-- uptolc n m = [ys|ys <- [n..m]]

upto n m | n < m = [n..m] | otherwise = []
-- h)
cantLetras 0 c = []
cantLetras n c = c ++ cantLetras (n-1) c

eco [] = []
eco xs = eco (take (length xs-1) xs) ++ cantLetras (length xs) (drop (length xs-1) xs)

-- 2)
-- a)

-- b)

-- f)
numexp 0 y = []
numexp x y = y : numexp (x-1) y

expandir [] = []
expandir (x:xs) = numexp x x ++ expandir xs 



-- PRACTICA 3)
-- 1)
--type Color = (Int,Int,Int)

mezclar (a,b,c) (a2,b2,c2) = ((a+a2)/2,(b+b2)/2,(c+c2)/2)

-- 2)
type Linea = (Int,String)

vacia (n,xs) = (0,"")
moverIzq (n,xs) | n <= 0 = (n,xs) | otherwise = (n-1,xs)
moverDer (n,xs) | n >= length xs = (n,xs) | otherwise = (n+1,xs)
moverIni (n,xs) = (0,xs)
moverFin (n,xs) = (length xs , xs)
insertar c (n,xs) = (n+1 , take n xs ++ [c] ++ drop n xs)
borrar (n,xs) = (n-1,take (n-1) xs ++ drop n xs)

-- 3)
data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving (Show)

-- a)
headCL (CUnit a) = a
headCL (Consnoc x y z) = x

tailCL (CUnit a) = EmptyCL
tailCL (Consnoc x (EmptyCL) y) = CUnit y
tailCL (Consnoc x xs y) = Consnoc (headCL xs) (tailCL xs) y  

isEmptyCL EmptyCL = True
isEmptyCL (CUnit a) = False
isEmptyCL (Consnoc x y z) = False

isCUnit EmptyCL = False
isCUnit (CUnit a) = True
isCUnit (Consnoc x y z) = False

-- b)
reverseCL EmptyCL = EmptyCL
reverseCL (CUnit a) = CUnit a
reverseCL (Consnoc x xs z) = Consnoc z (reverseCL xs) x

-- c)

-- d)

-- e)

-- 4)
data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp deriving (Show)
-- a)




eval (Num x) = x
eval (Prod x y) = (eval x) * (eval y)
eval (Div x y) = div (eval x) (eval y)


-- 5)
data Bin a = Hoja | Nodo (Bin a) a (Bin a) deriving (Show)
-- BST

arbol = (Nodo (Nodo Hoja 4 Hoja) 5 (Nodo Hoja 6 Hoja))

minimum' (Nodo Hoja a r ) = a
minimum' (Nodo l a r ) = minimum' l

maximum' (Nodo l a Hoja) = a
maximum' (Nodo l a r) = maximum' r

member' n Hoja = False
member' n (Nodo l a r) | n == a = True | n < a = member' n l | n > a  = member' n r

inorder Hoja = []
inorder (Nodo l a r) = inorder l ++ [a] ++ inorder r

insert' n Hoja = (Nodo Hoja n Hoja) 
insert' n (Nodo l a r) | n <= a = Nodo (insert' n l) a r | n > a  = Nodo l a (insert' n r)
{-
checkBST E = True
checkBST (N E x E) = True
checkBST (N E x r) = checkBST r && (minumun r) > x
checkBST (N l x E) = checkBST l && (maximun l) <= x
checkBST (N l x r) =  checkBST l && checkBST r && (maximun l) <= x && (minimun r) > x
-}



{-
delete n (Nodo Hoja a Hoja) | n == a = Hoja
delete n (Nodo l a Hoja) | n == a = l
delete n (Nodo Hoja a r) | n == a = r
delete n (Nodo l a r) | n == a = let y = minimum' r in Nodo l y (delete y r)
delete n (Nodo l a r) | n < a = Nodo (delete n l) a r | n > a = Nodo l a (delete n r)
-}

-- PRACTICA 4)
-- 1)
data Nat = Cero | Succ Nat deriving (Show)

-- a)
--Succ tiene tipo Nat -> Nat
-- b)
int2Nat 0 = Cero
int2Nat x = Succ(int2Nat (x-1))
-- c)
sumaNatconv x y = int2Nat((nat2int x) + (nat2int y))

sumaNat x Cero = x
sumaNat x (Succ y) = sumaNat (Succ x) y
-- d)
nat2int Cero = 0
nat2int (Succ x) = 1 + nat2int x 

-- 2)
--data Arb = E | H Int | N Arb Arb deriving (Show)
--data Cmd = LEFT | RIGHT deriving (Show)

--arb = (N (N (H 3) (H 4)) (H 5)) 

-- a)
-- N es de tipo Arb -> Arb -> Arb
-- b)
--select
--select x y | x == LEFT = select | x == RIGHT =


{-
-- RBT)

data Color = R | B deriving (Show)
data RBT a = E | T Color (RBT a) a (RBT a) deriving (Show)

rbtarbol = (T B (T R E 4 E) 5(T R E 6 E))

memberRBT n E = False
memberRBT n (T c l a r) | n == a = True | n < a = memberRBT n l | n > a = memberRBT n r

insertRBT n E = (T R E n E)
insertRBT n (T c l a r) | n < a = T c (insertRBT n l) a r | n > a = T c l a (insertRBT n r)

makeBlack E = E
makeBlack (T c l a r) = (T B l a r)

balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance c l a r = T c l a r

listoelem [x] = x

givelement 1 (x:xs) = x
givelement n (x:xs) = givelement (n-1) xs

--fromOrdList' [] = E
--fromOrdList' xs = (div length xs 2)(T B (fromOrdList' (take (div (length xs) 2) xs)) (xs !! (div (length xs) 2) xs) (fromOrdList' (drop (div (length xs) 2) xs)))
-}

{-
-- HEAPS)
data HEAP a = V | H (HEAP a) a (HEAP a) deriving (Show)

harbol = (H (H V 2 V) 1 (H V 3 V))

findMin E = 0
findmin (H l a r) = a


--deleteMin (H l a r) | deleteMin l

--insert
--deleteMin

--LEFTIST HEAPS)
type Rank = Int
data Heap a = E | N Rank a (Heap a) (Heap a) deriving (Show)

lharbol = (N 1 5 (N 0 6 E E) (N 0 7 E E))

rank E = 0
rank (N rango a r l) = rango

makeH x a b | rank a >= rank b = N (rankb+1) x a b | otherwise = N (rank a+1) x b a


merge z1 E = z1
merge E z2 = z2
merge z1@(N ra x l r) z2@(N ra2 y l2 r2) | x <= y = makeH x l (merge r z2) | otherwise = makeH y l2 (merge z1 r2)


-- merge
--insert
--findmin
--deletemin
-}



-- 10)
