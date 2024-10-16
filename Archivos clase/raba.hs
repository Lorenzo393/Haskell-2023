{-
3)
data CList = EmptyCL | CUnit a | Consnoc a(CList a)

[] -> EmptyCL
[2] -> CUnit 2
[1,2] -> Consnoc 1 Empty 2
[1,2,3] -> Consnoc 1 (CUnit 2) 3
['a','b','c','d'] -> Consnoc 'a' (Consnoc 'b' Empty 'c') 'd'
 
headCL (CUnit x) = x
headCL (Consnoc x xs y) = x

tailCl (CUnit) = EmptyCL
tailCL (Consnoc x EmptyCL y) = CUnit y
tailCL (Consnoc x xs ys) = Consnoc (headCL xs)
				   (tailCL xs)
				   y

reverseCL EmptyCL = EmptyCL
reverseCL (CUnit x) = CUnit x
reverseCL (Consnoc x xs y) = Consnoc y (reverse xs) x
---------------------------------------------------------------
balanceado :: int -> a -> Tree a 

:: toma
-> devuiekve

balanceado 0 _ = E
balanceado n x| even(n-1) =
				let m = div(n-1) 2
				t = balanceado m
				in N t x t
		   | otherwise = 
				let m = div(n-1) 2
				in N (balanceado (m+1)x)
				x
				(balanceado m x)

TUPPLING

	= let m = div(n-1) 2
	(t1,t2) = (balanceado' m x)
	in N t1 x t2
where
	balanceado' m x = (balanceado(m+1) x, balanceado m x)

[3,5,8,11,15,17,20,21] -> lista ordenada a balanceado

si length es impar div x 2 = bien

si length par de la lista xs = 8
			    4 = div 8 2
			   15 = xs !! 4

!! devuelve el entero de esa posicion

anteriores al 15 take 4 xs = [3,5,8,11]
posteriores al 15 drop (4+1) xs

zs = [3,5,8,11] = take 4 xs
ys = [17,20,21] = drop (4+1) xs

(N zs 15 ys)

si length es impar div x = bien

fromOrdList [] = E
fromOrdList xs = let m = length xs
			      m = div n 2
			      x = xs !! m
			     zs = take m xs 
			     ys = drop (m+1) xs
			(t1,t2) = (fromOrdList zs, fromOrdList ys)
			     in = N t1 x t2

10)
fromlist::[a] -> heap a

data Heap a = E | N a (heap a) (heap a)
[2,5,4,3]
[N 2 E E, N S E E, N 4 E E, N 3 E E]
    t1  ,    t2  ,   t3   ,    t4
map(x->N x E E) xs

[merge.t,t2,map t3 t4]
	t1 , t2


[merge t1' t2']
	t1"


fromlist[] = E
fromlist zs = let hs 
            = map(x->N x E E) zs
            pares[] = []
            pares[x] = [x]
            pares(x:y:xs) = merge x y : pares xs 
            g[] = E
            g[h] = h
            g ys = g(pares ys)
            in g hs
+

esqueleto funcion heap merge completar. Prueba







-}
