

























--1)
data Color = Color {red::Int , green::Int , blue::Int} deriving (Show)

mezclar x y = Color (div ((red x)+(red y)) 2) (div ((green x)+(green y)) 2)(div ((blue x)+(blue y)) 2)

--2)
data Lineas = Lineas {cadena::String , posicion::Int} deriving (Show)

vacia x = Lineas "" 0

moverIzq x = if (posicion x) == 0 then Lineas (cadena x) (posicion x) else Lineas (cadena x) ((posicion x)-1) 

moverDer x = if (posicion x) == (length (cadena x)) then Lineas (cadena x) (posicion x) else Lineas (cadena x) ((posicion x)+1) 

moverIni x = Lineas (cadena x) 0

moverFin x = Lineas (cadena x) (length (cadena x))

insert x y = Lineas (take (posicion x) (cadena x) ++ [y] ++ drop (posicion x) (cadena x)) ((posicion x)+1) 

borrar x = Lineas (take ((posicion x)-1) (cadena x) ++ drop (posicion x) (cadena x)) ((posicion x)-1) 

--3)
data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving (Show)


--5)
data BST a = E | N (BST a) a (BST a)
-- (N l x r)
{-
imput (N l x r)

minimun (N E x E) = x
minimun (N l x r) = minimun l 

maximun (N E x E) = x
maximun (N l x r) = maximun r

checkBST:: BST a -> bool
checkBST E = True
checkBST (N E x E) = True
checkBST (N E x r) = checkBST r && (minumun r) > x
checkBST (N l x E) = checkBST l && (maximun l) <= x
checkBST (N l x r) =  checkBST l && checkBST r && (maximun l) <= x &&(minimun r) > x
-}

--?)
{-
insert x E = N E x E
insert x (N l y r) \ x > y = (N l y (insert x r)) 
                   \otherwise = (N (insert x l) y r)
----------------------------------------------------------------      
member e E = false
member e (N l y r) \ e == y = True
                   \ e > y = member e r
                   \ otherwise = member e l
                   
peor caso d + 1. d es la altura del arbol
mejor caso raiz
----------------------------------------------------------------
-}
