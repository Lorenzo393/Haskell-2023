module Practica1 where

import Data.List
import Data.Char (ord)

{-
1) Los siguientes códigos tienen errores, cargar el archivo 20.Practica.0.hs en el interprete de Haskell
GHCi, leer los mensajes de error y corregirlos hasta que el archivo se cargue correctamente.
-}

-- a)
-- regla b = case b of
--    True -> "Quedate en Casa"
--    False -> "Qudate en Casa"

-- b)
-- case [x]         =  []
-- case (x:y:xs)      =  y : case (x:xs)
-- case []          =  []

-- c)
maps f []        =  []
maps f (x:xs)     =  f x : map f xs

-- d)
listNumeros = 1 : 2 : 3 : []

-- e)
[]     ++! ys = ys
(x:xs) ++! ys = x : xs ++! ys

-- f)
addToTail x xs = map (+x) (tail xs)

-- g)
listmin xs = head (sort xs)

-- h)
smap f [] = []
smap f [x] = [f x]
smap f (x:xs) = f x : smap f xs


{-
2. Definir las siguientes funciones y determinar su tipo:
a) five, que dado cualquier valor, devuelve 5

b) apply, que toma una función y un valor, y devuelve el resultado de
aplicar la función al valor dado

c) identidad, la función identidad

d) first, que toma un par ordenado, y devuelve su primera componente

e) derive, que aproxima la derivada de una función dada en un punto dado

f) sign, la función signo

g) vabs, la función valor absoluto (usando sign y sin usarla)

h) pot, que toma un entero y un número, y devuelve el resultado de
elevar el segundo a la potencia dada por el primero

i) xor, el operador de disyunción exclusiva

j) max3, que toma tres números enteros y devuelve el máximo entre llos

k) swap, que toma un par y devuelve el par con sus componentes invertidas
-}

five x = 5
-- five:: a -> Integer
apply f x = f x
-- apply:: (a -> b) -> a -> b
iden x = x
-- iden:: a -> a
first (x,y) = x
-- first:: (a,b) -> a
sign x | x > 0 = 1 
       | x < 0 = -1 
       | otherwise = 0
-- sign:: a -> Integer
vabsign x | sign x == 1 = x | sign x == (-1) = -x | otherwise = x
-- vabsing:: Int -> Int
pot x y = x^y
-- pot:: Int -> Float -> Float
swap (x,y) = (y,x)
-- swap:: (a,b) -> (b,a)

{-
3) Definir una función que determine si un año es bisiesto o no, de
acuerdo a la siguiente definición:
año bisiesto 1. m. El que tiene un día más que el año común, añadido al mes de febrero. Se repite
cada cuatro años, a excepción del último de cada siglo cuyo número de centenas no sea múltiplo
de cuatro. (Diccionario de la Real Academia Espaola, 22ª ed.)
¿Cuál es el tipo de la función definida?
-}


{-
4) Dar al menos dos ejemplos de funciones que tengan cada uno de los siguientes tipos:
a) (Int -> Int) -> Int
b) Int -> (Int -> Int)
c) (Int -> Int) -> (Int -> Int)
d) Int -> Bool
e) Bool -> (Bool -> Bool)
f) (Int,Char) -> Bool
g) (Int,Int) -> Int
h) Int -> (Int,Int)
i) a -> Bool
j) a -> a
-}


{-
5) Definir las siguientes funciones usando listas por comprensión:

a) 'divisors', que dado un entero positivo 'x' devuelve la
lista de los divisores de 'x' (o la lista vacía si el entero no es positivo)

b) 'matches', que dados un entero 'x' y una lista de enteros descarta
de la lista los elementos distintos a 'x'

c) 'cuadrupla', que dado un entero 'n', devuelve todas las cuadruplas
'(a,b,c,d)' que satisfacen a^2 + b^2 = c^2 + d^2,
donde 0 <= a, b, c, d <= 'n'

(d) 'unique', que dada una lista 'xs' de enteros, devuelve la lista
'xs' sin elementos repetidos
unique :: [Int] -> [Int]
-}

-- divisors x = [ys|ys<-[1..x], mod x ys == 0]
-- divisors:: Integral a => a -> [a]

-- matches x ys = [rs|rs<-ys, x/=rs]
-- marches:: a->[a]->[a]

-- cuadrupla x = [(a,b,c,d)| a<-[1..x], b<-[1..x], c<-[1..x], d<-[1..x],a^2 + b^2 == c^2 + d^2]
-- cuadrupla:: a->[(a,a,a,a)]

--unique resolver en algun momento


{-
6) El producto escalar de dos listas de enteros de igual longitud
es la suma de los productos de los elementos sucesivos (misma
posición) de ambas listas.  Definir una función 'scalarProduct' que
devuelva el producto escalar de dos listas.

Sugerencia: Usar las funciones 'zip' y 'sum'. -}

{-
7) Sin usar funciones definidas en el
preludio, defina recursivamente las siguientes funciones y
determine su tipo más general:

a) 'suma', que suma todos los elementos de una lista de números

b) 'alguno', que devuelve True si algún elemento de una
lista de valores booleanos es True, y False en caso
contrario

c) 'todos', que devuelve True si todos los elementos de
una lista de valores booleanos son True, y False en caso
contrario

d) 'codes', que dada una lista de caracteres, devuelve la
lista de sus ordinales

e) 'restos', que calcula la lista de los restos de la
división de los elementos de una lista de números dada por otro
número dado

f) 'cuadrados', que dada una lista de números, devuelva la
lista de sus cuadrados

g) 'longitudes', que dada una lista de listas, devuelve la
lista de sus longitudes

h) 'orden', que dada una lista de pares de números, devuelve
la lista de aquellos pares en los que la primera componente es
menor que el triple de la segunda

i) 'pares', que dada una lista de enteros, devuelve la lista
de los elementos pares

j) 'letras', que dada una lista de caracteres, devuelve la
lista de aquellos que son letras (minúsculas o mayúsculas)

k) 'masDe', que dada una lista de listas 'xss' y un
número 'n', devuelve la lista de aquellas listas de 'xss'
con longitud mayor que 'n' -}

suma [] = 0
suma (x:xs) = x + (suma xs)
-- suma:: [a]->a

alguno [] = False
alguno (x:xs) = if x == True then True else (alguno xs)
-- alguno:: [Bool]->Bool

todos [] = False
todos (x:xs) = if x == True then (todos xs) else False
-- todos:: [Bool]->Bool

codes [] = []
codes (x:xs) = (ord x):codes xs
-- codes:: [Char]->[Int]

restos n [] = []
restos n (x:xs) = (mod x n): (restos n xs)
-- restos:: a->[a]->[a]

cuadrados [] = []
cuadrados (x:xs) = (x^x): cuadrados xs
-- cuadrados:: [a]->[a]

longitudes [] = []
longitudes (x:xs) = (length x) : longitudes xs 
-- longitudes:: [[a]]->[a] !!!!!!!!!!!!!!!!!!!!!!!!!

orden [] = []
orden ((x,y):xs) = if x< (3*y) then (x,y):orden xs else orden xs
-- orden:: [(a,b)]->[(a,b)]

pares [] = []
pares (x:xs) = if even x then x: pares xs else pares xs
-- pares:: [a]->[a]

letras [] = []
letras (x:xs) = if ((ord x) >= 65 && (ord x) <= 90) || ((ord x) >= 97 && (ord x) <= 122) then x :letras xs else letras xs
-- letras:: [Char]->[Char]

masDe n [] = []
masDe n (x:xs) = if length x > n then x : masDe n xs else masDe n xs
-- masDe:: a->[a]->[a]

--8) Redefinir las funciones del ejercicio anterior usando foldr, map y filter.
--ver su definición en https://hoogle.haskell.org/

sumax xs = foldr (+) xs
algunox xs = foldr (||) False xs
todox xs = foldr (&&) True xs
codex xs = map (ord) xs
restox n xs = map (\x -> x `mod` n) xs
cuadradox xs = map (\x->x^x) xs
longitudex xss = map (\x->length x) xss
ordenx xst = filter (\(x,y)->x<(y*3)) xst
parex xs = filter (even) xs
letrax xs = filter (\x->((ord x) >= 65 && (ord x) <= 90) || ((ord x) >= 97 && (ord x) <= 122)) xs
masDex n xss = filter (\x->length x > n) xss