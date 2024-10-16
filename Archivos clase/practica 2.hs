import qualified Data.Map as Map
import qualified Data.Set as S
import Data.Char (isAlpha)
import Data.Char (ord)
import GHC.Exts (IsList(..))
import Data.List
import Data.List (nub, sort)

--1)
--a)
borrarul xs = take (length xs-1) xs
--b)
--collect [] =
--collect ((x,y):xs) = 
--c)
serie [] = [[]]
serie xs = serie (borrarul xs) ++ [xs]
--f)
ror xs n = drop n xs ++ take n xs

rox (x:xs) 0 = (x:xs)
rox (x:xs) n = rox (xs ++ [x]) (n-1)
--g)
upto n m | n <= m = [n] ++ upto (n+1) m | otherwise = []
--h)
letras c 0 = []
letras c n = c : letras c (n-1)

tomarul xs = drop ((length xs)-1) xs

eco [] = []
eco xs = eco (borrarul xs) ++ letras (tomarul xs) (length xs)

--2)
--a)
--divisors x = [xs| xs <- [1..x] , mod x xs == 0]
--b)
--matches x xs = [ys|ys <- xs, ys== x]
--c)
--cuadrupla n = [(a,b,c,d)| a <- [0..n], b <- [0..n], c <- [0..n], d <- [0..n], a^2 + b^2 == c^2 + d^2]
--d)
--unique xs = [ys| ys <- xs, ]
--e)
--cambios (x:xs) = [ys | ys <-[0..((length xs)-1)], ]
--f)
--oblongos = [n*(n+1)|n<-[0..]]
--g)
--abundantes = []
--h)
--ecol xs = [ys|ys]

--4)
--a)
--foo1::Bool -> (Bool -> Bool)
--b)
--foo2::(b -> c) -> (a -> b) -> a -> c
--c)
--foo3::(a->b)->a->[b]->[b]
--d)
--foo4::(c->b)->c->[d]->[b]
--e)
--foo5::a->(c->b)->c->[a]
--f)
--foo6::[a]->(c->[a])->c->[a]
--g)
--foo7::[a]->(a->Bool)->[a]
--h)
--foo8::[a]->(a->Bool)->[a]
--i)
--foo9::[a]->(a->Bool)->[a]
--5)
--a)
map' f xs = foldr (\x acc-> f x:acc) [] xs
--b)
filter' f xs = foldr (\x acc -> if f x then x:acc else acc) [] xs
--c)
unzip' xs = foldr (\(x,y) (accX,accY) -> (x:accX,y:accY)) ([],[]) xs



--1)
data Nat = Cero | Succ Nat deriving (Show)
--a)
--Succ: Nat->Nat
--b)
int2nat 0 = Cero
int2nat x = Succ(int2nat (x-1))
--c)
sumanat Cero Cero = Cero
sumanat (Succ x) Cero = (Succ x)
sumanat Cero (Succ y) = (Succ y)
sumanat (Succ x) (Succ y) = sumanat (Succ(Succ x)) y
--d)
nat2int Cero = 0
nat2int (Succ x) = 1 + (nat2int x)

--2)
--N::Arb->Arb->Arb
