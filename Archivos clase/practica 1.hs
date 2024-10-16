import qualified Data.Map as Map
import qualified Data.Set as S
import Data.Char (isAlpha)
import Data.Char (ord)
import GHC.Exts (IsList(..))
import Data.List
import Data.List (nub, sort)
--2)
--a)
five x = 5
-- five:: a -> Integer
--b)
apply f x = f x
-- apply:: (a -> b) -> a -> b
--c)
identidad x = x
-- identidad:: a -> a
-- d)
first x y = x
-- first:: a -> b -> a
-- e)
derive f x h = (f (x + h) - f x) / h
-- derive:: (Double -> Double) -> Double -> Double -> Double
-- f)
signo x = if x > 0 then 1 else if x < 0 then -1 else 0
-- signo:: (Ord a , Num a) => a -> a
--g)
vabs x = if x /= 0 then x else -x
-- vabs:: (Num a , Eq a , Ord a) => a -> a
--h)
pot x y = y^x
--pot:: (Num a) => a -> a -> a
--i)

--j)

--k)
swap (x,y) = (y,x)
--swap::(a,b)->(b,a)


--7)
--a)
suma [] = 0
suma (x:xs) = x + suma xs
--suma::Num a => [a] -> a
--b)
alguno [] = False
alguno (x:xs) | x == True = True | otherwise = alguno xs
--alguno::[Bool] -> Bool
-- c)
todos [] = True
todos (x:xs) | x == False = False | otherwise = todos xs
--todos::[Bool] -> Bool
--d)
codes [] = []
codes (x:xs) = (ord x) : codes xs
--e)
restos [] d = []
restos (x:xs) d = (mod x d): restos xs d
--restos::Integral a => [a] -> a -> [a]
--f)
cuadrados [] = []
cuadrados (x:xs) = x^2 : cuadrados xs
--cuadrados:: Num a => [a] -> [a]
--g)
longitudes [] = []
longitudes (x:xs) = length x : longitudes xs
--h)
orden [] = []
orden ((x,y):xs) | x < (3*y) = (x,y) : orden xs | otherwise = orden xs
--i)
pares [] = []
pares (x:xs) = if even x then x : pares xs else pares xs
--j)
letras [] = []
letras (x:xs) | x >= 'A' && x <= 'Z' = x : letras xs | x >= 'a' && x <= 'z' = x : letras xs |otherwise = letras xs
--k)
masde [] n = []
masde [[]] n = []
masde (x:xs) n | length x < n = masde xs n | otherwise = x : masde xs n

--8)
--a)
sumx xs = foldr (+) 0 xs
--b)
algunx xs | (filter (\x -> x /= False) xs) == [] = False | otherwise = True
--c)
todox xs | (filter (\x -> x /= True) xs) == [] = True | otherwise = False
--d)
codex xs = map (ord) xs 
--e)
restox xs n = map (\x -> x `mod` n) xs
--f)
cuadradox xs = map (^2) xs
--g)
longitudex xs = map (length) xs
--h)
ordex xs = filter (\(x,y) -> x < (y*3)) xs
--i)
parex xs = filter even xs
--j)
letrax xs = filter isAlpha xs
--k)
masdex xs n = filter (\x -> length x > n) xs