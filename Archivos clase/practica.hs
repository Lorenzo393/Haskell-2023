[(x,y,z)|x <- ["Black boy","Nigga","Nigger"], y <- ["must","go to"], z <- ["burn","die","jail"]] 




2)
a)
let five x = 5
b)
let sum3 x = x + 3
let apply x y = x y
c)
let identidad x = x
d)
let first x y = x
e)

f)
let sign x = if x > 0 then "pos" else if x < 0 then "nigger" else "cero"
g)
let valve x = if sign x == "pos" then x else if sign x == "nigger" then (-x) else 0

let valve2 x = if x > 0 then x else if x < 0 then (-x) else 0 
h)
let pot x y = y ^ x

pot 0 y = 1 
pot x y = y * pot (x-1) y 
i)
xor x y = if x /= y then True else False
j)
max3 x y z | x > y && x > z = x | y > x && y > z = y | otherwise = z 
k)
swap (x,y) = (y,x) 
-----------------------------------------------------------------
3)
bisiest x | (mod x 4 == 0) && (mod x 100 /= 0) = True | otherwise = False
----------------------------------------------------------------
5)
a)
divisors x = [f|f <- [1..x],mod x f == 0]
b)
matches x ys = [j| j <- ys, j == x]
c)


