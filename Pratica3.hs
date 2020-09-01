module Pratica3 where

--Exercicio1
--a

ou_logico :: (Bool,Bool) -> Bool			
ou_logico (True,True) = True
ou_logico (True,False) = True
ou_logico (False,True) = True
ou_logico (False,False) = False

ou_logico2 :: (Bool,Bool) -> Bool
ou_logico2 (False,False) = False
ou_logico2 (_,_) = True

ou_logico3 :: (Bool,Bool) -> Bool
ou_logico3 (False,a) = a
ou_logico3 (True,_) = True

--b
    
ou_logico4 :: Bool -> Bool -> Bool
ou_logico4 bool bool1
    |bool == True = True
    |bool1 == True = True
    |otherwise = False


ou_logico5 :: Bool -> Bool -> Bool
ou_logico5 bool bool1
    |bool == False && bool1 == False = False
    |otherwise = True

--Exercicio2

distancia :: (Float,Float) -> (Float,Float) -> Float
distancia (x,y) (x1,y1) = sqrt(((x-x1)^2)+((y-y1)^2))

--Exercicio4

fatorial :: Int -> Int
fatorial x
	|x == 0 = 1
	|otherwise = x * fatorial x-1

fatorial1 :: (Int) -> (Int)
fatorial1 0 = 1
fatorial1 a = a * fatorial1 (a-1)

--Exercicio5

fib :: Int -> Int
fib x
	|x == 0 = 0
	|x == 1 = 1
	|otherwise = fib (x-2) + fib (x-1)

--Exercicio6

n_tri :: Int -> Int
n_tri x
	|x == 0 = 0
	|otherwise = x + n_tri (x-1)

--Exercicio7

seq :: (Int, Int) -> (Int, Int)
seq (x, y) = (y, x+y)

fibo :: Int -> (Int, Int)
fibo 0 = (0,1)
fibo x = seq (fibo (x - 1))

--Exercicio8

potencia :: Int -> Int -> Int
potencia x y = if y == 0 then 1 else x * potencia x y-1

--Exercicio9
--a

prodI :: (Int,Int) -> Int
prodI (x,y) = if x == y then y else x * prodI (x+1,y)

--b
fatorialp :: Int -> Int
fatorialp x 
	|x == 0 = 0
	|otherwise = prodI (1,x)

--Exercicio11

resto_div :: (Int,Int) -> Int
resto_div (x,y)
	|x == y = 0
	|x < y = x
	|otherwise = resto_div (x-y,y)

div_inteira :: (Int,Int) -> Int
div_inteira (x,y)
	|x == y = 1
	|x < y = 0
	|otherwise = resto_div (div_inteira(x,y),y) * y

--Exercicio12

mdc1 :: (Int,Int) -> Int
mdc1 (x,y)
	|y == 0 = x
	|otherwise = mdc1(y,(mod x y))

mdc2 :: (Int,Int) -> Int
mdc2 (x,0) = x
mdc2 (x,y) = mdc2(y,(mod x y))

--Exercicio13

binomial1 :: (Int,Int) -> Int
binomial1 (n,0) = 1
binomial1 (n,k)
 |k == 0 = 1
 |k == n = 1
 |otherwise = binomialg (n-1,k) + binomialg (n-1,k-1)

binomial2 :: (Int,Int) -> Int
binomial2 (n,0) = 1
binomial2 (n,k) = if (k == n) then 1 else binomial (n-1,k) +
binomial2 (n-1,k-1)

--Exercicio14

--a
[5..1]
--b
[a,c..e]
--c
[1,4..16]
--d
zip :: [a] -> [b] -> [(a,b)]
zip [1,-2..-11] [1,5..17]

--Exercicio15

--a
lista1 :: Int -> Int -> [Int]
lista1 a b
	|a == b = [a]
	|a > b = []
	|otherwise = [a..b]

--b
lista2 :: Int -> Int -> [Int]
lista2 a b
	|a < b = [x | x <- [a..b], even x]
	|otherwise = []







	