module Trabalho1 where

-- Rafael Borges Morais - 11911BCC040

--Ex1
triangulo :: Float -> Float -> Float -> [Char]
triangulo x y z
 | (x > 0 && x < 180) && (y > 0 && y < 180) && (z > 0 && z < 180) && (x + y + z) == 180 && (x == y) && (x == z) = "Equilatero"
 | (x > 0 && x < 180) && (y > 0 && y < 180) && (z > 0 && z < 180) && (x + y + z) == 180 && (x == 90 && y < 90 && z < 90) || (y == 90 && x < 90 && z < 90 ) || (z == 90 && x < 90 && y < 90)  = "Retangulo"

 | (x > 0 && x < 180) && (y > 0 && y < 180) && (z > 0 && z < 180) && (x + y + z) == 180 && x > 90 || y > 90 || z > 90 = "Obtuso"
 | (x > 0 && x < 180) && (y > 0 && y < 180) && (z > 0 && z < 180) && (x + y + z) == 180 = "Simples"
 | otherwise = "nao_triangulo"

--Ex2 
equacao :: Float -> Float -> Float -> (Float,Float)
equacao a b c
 | (-b)^2 - 4*a*c < 0 = error "Nao ha raiz"
 | a /= 0 = (sqrt((-b)^2 - 4*a*c),-1 * sqrt((-b)^2- 4*a*c))	
 | otherwise = (((-c)/b), a)

--Ex3
type Data = (Int,Int,Int)
preco :: Float -> Data -> Data -> Float
preco x (d,m,a) (d1,m1,a1)
 | (a - a1) < 2 = (15*x)/100
 | (a - a1) == 2 && m < m1 = (15*x)/100
 | (a - a1) == 2 && m == m1 && d < d1 = (15*x)/100
 | (a - a1) <= 10 = (40*x)/100
 | (a - a1) == 11 && m < m1 = (40*x)/100
 | (a - a1) == 11 && m == m1 && d < d1 = (40*x)/100
 | (a - a1) > 70 = (50*x)/100
 | (a - a1) == 70 && m > m1 = (50*x)/100
 | (a - a1) == 70 && m == m1 && d >= d1 = (50*x)/100
 | otherwise = x

--Ex4 
gera1 :: [Int]
gera1 = [x^2 | x <- [1..15],mod x 2 /= 0, x >= 4, x <= 14] 

gera2 :: [(Int,Int)]
gera2 = [(x,y) | x <- [1..15], y <- [x..2*x], x <= 4]


l1 = [10,11,12,13,14,15]
gera3 :: [[Int]]
gera3 = [[1 .. l1] | l1 <- [1..15], l1>= 10, l1 <=15]

gera4 :: [(Int,Int)]
gera4 = [(x,y) | x <- [1..15], y <- [x+1], mod x 2 /= 0]

gera5 :: [Int]
gera5 = [x+y | (x,y) <- gera4]

--Ex5
contaNegativos :: [Int] -> Int
contaNegativos xs = length [x | x <- xs, x < 0, mod x 2 == 0]


listaNegativos :: [Int] -> [Int]
listaNegativos xs = [x | x <- xs, x < 0, mod x 2 == 0] 

--Ex6 

distancias_origem :: [(Float,Float)] -> [Float]
distancias_origem xs = [sqrt(x^2 + y^2) | (x,y) <- xs]


--Ex7

fatores :: Int -> [Int]
fatores n = [i | i <- [1..n], mod n i == 0]

primos :: Int -> Int -> [Int]
primos x y = [n | n <- [x..y], fatores n == [1,n]]

--Ex8

mdc::Int->Int->Int
mdc a b 
 | a < b = mdc b a
 | b == 0 = a
 | otherwise = mdc b (mod a b)

mmc::Int->Int->Int
mmc x y = (x * y) `div` (mdc x y)

mmc3::Int->Int->Int->Int
mmc3 x y z = mmc x (mmc y z)

--Ex9

calculaSerie :: Float -> Int -> Float
calculaSerie x n
 | n == 1 = 1 / x
 | even n = (x / fromIntegral(n)) + (calculaSerie x (n-1))
 | otherwise = (fromIntegral(n) / x) + (calculaSerie x (n-1))

--Ex10

fizzbuzz :: Int -> [String]
fizzbuzz n = [ if mod x 15 == 0 then "FizzBuzz" else if mod x 5 == 0 then "Buzz" else if mod x 3 == 0 then "Fizz" else "No" | x <- [1..n]]
	

--Ex11

conta_ocorrencia :: Int -> [Int] -> Int
conta_ocorrencia _ [] = 0
conta_ocorrencia a (x:xs) 
 |a == x = 1 + conta_ocorrencia a xs 
 |otherwise = conta_ocorrencia a xs

conta_ocorrencias :: Int -> Int -> [Int] -> (Int,Int)
conta_ocorrencias a b (x:xs) = (conta_ocorrencia a (x:xs),conta_ocorrencia b (x:xs))

--Ex12

ocorrencia :: Int -> [Int] -> Int
ocorrencia a [] = 0
ocorrencia a (x:xs)
 | a == x = 1 + ocorrencia a xs
 | a /= x = ocorrencia a xs
	
unica_ocorrencia :: Int -> [Int] -> Bool
unica_ocorrencia a (x:xs)
 | ocorrencia a (x:xs) == 1 = True
 | otherwise = False

--Ex13

intercala :: [Int] -> [Int] -> [Int]
intercala [] [] = []
intercala (x:xs) [] = x:xs
intercala [] (y:ys) = y:ys
intercala (x:xs) (y:ys) = x:y:intercala xs ys  
	
--Ex14

type Nome = (String)
type Endereco = (String)
type Telefone = (Int)
type Email = (String)
type Contato = (Nome,Endereco,Telefone,Email)

recupera :: Email -> [Contato] -> Nome
recupera x [] = []
recupera x ((a,b,c,d):xs)
 | x == d = a
 | otherwise = recupera x xs

--Ex15

type Pessoa = (String, Float, Int, Char)
pessoas :: [Pessoa]
pessoas = [ ("Rosa", 1.66, 27,'F'), ("João", 1.85, 26, 'C'),("Maria", 1.55, 62, 'S'),("Jose", 1.78, 42, 'C'),("Paulo", 1.93, 25, 'S'),("Clara", 1.70, 33, 'C'),("Bob", 1.45, 21, 'C'),("Rosana", 1.58,39, 'S'),("Daniel", 1.74, 72, 'S'),("Jocileide", 1.69, 18, 'S') ]


--alturaMedia

somaMedia :: [Pessoa] -> Float
somaMedia [] = 0
somaMedia ((nome,altura,idade,estado):xs) = (altura + somaMedia xs)

alturaMedia :: [Pessoa] -> Float
alturaMedia xs = (somaMedia xs)/ fromIntegral(length xs)

--IdadeDaPessoaMaisNova

idadeN :: [Pessoa] -> Int
idadeN [] = 0
idadeN [(nome,altura,idade,estado)] = idade
idadeN ((nome,altura,idade,estado):(nome1,altura1,idade1,estado1):xs)
 |idade < idade1 = idadeN ((nome,altura,idade,estado):xs)
 |otherwise = idadeN ((nome1,altura1,idade1,estado1):xs)

--NomeEstadoCivil

nomeE :: [Pessoa] -> (String,Char)
nomeE [(nome,altura,idade,estado)] = (nome,estado)
nomeE (((nome,altura,idade,estado):(nome1,altura1,idade1,estado1):xs))
 |idade > idade1 = nomeE ((nome,altura,idade,estado):xs)
 |otherwise = nomeE ((nome1,altura1,idade1,estado1):xs)

--Pessoas50+

dados :: [Pessoa] -> [Pessoa]
dados ((nome,altura,idade,estado):xs) = [(nome,altura,idade,estado) | (nome,altura,idade,estado) <- ((nome,altura,idade,estado):xs), idade >= 50]

--NumPessoas

numPessoas :: [Pessoa] -> Int -> Int
numPessoas [] i = 0
numPessoas ((nome,altura,idade,estado):xs) i
 |idade > i && estado == 'C' = 1 + numPessoas xs i 
 |otherwise = numPessoas xs i

--Ex16

insere_ord :: (Ord t) => t -> [t] -> [t]
insere_ord a [] = [a]
insere_ord a (x:xs)
 | a > x = x : insere_ord a xs
 | otherwise = (a:x:xs)

--Ex17

reverte :: [t] -> [t]
reverte [] = []
reverte (x:xs) = reverte xs ++ [x]

--Ex18

sem_repetidos :: (Eq t) => [t] -> [t]
sem_repetidos [] = []
sem_repetidos [x] = [x]
sem_repetidos (x:xs) = x:(sem_repetidos (filter (/=x) xs))
	




