module Pratica4 where

{- 1 Avalie os seguintes exemplos. -}


lst1 = [x*2 | x <- [1..10], x*2 >= 12]
lst2 = [ x | x <- [50..100], mod x 7 == 3]
lst3 = [ x | x <- [10..20], x /= 13, x /= 15, x /= 19]
lst4 = [(x,y)| x <- [1..4], y <- [x..5]]

{- 2 Escreva a função quadrados que recebe dois inteiros e retorna os quadrados dos
números entre eles. -}

quadrados :: Int -> Int -> [Int]
quadrados x y = [n*n | n <- [x..y]]

{- 3  Usando lista por compreensão, escreva a função seleciona_ímpares que recebe um
lista de inteiros e retorna uma nova lista com todos os números ímpares presentes na lista
de entrada. -}

seleciona_ímpares :: [Int] -> [Int]
seleciona_ímpares xs = [x | x <- xs, odd x]

{- 4  Escreva a função tabuada que recebe um valor inteiro e retorna a lista de seus dez
primeiros múltiplos. -}

tabuada :: Int -> [Int]
tabuada x = [x*y | y <- [1..10]]

{- 5 Escreva a função bissextos a seguir que recebe uma lista de inteiros e retorna uma
lista com os valores que representam anos bissextos. -}

bissexto :: Int -> Bool
bissexto x  
 |(mod x 400 == 0) = True
 |(mod x 4 == 0) && (mod x 100 /= 0) = True
 |otherwise = False

bissextos :: [Int] -> [Int]
bissextos xs = [x | x <- xs, bissexto x]

{- 6 Usando lista por compreensão, escreva a função sublistas que recebe uma lista
formada por sublistas de um mesmo tipo e retorna uma lista com todos os elementos da
lista de entrada na mesma ordem, mas no nível da lista principal, sem sublistas. -}

sublistas :: [[Int]] -> [Int]
sublistas (x:xs) = [x | y <- (x:xs), x <- y]


{- 7 Sejam os tipos Data, Emprestimo, Emprestimos e a variável bdEmprestimo do exemplo
da Biblioteca. Escreva a função atrasados que recebe um parâmetro do tipo Emprestimos
e a Data atual, e retorna uma lista com todos os empréstimos atrasados. -}

type Data = (Int,Int,Int)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]

bdEmprestimo :: [Emprestimo]

bdEmprestimo =
 [("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),
 ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"),
 ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]

atrasados :: Emprestimos -> Data -> Emprestimos
atrasados [] _ = []
atrasados ((x,y,(d,m,a),(dv,mv,av),z):xs) (d1,m1,a1)
 |d1 > dv = (x,y,(d,m,a),(dv,mv,av),z) : atrasados xs (d1,m1,a1)
 |otherwise = atrasados xs (d1,m1,a1)

{- 8 Escreva a função recursiva npares que recebe uma lista de inteiros e retorna a
quantidade de números pares pertencentes à lista. -}

npares :: [Int] -> Int
npares [] = 0
npares (x:xs)
 | mod x 2 == 0 = 1 + npares xs
 | otherwise = npares xs

 {- 9  Escreva a função recursiva produtorio que recebe uma lista de números e retorna o
produto de todos os seus elementos. -}

produtorio :: [Int] -> Int
produtorio [] = 1
produtorio (x:xs) = x * produtorio xs

{- 10 Escreva a função recursiva comprime a seguir que recebe uma lista de listas e retorna
uma lista contendo todos os elementos das sublistas.-}

comprime :: [[t]] -> [t]
comprime [[]] = []
comprime [[x]] = [x]
comprime ([]:ys) = comprime ys
comprime ((x:xs):ys) = x:(comprime (xs:ys))

{- 11  Escreva a função tamanho a seguir que recebe uma lista polimórfica (de qualquer
tipo) e retorna a quantidade de elementos que ela possui. -}

tamanho :: [t] -> Int
tamanho xs = length [x | x <- xs]

{- 12 Usando compreensão de listas, escreva a função uniaoNRec a seguir que faz a união
de duas listas de modo que ela mantenha todos os elementos da 1a lista na mesma
ordem e no final acrescenta apenas os elementos da 2a lista que não estejam presentes
na 1a lista. -}

compara :: Eq t => t -> [t] -> Bool
compara  x [] = False
compara x (y:ys)
 |x == y = True
 |otherwise = compara x ys

uniaoNRec :: Eq t => [t] -> [t] -> [t]
uniaoNRec xs ys = [x  | x <- xs] ++ [y | y <- ys, compara y xs == False]

{- 13 Escreva a função recursiva uniaoRec2 a seguir que faz a união de duas listas de
modo que ela mantenha todos os elementos da 1a lista na mesma ordem e no final
acrescenta apenas os elementos da 2a lista que não estejam presentes na 1a lista.-}

uniaoRec2 :: Eq t => [t] -> [t] -> [t]
uniaoRec2 [] [] = []
uniaoRec2 xs [] = xs
uniaoRec2 [] ys = ys
uniaoRec2 (x:xs) (y:ys)
 | x == y =  uniaoRec2 (x:xs) (y:ys)
 |otherwise = uniaoRec2 (x:xs) ys  ++ [y]
   