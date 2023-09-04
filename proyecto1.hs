{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import System.Win32 (COORD(xPos))
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Evaluate" #-}
--1

--a

esCero :: Int -> Bool
esCero x = x==0
--ejemplos
--ghci> esCero 5
--False 

--ghci> esCero 0
--True

--b
esPositivo :: Int -> Bool
esPositivo x = x>=0

--ghci> esPositivo 6
--True

--ghci> esPositivo (-9)
--False

--c
esVocal :: Char -> Bool
esVocal x = x=='a'|| x=='e'|| x=='i'|| x=='o'|| x=='u'

--ghci> esVocal 'a'
--True

--ghci> esVocal 'z'
--False

--d
valorAbsoluto :: Int -> Int
valorAbsoluto x | x>=0 =x
                | x<0 = x*(-1)

--ghci> valorAbsoluto (-10)
--10

--ghci> valorAbsoluto (-90)
--90

--2

--a
paratodo :: [Bool] -> Bool
paratodo [] = True
paratodo (x:xs) = x && paratodo xs

--ghci> paratodo [False, True, True]  
--False

--paratodo [True, True]       
--True

--b
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

--sumatoria [5,6,9,3,8]
--31

--ghci> sumatoria [9,8,7,5,3,2,1,4,6]
--45

--c
productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs 
--

--d
factorial :: Int -> Int
factorial 0=1
factorial n = n * factorial (n-1)

--ghci> factorial 3
--6

--ghci> factorial 0
--1

--e
promedio :: [Int] -> Int 
promedio x = div (sumatoria x) (length x)

--ghci> promedio [1,4,2]
--2

--ghci> promedio [6,8,9,2]
--6

--3

pertenece :: Int -> [Int] -> Bool
pertenece a [] = False 
pertenece a (x:xs)| x==a = True
                  | x/=a = False || pertenece a xs

--pertenece 5 [9,6,3,2,5,4,1]
--True

--pertenece 8 [2,5,6]  
--False

--4

--a
paratodo' :: [a] -> (a -> Bool) -> Bool
paratodo' [] b = True
paratodo' (x : xs) b
         | b x == True = paratodo' xs b
         | otherwise = False

--ghci> paratodo' [2,5,0,3,6] esCero
--False

--ghci> paratodo' [8, (-1), 2] esPositivo
--False

--b
existe' :: [a] -> (a -> Bool) -> Bool
existe' [] b = False
existe' (x:xs) b = b x || existe' xs b 

--existe' [6,5,0,3,2] esCero
--True

--ghci> existe' [(-9), (-6), 5, (-2)] esPositivo
--True

--c
sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] b = 0
sumatoria' (x:xs) b = b x + sumatoria' xs b 

--ghci> sumatoria'  [(-9), (-6), 5, (-2)] valorAbsoluto
--22

--ghci> sumatoria'  [3, 2, 5, 1] factorial             
--129

--d
productoria' :: [a] -> (a -> Int) -> Int
productoria' [] b = 1
productoria' (x:xs) b = b x * productoria' xs b 

--5

paratodo3 :: [Bool] -> Bool
paratodo3 xs = paratodo' xs id

--6

--a 

esDivisiblepor2 :: Int -> Bool
esDivisiblepor2 x = mod x 2 == 0

todosPares :: [Int] -> Bool 
todosPares (x:xs) = paratodo' (x:xs) esDivisiblepor2

--ghci> todosPares [6,2,4,8,12]
--True

--ghci> todosPares [7,2,6,9]   
--False

--b

multiplo :: Int -> Int -> Bool
multiplo x y = (mod x y) == 0

hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo b (x:xs) = existe' (x:xs) (multiplo b)

--ghci> hayMultiplo 60 [6, 2, 3]
--True

--ghci> hayMultiplo 100 [5, 2, 3]
--True

--c
 
cuadrado :: Int -> Int
cuadrado x = x^2

sumaCuadrados :: Int -> Int
sumaCuadrados x = sumatoria' [0..x] cuadrado 

--ghci> sumaCuadrados 3
--14

--ghci> sumaCuadrados 6
--91

existeDivisor :: Int-> [Int] -> Bool
existeDivisor n xs = existe' xs (multiplo n)

--Main> existeDivisor 2 [4,6,8]
--False

--Main> existeDivisor 16 [4,6,8]
--True

--e 

esPrimo :: Int -> Bool
esPrimo x = not (existe'[2..x-1] (multiplo x)) || x == 2  

--Main> esPrimo 2
--True

--Main> esPrimo 2
--True

--f

factorialSR :: Int -> Int
factorialSR n = productoria [1..n]

--Main> factorialSR 5
--120

--Main> factorialSR 9
--362880

--g

esPrimo2 :: Int -> Int
esPrimo2 x | esPrimo x = x
           | otherwise = 1

multiplicaPrimos :: [Int] -> Int
multiplicaPrimos (x:xs) = productoria' (x:xs) (esPrimo2)

--multiplicaPrimos [5,4,7]
--35

--ghci> multiplicaPrimos [4,3,2,1]    
--6

--h

fib:: Int -> Int
fib 0 = 0
fib 1 = 1 
fib n = fib (n - 1) + fib (n - 2)

esFibb :: [Int ]
esFibb = [fib x | x <- [0..]]

auxFibb :: Int -> [Int] -> Bool
auxFibb x [] = False
auxFibb x (y:ys) | x<y = False
                  | x>y = auxFibb x ys
                  | x==y = True

esFib b = auxFibb b esFibb

--ghci> esFib 3  
--True

--ghci> esFib 3  
--True

--i

todosFib :: [Int] -> Bool
todosFib b = paratodo' b esFib

--ghci> todosFib [5,8,6,3,2]
--False

--ghci> todosFib [3,2,5]    
--True

--7

--Que hacen estas funciones?

--map: Aplica la funcion a cada elemento de la lista y devuelve otra lista con los resultados
--tipo: map :: (a -> b) -> [a] -> [b]
-- Lo que hace la expresion map succ es sumar +1 a cada elemento de tipo Int de la lista dada. Devuelve [2, -3, 7, 3, -7]

--filter: Es una funcion que toma una funcion que devuelve valor booleano y una lista como entrada, y devuelve  
--una lista que contiene los elementos de la lista de entrada en los cuales la funcion booleana devuelve True.
--tipo: filter :: (a->Bool)->[a]->[a]
-- filter esPositivo es una funcion q diferencia entre enteros positivos y negativos, y su resultado es la nueva lista:
-- [1, 6, 2]

--8a

ej8:: [Int] -> [Int]
ej8  [] = []
ej8 (x:xs) = (x*2): ej8 xs

--ghci> ej8 [2,5,3,6]
--[4,10,6,12]

--ghci> ej8 [3,9,4]
--[6,18,8]

--8b

duplica2 x = map (*2) x

--ghci> duplica2 [2,3,6]
--[4,6,12]

--ghci> duplica2 [8,5,4]
--[16,10,8]

--9a

soloPrimos :: [Int] -> [Int]
soloPrimos [] = []
soloPrimos (x:xs) | esPrimo x == True = x:soloPrimos xs
                  | esPrimo x == False = soloPrimos xs

--ghci> soloPrimos [3,5,9]
--[3,5]

--ghci> soloPrimos [2,7,6,1]
--[2,7,1]

--b

soloPrimos2 xs = filter esPrimo xs

--ghci> soloPrimos2 [2,5,6,3]
--[2,5,3]

--ghci> soloPrimos2 [3,8,7,6,9]
--[3,7]

--c

esPrimoM :: Int -> Bool
esPrimoM x = not (null (filter esPrimo [2..x]))

--ghci> esPrimoM 3
--True

--ghci> esPrimoM 6 
--False

--10a

primIgualesA:: Eq a=> a -> [a] -> [a]
primIgualesA b [] = []
primIgualesA b (x:xs) | (b == x) = x : primIgualesA b xs
                      | otherwise = []

--ghci> primIgualesA 5 [5,5,2,5,6,3]
--[5,5]

--ghci> primIgualesA 3 [9,2,6,3,5]  
--[]

--b

primIgualesA2 b xs = takeWhile (\x -> x==b) xs

--ghci> primIgualesA2 3 [9,2,6,3,5]
--[]

--ghci> primIgualesA2 3 [3,3,3,6,5] 
--[3,3,3]

--11a

primIguales :: Eq a => [a] -> [a]
primIguales [] = []
primIguales [x] = [x]

primIguales (x:xs) | (x == head xs) = x : primIguales xs
               | otherwise = [x]

--ghci> primIguales [2,2,2,5,6,3]
--[2,2,2]

--ghci> primIguales [3,3,6,5]  
--[3,3]

--b

primIgualesB :: Eq a => [a] -> [a]
primIgualesB xs = primIgualesA (head xs) xs

--ghci> primIgualesB [3,3,3,6,5]  
--[3,3,3]

--ghci> primIgualesB [7,7,5,9,5]    
--[7,7]
