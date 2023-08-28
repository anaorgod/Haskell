{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
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
--42

--c
productoria :: [Int] -> Int
productoria [] = 1 
productoria (x:xs) = x * productoria xs 


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
paratodo' (x:xs) b = b x && paratodo' xs b 

--ghci> paraTodo [2,5,0,3,6] esCero
--False

--ghci> paraTodo [8, (-1), 2] esPositivo
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

paraTodo :: [Bool] -> Bool
paraTodo xs = paratodo' xs (==True) 

--6

--a 

esDivisiblepor2 :: Int -> Bool
esDivisiblepor2 x = mod x 2 == 0

--Main> esDivisiblepor2 4
--True

--Main> esDivisiblepor2 3
--False

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

--d

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

esPrimo2 :: [Int] -> [Int]
esPrimo2 (x:xs) = [2..x-1] (multiplo x) 


