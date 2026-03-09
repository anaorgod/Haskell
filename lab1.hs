{-# LANGUAGE NPlusKPatterns #-}

--EJERICICIO 1

--a)verifica si x vale 0
esCero :: Int -> Bool
esCero x = x == 0

--b)verifica si el numero es mayor a 0
esPositivo :: Int -> Bool
esPositivo x = x > 0 

--c)verifica si un caracter es una vocal en minuscula
esVocal :: Char -> Bool
esVocal x | x == 'a' = True
          | x == 'e' = True
          | x == 'i' = True
          | x == 'o' = True
          | x == 'u' = True
          |otherwise = False 
          
--d)devuelve el valor absoluto de un entero ingresado.
valorAbsoluto :: Int -> Int
valorAbsoluto x | (x >= 0) = x
                | (x < 0) = -x

--EJERCICIO 2

--a)que verifica que todos los elementos de una lista sean True.
paratodo :: [Bool] -> Bool
paratodo [] = True
paratodo (x:xs) = x && paratodo xs 

--b)que calcula la suma de todos los elementos de una lista de enteros.
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

--c)que calcula el producto de todos los elementos de la lista de enteros.
productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs

--d)que toma un numero n y calcula n!.
factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x-1)

--e)sumatoria para definir, promedio :: [Int] -> Int, que toma una lista de numeros no vacıa y calcula el valor promedio
promedio :: [Int] -> Int
promedio (x:xs) = div (sumatoria xs) (length xs)

--EJERCICIO 3
--verifica si un numero se encuentra en una lista.
pertenece :: Int -> [Int] -> Bool
pertenece a [] = False
pertenece a (x:xs) | ( a==x ) = True
                   | ( a/=x ) = pertenece a xs 
                   
                  
--EJERCICIO 4
--A)dada una lista xs de tipo [a] y unpredicado T :: a -> Bool, determina si todos los elementos de xs satisfacen el predicado T.
paratodo' :: [a] -> (a -> Bool) -> Bool
paratodo' [] f = True
paratodo' (x:xs) f = f(x) && paratodo' xs f






























--
