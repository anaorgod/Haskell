maximo :: [Int] -> Int

maximo [] = error "no tiene maximo"
maximo [x] = x 
maximo (x : xs) = max x (maximo xs)
