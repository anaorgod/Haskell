ultimo :: [Int] -> Int
 
ultimo [] = 0
ultimo (x:xs) |length (x:xs) /= 0 = x 
              |otherwise = ultimo xs
