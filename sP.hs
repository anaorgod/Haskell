sP :: [Int] -> [Int]
sP xs | xs == [] = []
      | mod (head xs) 2 == 0 = head xs : sP (tail xs)
      | mod (head xs) 2 /= 0  = sP (tail xs) 
