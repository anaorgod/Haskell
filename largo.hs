largo :: [Int] -> Int 

largo [] = 0 
largo (x:xs) = 1 + largo xs
