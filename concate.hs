concate :: [[Int]] -> [Int]

concate [] = []
concate (x:xs) = x ++ concate xs
