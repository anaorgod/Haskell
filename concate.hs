concate :: [Int] -> [Int] -> [Int]

concate [] [] = []
concate xs [] = xs
concate [] ys = ys
concate (x:xs) ys = x: concate xs ys
