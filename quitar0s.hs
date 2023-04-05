quitar0s :: [Int] -> [Int]

quitar0s [] = []
quitar0s (x:xs)| x /= 0 = x: quitar0s xs
               | x == 0 = quitar0s xs
