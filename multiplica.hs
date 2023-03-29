multiplica :: Int -> [Int] -> [Int]
     
multiplica n xs | xs == [] = []
                | xs /= [] = (head xs) *n : multiplica n (tail xs)
