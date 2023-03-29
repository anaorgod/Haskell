duplica :: [Int] -> [Int]

duplica xs | xs == [] = []
           | xs /= [] = (head xs) *2 : duplica (tail xs)
