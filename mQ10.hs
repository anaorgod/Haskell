mQ10 :: [Int] -> [Int]

mQ10 xs |xs == [] = []
        | head xs > 10 = head xs : mQ10 (tail xs)
        | head xs <= 10 = mQ10 (tail xs)
