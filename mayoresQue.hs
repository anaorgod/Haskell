mayoresQue :: Int -> [Int] -> [Int]

mayoresQue n xs |xs == [] = []
                | head xs > n = head xs : mayoresQue n (tail xs)
                | head xs <= n = mayoresQue n (tail xs)
