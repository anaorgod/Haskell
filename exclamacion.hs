exclamacion :: [Int] -> Int -> Int

exclamacion [] n = 0
exclamacion (x:xs) 0 = x
exclamacion (x:xs) n = exclamacion xs (n - 1)
