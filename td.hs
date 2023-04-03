todosmenores :: [Int] -> Bool

todosmenores  [] = True 

todosmenores (x:xs)  | x >= 10 = False 
                     | otherwise = todosmenores xs
