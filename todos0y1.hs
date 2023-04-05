todos0y1 :: [Int] -> Bool

todos0y1 [] = True 
todos0y1 (x:xs) | ((x==0) || (x==1)) && todos0y1 xs = True
                | (x/=0) || (x/=1) = False
