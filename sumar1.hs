sumar1 :: [Int]->[Int]

sumar1 xd | xd  == [] = []
          | xd /=  [] = (head xd)+1 : sumar1 (tail xd)
