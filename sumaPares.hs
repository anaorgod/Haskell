sumaPares :: [(Int, Int)] -> Int

sumaPares [] = 0 
sumaPares ((x,y):xs) = x + y + sumaPares xs
