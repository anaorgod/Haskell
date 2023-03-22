esBisiesto  :: Int -> Bool
esBisiesto x | (mod x 400 == 0) || (mod x 4 == 0 && mod x 100 /= 0) = True
             | (mod x 400 /= 0) && (mod x 4 /= 0 || mod x 100 == 0) = False
