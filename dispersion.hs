dispersion :: Int -> Int -> Int -> Int
dispersion x y z = (max (max x y) z)-(min (min x y) z)
