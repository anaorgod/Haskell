f_T_c :: Double -> Double
f_T_c a = (a-32)/1.8

haceFrioF :: Double -> Bool
haceFrioF a= if (f_T_c a) <= 8 then True else False
