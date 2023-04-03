repartir [] [] = []
repartir [] zs = []
repartir xs [] = []
repartir (x:xs) (z:zs) = (x,z) : repartir xs zs
