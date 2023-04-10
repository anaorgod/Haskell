repartir :: [String]->[String]->[(String, String)]

repartir [] [] = []
repartir xs [] = []
repartir [] zs = []
repartir (x:xs) (z:zs) = (x, z) : repartir xs zs
