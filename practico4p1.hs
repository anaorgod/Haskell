data Color = Rojo | Amarillo | Azul | Verde
	deriving (Show, Eq)

data Forma = Triangulo | Cuadrado | Rombo | Circulo
	deriving (Show, Eq)

type Figura = (Forma, Color, Int)

rojo :: Figura -> Bool
rojo (f ,c ,t) = c == Rojo

azul :: Figura -> Bool
azul (f,c,t) = c == Azul

verde :: Figura -> Bool
verde (f,c,t) = c == Verde

amarillo :: Figura -> Bool
amarillo (f,c,t) = c == Amarillo

circulo :: Figura -> Bool
circulo (f,c,t) = f == Circulo

triangulo :: Figura -> Bool
triangulo (f,c,t) = f == Triangulo

rombo :: Figura -> Bool
rombo (f,c,t) = f == Rombo

cuadrado:: Figura -> Bool
cuadrado (f,c,t) = f == Cuadrado
