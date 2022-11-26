
data Color a = Black
             | White
             | RGB a a a

--funkce ktere manipuluji s datovzm typem 

isBlack::Color Int->Bool
isBlack Black = True
isBlack (RGB x y z) = x == 0 && y == 0 && z == 0
isBlack _ = False

-- datovy konstruktor muze obssa 