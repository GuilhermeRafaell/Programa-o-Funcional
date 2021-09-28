esfera :: Int -> Float
esfera r = div(4*pi*(r*r*r)) 3

hipotenusa :: Int -> Int -> Float
hipotenusa a b = sqrt(a*a+b*b)

distancia :: Int -> Int -> Int -> Int -> Float
distancia xa ya xb yb = sqrt((xb-xa)^2 + (yb-ya)^2)

quadrado :: Int -> Int
quadrado x = x^2

quarta_potencia :: Int -> Int
quarta_potencia x = (quadrado x)*(quadrado x)

formula_logica :: (Bool,Bool) -> Bool
formula_logica (p,q) = (p||q) && not(p&&q)