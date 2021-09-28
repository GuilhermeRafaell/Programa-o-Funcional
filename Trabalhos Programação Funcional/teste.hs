dobro :: Int -> Int
dobro x = 2 * x

quadruplo :: Int -> Int
quadruplo x = dobro(dobro x)


soma2 :: Int -> Int -> Int
soma2 a b = a+b


soma4 :: Int -> Int -> Int -> Int -> Int
soma4 a b c d = (soma2 a b) + (soma2 c d)


misterio :: Int -> Int -> Int -> Int ->Int
misterio x y z w = soma2 (soma2 x y)(soma2 z w)


hip :: Int -> Int -> Int
hip a b = sqrt (a*a + b*b)

