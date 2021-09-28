

duplica_lista :: [Int] -> [Int]
duplica_lista (x:xs) = [2*n | n <- (x:xs)]

quadrado_lista :: [Int]
quadrado_lista = [2^x | x <- [1..30]]

uniao :: [Int] -> [Int] -> [Int]
uniao (x:xs) (a:as) = concat [(x:xs),(a:as)]