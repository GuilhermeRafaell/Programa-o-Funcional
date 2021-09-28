--Ex 8
conta_digitos :: Int -> Int
conta_digitos 0 = 1
conta_digitos n
 |(n<10) = conta_digitos 0
 |otherwise = 1 + conta_digitos (n `div` 10)

-- Ex 10
potencia :: (Int,Int) -> Int
potencia (b,e) =  b^e
potencia (b,0) = 1
potencia (0,_) = 0

--Ex 11
ackermann :: (Int,Int) -> (Int,Int)
ackermann (m,n)  
 |m == 0 = (0,n+1)
 |m > 0 && n == 0 = (m-1,1)
 |m > 0 && n > 0  = (m,n-1)

--2(h)
ultimo :: [Int] -> Int
ultimo (x:xs) = head (reverse xs) 

--2(i)
duplica :: [Int] -> [Int]
duplica [] = []
duplica (x:xs) = x:x:(duplica xs)

--2(k)
substituir_todos :: Int -> Int -> [Int] -> [Int]
substituir_todos a b [] = []
substituir_todos a b (x:xs)
 | a == x = b : substituir_todos a b xs
 | otherwise = x : substituir_todos a b xs

--2(n)
maior :: [Int] -> Int
maior [x] = x
maior (x:xs)
 | x > maior xs = x 
 | otherwise = maior xs

--3(b)
uniao :: Eq t => [t] -> [t] -> [t]
uniao as bs = as ++ [b | b <- bs, not (pertence b as)]


--Ex 3(c)
inter :: [Int] -> [Int] -> [Int]
inter []  = []
inter  [] = []
inter (x:xs) (y:ys)
 | x < y = inter xs (y:ys)
 | x == y = x : inter xs ys
 | x > y = inter (x:xs) ys
 
--Ex 4(c)
maius :: String -> String
maius [] = []
maius (x:xs) = toUpper x : maius xs

maius2 :: String -> (String, String)
maius2 x = (x, maius x)

--Ex 7
converte :: [Int] -> Int
converte [] = 0
converte x = head x * 2 ^ (length x - 1) + converte (tail x)

--Ex 8
digitos :: Int -> [Int]
digitos 0 = []
digitos x = digitos (x `div` 10) ++ [x `mod` 10]

import Data.Char
