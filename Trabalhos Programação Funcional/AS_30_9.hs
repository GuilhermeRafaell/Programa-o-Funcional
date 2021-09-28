--(1)
import Data.Char
--(a)
primeiros :: [(a,b)] -> [a]
primeiros [] =[]
primeiros (x:xs) = fst x : primeiros xs

primeiros1 :: [(a,b)] -> [a]
primeiros1 [] = []
primeiros1 x = map (fst) x

--(b)
maiuscula :: String -> String
maiuscula [] = []
maiuscula (x:xs) = toUpper x : maiuscula xs

maiuscula1 :: String -> String
maiuscula1 x = map (toUpper) x

--(c)
dobra :: Num a => [a] -> [a]
dobra [] = []
dobra (x:xs) = 2*x:dobra xs

dobra1 :: Num a => [a] -> [a]
dobra1 x = map (*2) : dobra xs

--(d)
hora_em_seg :: [Float] -> [Float]
hora_em_seg [] = []
hora_em_seg (x:xs) = 3600*x:hora_em_seg xs

hora_em_seg1 :: [Float] -> [Float]
hora_em_seg1 x = map (*3600) x 

--(7)
--(a)
pares :: [Int] -> [Int]
pares [] = []
pares (x:xs)
 | even x = x : pares xs
 | otherwise = pares xs

pares1 :: [Int] -> [Int]
pares1 x = filter even x 

--(b)
alfa :: String -> String
alfa [] = []
alfa (x:xs)
   | isAlpha x = x : alfa xs
   | otherwise = alfa xs

alfa2 :: String -> String
alfa2 x = filter isAlpha x

--(c) 
rm_char :: Char -> String -> String
rm_char a [] = []
rm_char a (x:xs)
   | a == x = rm_char a xs
   | otherwise = x : rm_char a xs

rm_char1 :: Char -> String -> String
rm_char1 a x = filter (a /=) x

--(d)
acima :: Int -> [Int] -> [Int]
acima a [] = []
acima a (x:xs)
   | a >= x = acima a xs
   |otherwise = x : acima a xs

acima1 :: Int -> [Int] -> [Int]
acima1 a x = filter (a <) x

--(e)
desiguais :: Eq t => [(t,t)] -> [(t,t)]
desiguais [] = []
desiguais (x:xs)
    | fst x == snd x = desiguais xs
    | otherwise = x : desiguais xs

desiguais1 :: Eq t => [(t,t)] -> [(t,t)]
desiguais1 b = filter (\ (x,y) -> x /= y) b

--(8)
--(a)
produto :: Num a => [a] -> a
produto [] = 0
produto (x:xs) = x * produto xs

produto1 :: Num a => [a] -> a
produto1 x = foldr (*) 0 x

--(b)
e_logico :: [Bool] -> Bool
e_logico [] = False
e_logico (x:xs) = x && e_logico xs

e_logico1 :: [Bool] -> Bool
e_logico1 ls = foldr (&&) False xs 

--(c)
concatena :: [String] -> String
concatena [] = ""
concatena (x:xs) = x ++ concatena xs

concatena1 :: [String] -> String
concatena1 x = foldr (++) "" x

--(d) 
maior :: (Num a, Ord a)  => a -> [a] -> a
maior a [] = a
maior a (x:xs) 
   |a < x = maior x xs
   |otherwise = maior a xs

maior1 :: (Num a, Ord a)  => a -> [a] -> a
maior1 x y = foldr (max) x y


