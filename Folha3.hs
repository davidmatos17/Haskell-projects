import Data.Char

--3.1
divprop :: Integer -> [Integer]
divprop n = [x | x <- [1..n-1], mod n x == 0]

--3.2
perfeitos :: Integer ->[Integer]
perfeitos n = [x | x <- [1..n], sum(divprop x) - x == x]

--3.3
pitagoricos :: Integer -> [(Integer ,Integer ,Integer)] 
pitagoricos n = [(x, y, z) | x <- [1..n] , y <- [1..n], z <- [1..n], x^2 +y^2 == z^2]

--3.4
primo :: Integer -> Bool
primo n = divprop n == [1,n]

--3.5 a)
binom :: Integer -> Integer -> Integer
binom n k = div (product [1..n]) (product[1..k] * product[1..n-k])

--3.5 b)
pascal :: Integer -> [[Integer]]
pascal n = [[binom i j | j <- [0..i]] | i <- [0..n]]


--  comprimento maior ou igual a 8 e pelo menos uma letra maiúscula, uma
--  letra minúscula e um algarismo.
checkSize :: String -> Bool
checkSize word = length word >= 8

hasUpper :: String -> Bool
hasUpper  = any isUpper

hasLower :: String -> Bool
hasLower = any isLower

hasDigit :: String -> Bool
hasDigit = any isDigit

forte :: String -> Bool
forte password = (checkSize password) && (hasUpper password) && (hasDigit password) && (hasLower password)

-- usando a versao com listas em compreensao

strong :: String -> Bool
strong pass = length pass >= 8 && or [isUpper c | c <- pass] && or [isLower c | c <- pass] && or [isDigit c | c <- pass]



--   PARTE DE EXERCICIOS DE RECURSAO

myand :: [Bool] -> Bool
myand [] = True
myand (x:xs) =  x && myand (xs)

myor :: [Bool] -> Bool
myor [] = False
myor (x:xs) = x || myor xs

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ myconcat xs

replicate1 :: Int -> a -> [a]
replicate1 0 _ = [] 
replicate1 n a = a : replicate1 (n - 1) a 


index :: [a] -> Int -> a
index [] _ = error "empty list"
index (x:_) 0 = x
index (_:xs) n = index xs(n-1)