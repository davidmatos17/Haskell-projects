--3.1
divprop :: Integer -> [Integer]
divprop n = [x | x <- [1..n], mod n x == 0]

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
pascal :: Integer -> [Integer]
pascal n = [binom n k | k <- [0..n]]

forte :: String -> Bool
forte password = 