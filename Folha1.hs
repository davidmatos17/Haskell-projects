incr :: Int -> Int
incr x = x+1    

triple :: Int -> Int
triple x = 3*x

square :: Int -> Int
square x = x*x

cube :: Int -> Int
cube x = x*x*x

welcome :: String -> String
welcome name = "Hello, " ++ name ++ "!"

count :: String -> String
count str = show (length str) ++ " characters."

lefthalf :: [a] -> [a]
lefthalf x = take(div (length x) 2) x

righthalf :: [a] -> [a]
righthalf (x) = drop(div (length x) 2) x

second :: [a] -> a
second lista = head (drop 1 lista)

last :: [a] -> a
last lista = head(reverse lista)

last_1 :: [a] -> a
last_1 lista = head (reverse(tail lista))

removeLast :: [a] -> [a]
removeLast lista = reverse (drop 1 (reverse lista))

middle :: [a] -> a
middle lista = lista !! middleIndex
        where middleIndex = div (length lista) 2

checkTriangle :: Float -> Float -> Float -> Bool
checkTriangle a b c = a < b + c && b < a + c && c < a + b 

triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c = sqrt(s*(s-a)*(s-b)*(s-c))
    where s = (a+b+c) / 2 

checkPalindrome :: String -> Bool
checkPalindrome string = string == reverse string 
