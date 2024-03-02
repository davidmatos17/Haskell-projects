classifica :: Int -> String
classifica n 
    | 0 <= n && n <= 9 = "reprovado"
    | 10 <= n  && n <= 12 = "suficiente"
    | 13 <= n && n <= 13 = "bom"
    | 16 <= n && n <= 18 = "muito bom"
    | 19 <= n && n <= 20 = "muito bom com distincao"
    |otherwise = "introduza um valor entre 0 e 20."


imc :: Float -> Float -> String
imc peso alt 
    | valor < 18.5 = "baixo peso"
    | 18.5 <= valor && valor< 25 ="peso normal"
    | 25 <= valor && valor < 30 ="excesso de peso"
    | 30 <= valor ="obsesidade"
    |otherwise = "impossivel calcular"
        where valor = peso / (alt^2)

max3 :: Ord a => a -> a -> a -> a
max3 x y z = max (max x y) z
  
min3 :: Ord a => a -> a -> a -> a
min3 x y z = min (min x y) z
    

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False
    
-- funcao condicional
safetail1 :: [a] -> [a]
safetail1 lista = if (length lista) == 0 then [] else tail lista

-- funcao com equacoes
safetail2 :: [a] -> [a]
safetail2 lista 
    |(length lista) == 0 = []
    |otherwise = tail lista

-- funcao com padroes   MUITO UTIL PARA OBTER/MODIFICAR/RETIRAR VALORES EM POSICOES ESPECIFICAS 
safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 (_:xs) = xs


-- 2.9
-- a)
curta1 :: [a] -> Bool
curta1 lista = if (length lista) < 3 then True else False

-- b)
curta2 :: [a] -> Bool
curta2 [] = True
curta2 [_] = True
curta2 [_ , _] = True
curta2 (x:y:xs) = False


-- 2.10
-- a)
mediana :: Ord a => a -> a -> a -> a
mediana x y z
    | (y <= x && x <= z) || (z <= x && x <= y) = x
    | (x <= y && y <= z) || (z <= y && y <= x) = y
    | otherwise = z

-- b)
-- precisa de ter os dois tipos NUM e ORD por causa das funcoes chamadas
mediana2 :: (Num a, Ord a) => a -> a -> a -> a
mediana2 x y z = x + y + z - max x (max y z) - min x (min y z)


{- 

Na classe "Eq" estao inseridos os tipos :
 -> Int 
 -> Char
 -> (_,_)
 -> Float

   A Classe Ord apenas podemos operar com tipos ordenaveis, usamos operacoes matematicas

   Exemplos : < , > , == , <= , >= ...

-}