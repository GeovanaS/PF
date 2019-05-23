map1 :: (a->b) -> [a] -> [b]
map1 f [] = []
map1 f (a:x) = f a: map f x

foldr2 :: (a->a->a) -> [a] -> a
foldr2 f [a] = a
foldr2 f (a:x) = f a (foldr2 f x)

filter1 :: (t->Bool) -> [t] -> [t]
filter1 p [] = []
filter1 p (a:x)
       | p a = a: filter1 p x
       | otherwise = filter1 p x

-- 1. Defina a função concat usando fold
-- Hugs > concat [[1,2,3],[1],[3,3]]
-- > [1,2,3,1,3,3]
concat1 :: [[a]] -> [a]
concat1 l = foldr (++) [] l 

-- 2. Defina a função and usando o fold
-- Hugs > and [True,False,True,True]
-- > False
and1 :: [Bool] -> Bool
and1 l = foldr (&&) True l

-- 3. Use filter e a função par para definir uma função que retorne os numeros pares de uma lista
-- Hugs> pares [1,2,4,5]
-- [2,4]
par :: Int -> Bool
par n = mod n 2 == 0

pares :: [Int] -> [Int]
pares l = filter1 par l

-- 4. Implemente a função que inverte uma lista usando foldr
inverteLista :: [a] -> [a]
inverteLista l = foldr (\l x -> x ++ [l]) [] l

-- 5. Defina uma função que calcule a soma dos quadrados dos numeros de uma lista usando map e fold
somaQuad :: [Int] -> Int 
somaQuad l = foldr (+) 0 (map (^2) l)

-- 6. Defina uma funnção que calcule a soma dos quadrados dos numeros positivos de uma lista.
somaQuadPos :: [Int] -> Int
somaQuadPos l = foldr (+) 0 (map (^2) (filter1 (>0) l))

-- 7. Implemente a função que calcula o numero de elementos de uma lista usando apenas fold e map
numElem :: [a] -> Int
numElem l = foldr (+) 0 (map (\x -> 1) l)

-- 8. Defina a funcao que some os elementos de uma lista de listas (usando map e fold)
-- Hugs> somaListas [[1,2,3,4],[4], [3,2],[3]]
-- 22
somaListas :: [[Int]] -> Int
somaListas l = foldr (+) 0 (map (foldr(+) 0) l)

-- 9. Defina uma funcao que calcule o tamanho total dos elementos de uma lista de listas (usando map e fold)
-- Hugs> somaTamanhoListas [[1,2,3,4],[4], [3,2],[3]]
-- 8
somaTamanhoListas :: [[Int]] -> Int
somaTamanhoListas l = foldr (+) 0 (map numElem l)