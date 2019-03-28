-- Exercicios sobre Listas

--Exemplo Aula
somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (a:x) = a + somaLista x

-- Lista 4
dobraLista :: [Int] -> [Int]
dobraLista [] = []
dobraLista (a:x) = 2 * a : dobraLista x

tamanho :: [Int] -> Int
tamanho [] = 0
tamanho (a:x) = 1 + tamanho x

produtoLista :: [Int] -> Int
produtoLista [] = 1
produtoLista [a] = a
produtoLista (a:x) = a * produtoLista x

andLista :: [Bool] -> Bool
andLista [] = False
andLista [a] = a
andLista (a:x) = a && (andLista x)

concatLista :: [[Int]] -> [Int]
concatLista [] = []
concatLista (a:x) = a ++ concatLista x

inverteLista :: [Int] -> [Int]
inverteLista [] = []
inverteLista(a:x) = inverteLista x ++ [a]
