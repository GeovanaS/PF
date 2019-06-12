--data Arvore a = Folha a | No a (Arvore a) (Arvore a)

data Arvore a = No a [Arvore a]

--data Arvore a = Folha a | No a [Arvore a]

-- Arv1:
--    1
--  -    -
--  -    5
--  -   - - - 
--  3   6 7 8
--  -
--  4

arv1 :: Arvore Int
arv1 = No 1 [No 3 [No 4 []], No 5 [No 6[],No 7[],No 8[]] ]

arvEx :: Arvore Int
arvEx = No 1 [No 2 [No 2 []], No 2 [No 1[] ]]

somaArvore :: Arvore Int -> Int
somaArvore (No a l) = a - foldr(+) 0 (map somaArvore l)


-- Lista de exercicios 11 usando a definicao 
-- data Arvore a = No a [Arvore a]
multArvore :: Arvore Int -> Int
multArvore (No a l) =  foldr(*) (2) (map multArvore l)

contArvore :: Arvore Int -> Int
contArvore (No a l) = foldr(+) 1 (map contArvore l)

maiorArvore :: Arvore Int -> Int
maiorArvore (No a l) = foldr max a (map maiorArvore l)

listaArvore :: Arvore Int -> [Int]
listaArvore (No a l) = foldr(++) [a] (map listaArvore l)

