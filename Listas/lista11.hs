-- Lista 11

-- Ex1
multArvore :: Arvore Int -> Int
multArvore (Folha n) = n * 2
multArvore (Nodo n a1 a2) = n * 2 + multArvore a1 + multArvore a2

-- Ex2
contArvore :: Arvore Int -> Int
contArvore (Folha n) = 1
contArvore (Nodo n a1 a2) = 1 + contArvore a1 + contArvore a2

-- Ex3
maiorElem :: Arvore Int -> Int
maiorElem (Folha n) = n
maiorElem (Nodo n a1 a2)
          | n > (maiorElem a1) && n > (maiorElem a2) = n
          | (maiorElem a1) > (maiorElem a2) = maiorElem a1
          | otherwise = maiorElem a2


-- Ex4          
buscaArvore :: Arvore Int -> Int -> Bool
buscaArvore (Folha n) x = (n == x)
buscaArvore (Nodo n a1 a2) x
           | (n == x) = True
           | (buscaArvore a1 x) || (buscaArvore a2 x) = True
           | otherwise = False 

-- Ex5
quantArvore :: Arvore Int -> Int -> Int
quantArvore (Folha n) x = if(x==n) then 1 else 0 
quantArvore (Nodo n a1 a2) x 
            | (x == n) = 1 + (quantArvore a1 x) + (quantArvore a2 x)
            | otherwise = (quantArvore a1 x) + (quantArvore a2 x)

-- Ex6
refleteArvore :: Arvore Int -> Arvore Int
refleteArvore (Folha n) = (Folha n)
refleteArvore (Nodo n a1 a2) = Nodo n  (refleteArvore a2)  (refleteArvore a1)

-- Ex7
altArvore :: Arvore Int -> Int
altArvore (Folha n) = 0
altArvore (Nodo n a1 a2) = 1 + max(altArvore a1) (altArvore a2)

-- Ex8
listArvore :: Arvore Int -> [Int]
listArvore (Folha n) = [n]
listArvore (Nodo n a1 a2) = [n] ++ (listArvore a1) ++ (listArvore a2)

-- Ex9
-- mapTree :: (a -> b) -> Arvore a -> Arvore b
-- mapTree f (Folha n) = (Folha n)
-- mapTree f (Nodo n a1 a2) = Nodo (f n) (mapTree f a1) (mapTree f a2)