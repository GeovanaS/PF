-- Exercicios sobre Type Classes

-- Exercicio 1
data Lista a = Cons a (Lista a) | Vazio

instance Show a => Show (Lista a) where
   show = showLista 

showLista :: Show a => Lista a -> String
showLista Vazio = "]"
showLista (Cons x xs) = (show x) ++ ","  ++ (show xs) 

-- Cons 1 (Cons 2 (Cons 3 Vazio))
-- [1,2,3]

-- Exercicio 2
data Tree a = Nodo a (Tree a) (Tree a) | Folha a

instance Show a => Show (Tree a) where
   show = showTree

showTree :: Show a => Tree a -> String
showTree (Folha n) = (show n)  
showTree (Nodo n a1 a2) = (show n) ++ ","  ++ (show a1) ++ "," ++ (show a2) 


-----------------------------------------------------------------------------
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x < y     = x:y:ys
                | otherwise = y:(insert x ys)

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)
