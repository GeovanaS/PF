-- Tipo Algebricos Recursivos

-- Arvores
data Arvore a = Folha a | No a (Arvore a) (Arvore a)
--
--    1
--  -    -
--  2    1
-- -  -  
-- 3  4

--arv1 :: Arvore Int
--arv1 = No 1 (No 2(Folha 3)(Folha 4)) 

-- Lista
data Lista a = Vazio |Cons a (Lista a)
     deriving(Eq,Show)

-- [1,2,3,4] = 1:2:3:4:[]
--              |       |
--              cons  	vazio

lista1 :: Lista Int
lista1 = Cons 1 (Cons 2 (Cons 3 (Cons 4 Vazio)))


tamanho :: Lista a -> Int
tamanho Vazio = 0
tamanho (Cons x xs) = 1 + (tamanho xs) 


-- [[1,2],[],[4]]

lista2 :: Lista (Lista Int)
lista2 = Cons (Cons 1(Cons 2 Vazio)) (Cons Vazio (Cons (Cons 4 Vazio)Vazio))

-- map :: (a->b) -> [a] -> [b]
-- map f [] = []
-- map f (x:xs) = f x: map f xs

mapl :: (a->b) -> Lista a -> Lista b
mapl f Vazio = Vazio
mapl f (Cons x xs) = Cons (f x) (mapl f xs)  

foldr1l :: (a -> a -> a) -> Lista a -> a 
foldr1l f (Cons a Vazio) = a 
foldr1l f (Cons x xs) = f x (foldr1l f xs)

filterl :: (a -> Bool) -> Lista a -> Lista a
filterl p Vazio = Vazio
filterl p (Cons x xs) 
        | p x   = Cons (p x) (filterl p xs) 
        | otherwise = (filterl p xs)