map1 :: (a->b) -> [a] -> [b]
map1 f [] = []
map1 f (a:x) = f a : map f x

fold :: (a->a->a) -> [a] -> a
fold f [a] = a
fold f (a:x) = f a (fold f x)

-- Para saber tipo geral da função: 
-- > :t nomedaFunção

--foldr :: (a->b->b) -> b -> [a] -> b

--foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
--foldr f n [] = n
--foldr f n (x:xs) = f x (foldr n xs)

--achaElemento :: a -> [a] -> Bool

-- achaElemento :: Eq a => a -> [a] -> Bool
-- achaElemento :: Ord a => a -> [a] -> Bool
achaElemento :: (Ord a, Show a) => a -> [a] -> String
achaElemento a [] = show a
achaElemento a (x:xs)
     --  | (a==x) = True
         | (a==x) = show a
         | a <= x = achaElemento a xs
      --  | otherwise = achaElemento a xs

-- Classes de Tipo
--  Eq    Ord
--  ==    <=
--  /=    <
--        >

-- Lista 8

--Ex1 
head1 :: [a] -> a
head1 (a:x) = a
-- Tipo mais geral: head :: [a] -> a

tail1 :: [a] -> [a]
tail1 (a:x) = x
-- Tipo mais geral: tail :: [a] -> [a]

fst :: (a,b) -> a
fst (t,u) = t
-- Tipo mais geral: fst :: (a, b) -> a

shift :: ((a,b),c) -> (a,(b,c))
shift ((x,y),z) = (x,(y,z))
-- Tipo mais geral: shift :: ((a,b),c) -> (a,(b,c))

--Ex 2
concatena :: [[a]] -> [a]
concatena [] = []
concatena (x:xs) = x ++ (concatena xs)

-- Ex 3
inverte :: [a] -> [a]
inverte [] = []
inverte (x:xs) = (inverte xs) ++ [x]

-- Ex4
ultimo :: [a] -> a
ultimo [] = error "lista vazia"
ultimo [x] = x
ultimo (x:xs) = ultimo xs

inicio :: [a] -> [a]
inicio [] = []
inicio [x] = []
inicio (x:xs) = x: inicio xs

--Ex5
takeP :: Int -> [a] -> [a]
takeP 0 l = []
takeP n [] = []
takeP n (x:xs) = x: take(n-1) xs

dropP :: Int -> [a] -> [a]
dropP 0 l = l
dropP n [] = []
dropP n (x:xs) = drop(n-1) xs
