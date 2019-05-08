--dobra :: [Int] -> [Int]
--dobra [] = []
--dobra (a:xs) = (2*a):dobra x

--triplo :: [Int] -> [Int]
--triplo [] = []
--triplo (a:xs) = (3*a):triplo x

-- Função de alta ordem : função que recebe outras funções como argumento

--exemplo:
time2,time3 :: Int -> Int
time2 n = 2*n
time3 n = 3*n

mapInt :: (Int->Int) -> [Int] -> [Int]
mapInt f [] = []
mapInt f (a:x) = f a : mapInt f x

dobro l = map time2 l
triplo l = map time3 l

-- > mapInt (time2) [1,2,3,4]
-- > [2,4,6,8]

-- (*) :: Int -> (Int->Int)

-- > mapInt (*3) [1,2,3,4]
-- > [3,6,9,12]

-- time2 = *2
-- time3 = *3

-- Tipo polimorfo da função map
map1 :: (a -> b) -> [a] -> [b]
map1  f [] = []
map1 f (a:x) = f a : map1 f x

-- Lista 7

--Ex1
total :: (Int -> Int) -> Int -> Int
total f 0 = 0
total f n = f n + total f n

--Ex2
foldInt :: (Int -> Int -> Int) -> [Int] -> Int
foldInt f [] = error "lista vazia"
foldInt f [a] = a
foldInt f (x:xs) = f x (foldInt f xs)

soma :: Int -> Int -> Int
soma x y = x + y

-- Ex3
mult :: Int -> Int -> Int
mult x y = x * y

sub :: Int -> Int -> Int
sub x y = x - y

-- Ex4
filterString :: (Char -> Bool) -> [Char] -> [Char]
filterString f (x:xs)
        | (f x)= x:(filterString f xs) 
        | otherwise = (filterString f xs)

naoEspaco :: Char -> Bool
naoEspaco x = x /= ' '

-- > filterString (/=' ') "Andre Du Bois"

-- Ex5
naoNovaLinha :: Char -> Bool
naoNovaLinha x = x /= '\n'

naoVogal :: Char -> Bool
naoVogal x = (x/= 'a' && x/='e' && x/='i' && x/='o' && x/='u')

-- Ex6
--somaQuad :: [Int] -> Int
--somaQuad (x:xs) = (2*foldInt soma xs x)
--somaQuad (x:xs) = x*x
 
-- Ex7
maiorZero :: (Int->Int) -> Int -> Bool
maiorZero f n 
        | (f n > 0) = True
        | (f n < 0) = False
        | otherwise = maiorZero f (n-1) 


--duasVezes :: (Int->Int) -> Int -> Int

