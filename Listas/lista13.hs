--List Comprehension

somaTuplas :: [(Int,Int)] -> [Int]
somaTuplas l = [a + b | (a,b) <- l]

addOrdPairs :: [(Int,Int)] -> [Int]
addOrdPairs = [a+b | (a,b) <- l, a < b] 

-- Lista 13
--1
filterLC :: (a->Bool) -> [a] -> [a]
filterLC f l = [x, x <- l, f x]

--2
mapLC :: (a->b) -> [a] -> [b]
mapLC f l = [f x | x <- l]

--3
removeEspacos :: String -> String
removeEspacos l = [x | x<-l, x/=' ']

--4
sings :: [[a]] -> [a]
sings l = [x | x:xs<-l, lenght(x:xs)==1]

--5
matches :: Int -> [Int] -> [Int]
matches n l = [x | x <- l, x==n]

--6
elemento :: Int -> [Int] -> Bool
elemento n l = [b | x <- l,lenght(matches(n) (l))/=0]

--7
divisores :: Int -> [Int]
divisores a = [x | x <- lista(a)(a), mod a x == 0]

lista :: Int -> Int -> [Int]
lista 0 a = []
lista n 0 = []
lista 1 a = [1]
lista n a
      | mod n a == 0 = a:lista(n)(a-1)
      | otherwise = lista(x)(n-1)

-- 8
isPrime :: [Int] -> [Int]
isPrime l = [x | x <- l, lenght(divisores(x))==2]

-- 9
quickSort :: Ord a => [a] -> [a]
quickSort (a:l) = quickSort (menores a l) ++ [a] ++ quickSort (maiores a l)

menores :: [Int] -> [Int] -> [Int]
menores a l = [x | x <- l , x < a]

maiores :: [Int]->[Int] -> [Int]
maiores a l = [x | x <- l , x > a]
