maiorEmenor :: [Int] -> (Int,Int)
maiorEmenor x = (primeiro(iSort x),ultimo(iSort x))

iSort :: [Int] -> [Int]
iSort [] = []
iSort (a:x) = ins a (iSort x)

ins :: Int -> [Int] -> [Int]
ins a [] = [a]
ins a (b:x)
    | a <= b = a:(b:x)
    | otherwise = b: ins a x

primeiro :: [Int] -> Int
primeiro (a:x) = a

ultimo :: [Int] -> Int
ultimo [a] = a
ultimo(a:x) = ultimo x

maiorEmenor1 :: [Int] -> (Int,Int)
maiorEmenor1 x = let s = iSort x          
                 in (primeiro s,ultimo s)

somaPares :: [(Int, Int)] -> Int
somaPares []  = 0
somaPares (a:x) = c + d + somaPares x
     where 
     c = fst a
     d = snd a

somaPares1 :: [(Int, Int)] -> Int
somaPares1 [] = 0
somaPares1 ((c,d):x) = c + d + somaPares1 x

dobra xs = [ 2*x | x <- xs]

membro :: [Int] -> Int -> Bool
membro xs v
 | l == [] = False
 | otherwise = True
 where
  l = [x | x <-xs, v==x]

--map recebe como argumentos uma funcao e uma lista e aplica essa funcao a todos os elementos da lista
map f xs = [f x | x <-xs]

retiraEspacos xs = [ x | x <-xs , x /= " "]