iSort :: [Int] -> [Int]
iSort [] = []
iSort(x:xS) = ins x (iSort xS)

ins :: Int -> [Int] -> [Int]
ins n [] = [n]
ins n (x:xS)
    | (n <= x) = n:(x:xS)
    | otherwise = x:ins n xS

maiorEmenor :: [Int] -> (Int,Int)
maiorEmenor x = (head(iSort x),head(reverse(iSort x)))

maiorEmenor1 :: [Int] -> (Int,Int)
maiorEmenor1 l = let l2 = iSort l   
                  in(head l2, head(reverse l2) )

-- Retira numeros repetidos da lista
ins1 :: Int -> [Int] -> [Int]
ins1 n [] = [n]
ins1 n (x:xS)
      | (x==n) = n:xS
      | (n < x) = n:(x:xS)
      | otherwise = x:ins1 n xS

iSort1 :: [Int] -> [Int]
iSort1 [] = []
iSort1(x:xS) = ins1 x (iSort1 xS)

-- Ordena de forma decrescente
ins2 :: Int -> [Int] -> [Int]
ins2 n [] = [n]
ins2 n (x:xS)
      | (n > x) = n:(x:xS)
      | otherwise = x:ins2 n xS

iSort2 :: [Int] -> [Int]
iSort2 [] = []
iSort2(x:xS) = ins2 x (iSort2 xS)