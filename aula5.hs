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

 