membro :: [Int] -> Int -> Bool
membro [] n = False
membro(x:xS) n 
       | (x == n) = True
       | otherwise = membro xS n

membroNum :: [Int] -> Int -> Int
membroNum [] n = 0
membroNum(x:xS) n
     | (x == n) = 1 + membroNum xS n
     | otherwise = membroNum xS n

membro1 :: [Int] -> Int -> Bool
membro1 (x:xS) n
       | membroNum (x:xS) n == 0 = False
       | otherwise = True

removeLista :: [Int] -> Int -> [Int]
removeLista [] n = []
removeLista (x:xS) n
      | n == x = removeLista xS n
      | otherwise = x:removeLista xS n

unico :: [Int] -> [Int]      
unico [] = []
unico (x:xS)
     | (membroNum(x:xS) x == 1) = x : unico xS
     | (membroNum(x:xS) x > 1) = unico(removeLista(x:xS) x)

membro2 :: [Int] -> Int -> Bool
membro2 x n = membro2(iSort x) n

ins :: Int -> [Int] -> [Int]
ins n [] = [n]
ins n (x:xS)
    | n >= x = x: ins n xS
    | otherwise = [n,x] ++ xS

iSort :: [Int] -> [Int]
iSort [] = []
iSort(x:xS) = ins x (iSort xS)