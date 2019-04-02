membro :: [Int] -> Int -> Bool
membro(x:xS) n 
       | (x==n) = True
       | otherwise = False

membroNum :: [Int] -> Int -> Int
membroNum(x:xS) n
     | (x == n) = 1 + membroNum xS n
     | otherwise = 0

membro1 :: [Int] -> Int -> Bool
membro1 (x:xS) n
       | (membroNum xS n > 0) = True
       | otherwise = False 

unico :: [Int] -> [Int]
unico l = unico2 l l

-- unico2 :: [Int] -> [Int]
--unico2 (x:xS) 
     