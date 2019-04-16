somaPares :: [(Int,Int)] -> Int
somaPares [] = 0
somaPares (a:xS) = fst a + snd a + somaPares xS

somaPares1 :: [(Int,Int)] -> Int
somaPares1 [] = 0
somaPares1 ((c,d):x) = c + d + somaPares1 x

somaTripla :: [(Int,Int,Int)] -> Int
somaTripla [] = 0
somaTripla ((a,b,c):xS) = a + b + c + somaTripla xS

somaTuplas :: [((Int,Int),(Int,Int))] -> Int
somaTuplas [] = 0
somaTuplas (((a,b),(c,d)):xS) = (a + b) + (c + d) + somaTuplas xS

zipp :: [Int] -> [Int] -> [(Int,Int)]
zipp [][] = 0
zipp (a:x) (b:xS) = [fst a] ++ [fst b] : zipp((a,b):x)

-- zipp3 :: [Int] -> [Int] -> [Int] -> [(Int,Int,Int)]
-- zipp3 [] a b = []
-- zipp3 a [] b = []
-- zipp3 a b [] = []

