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
zipp [] y = []
zipp x [] = []
zipp (x:xS) (y:yS) = (x,y):(zipp xS yS)

zipp3 :: [Int] -> [Int] -> [Int]  -> [(Int,Int,Int)]
zipp3 x [] [] = []
zipp3 [] y [] = []
zipp3 [] [] z = []
zipp3 (x:xS) (y:yS) (z:zS) = (x,y,z) :(zipp3 xS yS zS)

unZipp :: [(Int,Int)] -> ([Int],[Int])
unZipp l = (unzipEsq l, unzipDir l)

unzipEsq, unzipDir :: [(Int,Int)] -> [Int]
unzipEsq [] = []
unzipEsq ((a,b):xS) = a:(unzipEsq xS)
unzipDir [] = []
unzipDir ((a,b):xS) = b:(unzipDir xS)