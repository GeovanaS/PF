adicionaTupla :: (Int,Int,Int) -> (Int)
adicionaTupla (x,y,z) = (x+y+z)

shift :: ((Int,Int),Int) -> (Int,(Int,Int))
shift ((x,y),z) = (z,(x,y))

maior :: Int -> Int -> Int -> Int
maior x y z 
  | (x > y) && (x > z) = x
  | (y > x) && (y > z) = y
  | otherwise = z

menor :: Int -> Int -> Int -> Int
menor x y z 
  | (x < y) && (x < z) = x
  | (y < x) && (y < z) = y
  | otherwise = z

minEmax :: Int -> Int -> Int -> (Int,Int)
minEmax x y z = (maior x y z, menor x y z)

vendas :: Int -> Int
vendas 0 = 11
vendas 1 = 35
vendas 2 = 20
vendas 3 = 0
vendas _ = 55

zeroVenda :: Int -> (Int,Bool)
zeroVenda n 
   | (vendas n == 0) = (0,True)
   | otherwise = (-1,False)

livro :: (String,String,Int) -> (String,String,Int)
livro (t,a,n) = (t,a,n)
titulo :: (String) -> (String)
titulo (t) = t
autor :: (String) -> (String)
autor (a) = a
numeroIsbn :: (Int) -> (Int) 
numeroIsbn (n) = n



