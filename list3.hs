adicionaTupla :: (Int,Int,Int) -> (Int)
adicionaTupla (x,y,z) = (x+y+z)

shift :: ((Int,Int),Int) -> (Int,(Int,Int))
shift ((x,y),z) = (x,(y,z))

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

ordenaTupla :: (Int,Int,Int) -> (Int,Int,Int)
ordenaTupla (x,y,z) = (menor x y z, adicionaTupla(x,y,z) - menor x y z - maior x y z, maior x y z)

zeroVenda :: Int -> (Int,Bool)
zeroVenda n 
   | (vendas n == 0) = (0,True)
   | otherwise = (-1,False)


type Livro = (String,String,Int) 
titulo :: (String,String,Int) -> (String)
titulo (t,a,n) = t
autor :: Livro -> String
autor (t,a,n) = a
numeroIsbn :: Livro -> Int 
numeroIsbn (t,a,n) = n
