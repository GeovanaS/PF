vendas :: Int -> Int
vendas 0 = 11
vendas 1 = 35
vendas 2 = 20
vendas 3 = 0
vendas _ = 55

inicio :: String
inicio =  "Semana    Vendas\n"

geraVendas :: Int -> String
geraVendas 0 = "\nSemana " ++ show 0 ++ "    " ++ show(vendas 0)
geraVendas n =  geraVendas(n-1) ++  "\nSemana " ++ show n ++ "    " ++ show(vendas n) 

vendaTotal :: Int -> Int
vendaTotal 0 = (vendas 0)
vendaTotal n =  (vendas n + vendaTotal(n-1)) 

tabela :: Int -> String
tabela n = inicio ++ geraVendas n ++ "\nTotal: " ++ show(vendaTotal n) ++ "\n"

----------------------------------------------------------------------------------
-- Lista 2 

-- Lista 2
maior :: Int -> Int -> Int
maior x y
   | (x > y) = x
   | otherwise = y

maiorVenda :: Int -> Int
maiorVenda n 
  | (n==0) = vendas 0
  | otherwise = maior(vendas n)  (maiorVenda(n-1))

maxVenda :: Int -> Int
maxVenda n  
  |  (n == 0) = 0 
  |  (maiorVenda n == vendas n) = n
  |  otherwise = maxVenda(n-1) 

zeroVendas :: Int -> Int
zeroVendas n
   | (vendas n == 0) = n
   | otherwise = -1

vendasIguais :: Int -> Int -> Int
vendasIguais s n 
   | (vendas n == s) = n
   | otherwise = vendasIguais s(n-1) 

zeroVendas2 :: Int -> Int -> Int
zeroVendas2 n m
  | (vendas n == 0) && (vendas m == 0) = 0
  | (vendasIguais n m == vendas n)  = -1
  | otherwise = -1

maiorVenda2 :: Int -> Int -> Int
maiorVenda2 n m
  | (n == m) = vendas n
  | otherwise = maior(vendas n) (maiorVenda2 n (m-1))

maxVenda2 :: Int -> Int -> Int
maxVenda2 n m
 | (n == m) = n
 | (maiorVenda2 n m == vendas n) = n
 | otherwise = maxVenda2 m(n-1)

zeroVendas3 :: Int -> Int -> Int
zeroVendas3 n m
 | (vendas n == 0) = n
 | (m == n) = -1
 | otherwise = zeroVendas3 m (n-1)

vendasIguais2 :: Int -> Int -> Int -> Int
vendasIguais2 s n m
 | (vendas n == s) = n
 | (m == n) = -1
 | otherwise = vendasIguais2 s m(n-1)

fatorial :: Int -> Int
fatorial n
  | (n == 0) = 1
  | (n == 1) = 1
  | otherwise = n * fatorial(n - 1)

produto :: Int -> Int -> Int
produto m n
  | (n <= 0) = 1
  | (m == n) = n 
  | otherwise = n * produto m (n-1)

fib :: Int -> Int
fib n 
 | (n == 0) = n
 | (n == 1) = n
 | otherwise = fib(n-1)+fib(n-2)
