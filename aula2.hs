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

-- Ex1
maior :: Int -> Int -> Int
maior x y 
  | (x > y) = x
  | otherwise = y

-- Ex2
maiorVenda :: Int -> Int
maiorVenda n
   | (n == 0) = vendas 0
   | otherwise = maior(vendas n) (maiorVenda(n-1))

-- Ex3
maxVenda :: Int -> Int
maxVenda n
  | (n==0) = vendas 0
  | (maiorVenda n == vendas n) = n
  | otherwise = maxVenda(n-1)

-- Ex4
zeroVendas :: Int -> Int
zeroVendas n
   | (vendas n == 0) = n
   | otherwise = -1

-- Ex5
vendasIgual :: Int -> Int -> Int
vendasIgual s n
   | (vendas n == s) = n
   | otherwise = vendasIgual s(n-1) 
  
 -- Ex6 
 zeroVendas2 :: Int -> Int
 zeroVendas2 n
   | (vendasIgual 0 (n)) = n
   | otherwise = -1


