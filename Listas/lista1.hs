osQuatroSaoIguais :: Int -> Int -> Int -> Int -> Bool
osQuatroSaoIguais x y z w = (x==y) && (y==z) && (z==w)
----------------------------------------------------------------------
quantosSaoIguais :: Int -> Int -> Int -> Int
quantosSaoIguais x y z 
   | (x == y) && (y == z) = 3
   | (x /= y) && (y /= z) = 0
   | otherwise = 2
----------------------------------------------------------------------
todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes x y z  
   | (x /= y) && (y /= z) && (z /= x) = True
   | otherwise = False
----------------------------------------------------------------------
-- Conjunto de testes para a função todosDiferentes
--  todosDiferentes 2 5 5
--  todosDiferentes 2 5 2
--  todosDiferentes 2 2 2
--  todosDiferentes 8 2 4
-----------------------------------------------------------------------
-- O conjunto de testes não funciona, 
-- pois não compara se n é diferente de p
-----------------------------------------------------------------------
todosIguais :: Int -> Int -> Int -> Bool
todosIguais x y z 
      | (x==y) && (y==z) && (x==z) = True
      | otherwise = False  
-------------------------------------------------------------------------
quantosSaoIguais2 :: Int -> Int -> Int -> Int
quantosSaoIguais2 x y z
    | todosDiferentes x y z = 0
    | todosIguais x y z = 3
    | otherwise = 2
-------------------------------------------------------------------------
elevadoDois :: Int -> Int
elevadoDois n = n ^ 2
-------------------------------------------------------------------------
elevadoQuatro :: Int -> Int
elevadoQuatro n = elevadoDois(elevadoDois n)
-------------------------------------------------------------------------
vendas :: Int -> Int
vendas 0 = 23
vendas 1 = 62
vendas 2 = 88
vendas _ = 99

vendaTotal :: Int -> Int
vendaTotal 0 = vendas 0 
vendaTotal n = vendas n + vendaTotal(n-1)