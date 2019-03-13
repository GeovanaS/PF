--      exemplo.hs--    comentario--
idade :: Int  -- Um valor inteiro constante
idade = 17
-- exemplo.hs
--
maiorDeIdade :: Bool  -- Usa a definicao de
maiorDeIdade = (idade>=18) --idade 

maiorDeIdade2 :: Int -> Bool
maiorDeIdade2 n 
    | (n > 17) = True
    | otherwise = False

quadrado :: Int -> Int   -- funcao que eleva num
quadrado x = x*x       -- ao quadradomini :: Int -> Int -> Int --funcao que mostra

mini a b                  --o menor entre
  | a <= b    = a       -- dois valores
  | otherwise = b

tresIguais :: Int -> Int -> Int -> Bool
tresIguais x y z = (x==y) && (y==z) && (z==x) 
----------------------------------------------
palindromo :: String -> Bool
palindromo s = s == reverse s

verificaTriangulo :: Int -> Int -> Int -> Bool
verificaTriangulo x y z 
   | (x + y) > z && (y + z) > x && (x + z) > y = True
   | otherwise = False

potencia :: Int -> Int -> Int
potencia b e = b ^ e

fib :: Int -> Int
fib n 
 | (n==0) = 0
 | (n==1) || (n==2) = 1
 | otherwise = fib(n-1) + fib(n-2)

--isPrime :: Int -> Bool
--isPrime n 
--  | (n%2 == 0) = True
--  | otherwise = False

maiorDeTres :: Int -> Int -> Int -> Int
maiorDeTres x y z
  | (x > y) && (x > z) = x
  | (y > x) && (y > z) = y
  |otherwise = z

insere :: Int -> [Int]
insere a [] = [a]
insere a (b:x)
  | a <= b = a:(b:x)
  | otherwise = b : insere a x 

listaOrd :: Int -> Int -> Int -> [Int]
listaOrd (a:x) = insere a (listaOrd x)
