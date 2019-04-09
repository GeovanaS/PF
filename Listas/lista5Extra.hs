-- Exercicios sobre listas em Haskell
retornaUltimo :: [Int] -> Int
retornaUltimo [a] = a
retornaUltimo(a:x) = retornaUltimo x

pegaPosicao :: Int -> [Int] -> Int
pegaPosicao n (x:xS) 
    | n == 1 = x
    | otherwise = pegaPosicao(n - 1) xS

pega :: Int -> [Int] -> [Int]
pega n [] = []
pega 0 l = []
pega n (x:xS) 
    | n == 1 = [x]
    | otherwise = x:pega(n-1)xS

retira :: Int -> [Int] -> [Int]
retira n [] = []
retira 0 l = l
retira n (x:xS) 
    | n == 1 = xS
    | otherwise = retira(n-1)xS

somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (a:x) = a + somaLista x

tamanho :: [Int] -> Int
tamanho [] = 0
tamanho (a:x) = 1 + tamanho x

mediaLista :: [Int] -> Float
mediaLista [] = 0
mediaLista l = (fromIntegral (somaLista l))/(fromIntegral(tamanho l))

maior :: Int -> [Int] -> Int -> Bool
maior a l 0 = False
maior a [] n = True
maior a (x:xs) n
   |a >= x = maior a xs n
   |otherwise = maior a xs (n - 1) 

pegaMaiores :: Int -> [Int] -> [Int]
pegaMaiores 0 l = []
pegaMaiores n [] = []
pegaMaiores n (x:xS) 
     | maior x xS n = x: pegaMaiores(n-1)xS
     | otherwise = pegaMaiores n xS

contaMaiores :: Int -> [Int] -> Int     
contaMaiores n [] = 0
contaMaiores n (x:xS)
      | x > n = 1 + contaMaiores n xS
      | otherwise = contaMaiores n xS

concatena :: [Int] -> [Int] -> [Int]
concatena [] x = x
concatena(x:xS) y = x:(concatena xS y)

intercala :: [Int] -> [Int] -> [Int]
intercala [] y = y
intercala(x:xS) y = x:(intercala y xS)

      

