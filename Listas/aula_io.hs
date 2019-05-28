main :: IO()

leNomeESobrenome :: IO String
leNomeESobrenome = do
	 putStrLn "Qual o seu nome?"
	 nome <- getLine
	 putStrLn "Qual o seu sobrenome?"
	 sob <- getLine
	 return (nome++" "++sob)


--main = do
--	 resp <- leNomeESobrenome
--	 putStrLn resp

ins :: Ord a => a -> [a] -> [a]
ins x [] = [x]
ins x (y:ys) 
	| x <= y = x : y : ys
    | otherwise = y : ins x ys

iSort :: Ord a => [a] -> [a]
iSort []     = []
iSort (x:xs) = ins x (iSort xs)

leNomes :: IO [String]
leNomes = do
	nome <- getLine
	if(nome=="")
		then return []
		else do
			nomes <- leNomes
			return (nome:nomes)

main = do 
  nomes <- leNomes
  putStr ((foldr(++)[] .
  	       map (++"\n").
  	       iSort) nomes)	 

-- pode ser reescrito como:
-- putStr (foldr(++)[] (map(++"\n") (iSort nomes)))
-- (iSort nomes) : ordena nomes
-- (map(++"\n") : adiciona "\n" ao final de cada string
-- foldr(++)[] : pega todas as strings e junta em uma só