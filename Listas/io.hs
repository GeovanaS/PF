-- Compilação:
-- > ghc io.hs -o io 
-- > ./io

main :: IO()

main = do
	putStrLn "Qual o seu nome?"
	nome <- getLine
	putStr nome
	putStr "\n"

leNomeESobrenome :: IO String
leNomeESobrenome = do
	 putStrLn "Qual o seu nome?"
	 nome <- getLine
	 putStrLn "Qual o seu sobrenome?"
	 sob <- getLine
	 return (nome++" "++sob)
