import Data.Char
import System.IO
import Prelude 
import Data.List
import System.Random

data Direcao = Cima | Baixo | Esq | Dir
    deriving(Eq)

type Tab = [[Int]]   

-- Tabuleiro inicial 
tabInicial :: Tab
tabInicial = [[0, 0, 0, 0],
              [0, 0, 0, 0],
              [0, 0, 0, 2],
              [0, 2, 0, 0]]

-- Pega os valores diferentes de zeros e concatena com os zeros replicados
concatena :: [Int] -> [Int]
concatena x = somaIguais(filter (> 0) x) ++ (replicate(length x - length (somaIguais (filter (> 0) x) ) ) 0) 

-- Soma elementos adjacentes em uma direção 
somaIguais :: [Int] -> [Int]
somaIguais (x:y:xs)
              | x==y = x + x : somaIguais xs
              | otherwise = x: somaIguais(y:xs)
somaIguais x = x


-- Pega a direcao para mover os elementos
pegaDirecao :: IO Direcao
pegaDirecao = do 
          dir <- hGetChar stdin 
          case (ord dir) of 
              101 -> return Esq 
              100 -> return Dir 
              99 -> return Cima
              98 -> return Baixo
              _   -> do putStrLn ""
                        pegaDirecao


-- Verifica se jogador venceu o jogo
verificaVitoria :: Tab -> Bool
verificaVitoria tab = [] /= filter (==2048) (concat tab)

-- Imprime tabuleiro
imprimeTab :: Tab -> IO()
imprimeTab tab = do putStr $ "\ESC 2"
                    mapM_ (putStrLn . imprimeLinha) tab 

-- Imprime as linhas do tabuleiro
imprimeLinha :: [Int] -> String
imprimeLinha [] = ""
imprimeLinha (x:xs) = show x ++ " " ++ imprimeLinha xs 

-- Cria novo tabuleiro
novoTab :: Tab -> IO Tab
novoTab tab = do e <- pegaDirecao    
                 let novoT = moveElementos tab e                         
                 return novoT


-- Verifica se jogador ainda existe algum movimento, caso contrario imprime mensagem de derrota
jogo :: Tab -> IO ()
jogo tab =
    case existeMovimento tab of
            True  -> do imprimeTab tab
                        result tab
            False -> do imprimeTab tab
                        putStrLn "Você perdeu!"       
        

-- Realiza a transporsição da matriz, ou seja, troca linha por colunas
moveElementos :: Tab -> Direcao -> Tab
moveElementos tab Esq = map concatena tab 
moveElementos tab Dir = map (reverse . concatena . reverse) tab
moveElementos tab Cima = transpose (moveElementos(transpose tab) Esq)
moveElementos tab Baixo = transpose(moveElementos(transpose tab) Dir)


-- Verifica se existe algum movimento sobrando a ser executado no tabuleiro da partida 
existeMovimento :: Tab -> Bool
existeMovimento tab = sum possibilidades > 0 
    where possibilidades = map (length . getZeros . moveElementos tab) [Esq, Dir, Cima, Baixo] 

-- determina os quadrantes do tabuleiro
determinaQuad :: Tab -> (Int, Int) -> Int -> Tab
determinaQuad tab (lin, col) n = inicio ++ [meio] ++ fim
    where inicio  = take lin tab
          meio  = take col (tab!!lin) ++ [n] ++ drop (col + 1) (tab!!lin)
          fim = drop (lin + 1) tab

-- Adiciona novo bloco com o valor de 2 gerado no final de cada turno em um quadrado vazio escolhido aleatoriamente
adicionaBloco :: Tab -> IO Tab
adicionaBloco tab = do pegaDirecao <- newStdGen
                       let tabIni = getZeros tab
                           posicao = head(randoms pegaDirecao :: [Int]) `mod` length tabIni
                           elem = [2,2,2,2,2,2,2,2,2,2] !! (head (randoms pegaDirecao :: [Int])  `mod` 10)
                           novoBloco = determinaQuad tab (tabIni!!posicao) elem
                       return novoBloco
                 
-- Anuncia vitoria ou cria novo tabuleiro
result :: Tab -> IO()
result tab = if verificaVitoria tab
             then putStrLn "Você venceu!"
                        else do
                           novoT <- novoTab tab  
                           if tab /= novoT
                           then do novo <- adicionaBloco novoT
                                   jogo novo
                           else jogo tab

-- pega linhas do tabuleiro com zeros
getZeros :: Tab -> [(Int, Int)]
getZeros tab = filter (\(lin, col) ->(tab!!lin)!!col == 0) (concatMap linha [0..3])
    where linha n = zip (replicate 4 n) [0..3]


------------------------------------------------------------------------------------------------------------------------------
main :: IO ()
main = do putStrLn "2048"
          putStrLn "Use as teclas 'e','d','c','b' para mover os numeros de acordo com a direção indicada"
          putStrLn "Combine numeros até obter 2048"
          putStrLn  "Digite uma direção:"
          jogo tabInicial
