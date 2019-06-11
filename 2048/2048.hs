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

-- Concatena e soma elementos com o mesmo valor
concatena :: [Int] -> [Int]
concatena x = somaIguais(filter (/=0) x) ++ (replicate(length x - length (somaIguais (filter (/= 0) x) ) ) 0) 

somaIguais :: [Int] -> [Int]
somaIguais (x:y:xs)
              | x==y = x + x : somaIguais xs
              | otherwise = x: somaIguais(y:xs)
somaIguais x = x


-- Pega a direcao para mover os elementos
entrada :: IO Direcao
entrada = do 
          dir <- hGetChar stdin 
          case (ord dir) of 
              101 -> return Esq
              100 -> return Dir
              99 -> return Cima
              98 -> return Baixo
              _   -> do putStrLn ""
                        entrada


-- Verifica se jogador venceu o jogo
verificaVitoria :: Tab -> Bool
verificaVitoria tab = [] /= filter (==2048) (concat tab)

-- Imprime tabuleiro
imprimeTab :: Tab -> IO()
imprimeTab tab = do putStr $ "\ESC[2J" ++ "\ESC[2J"
                    mapM_ (putStrLn . imprimeLinha) tab

imprimeLinha :: [Int] -> String
imprimeLinha [] = ""
imprimeLinha (x:xs) = show x ++ " " ++ imprimeLinha xs

novoTab :: Tab -> IO Tab
novoTab tab = do e <- entrada    
                 let novoT = moveElementos tab e                         
                 return novoT


jogo :: Tab -> IO ()
jogo tab =
    case existeMovimento tab of
            True  -> do imprimeTab tab
                        if verificaVitoria tab
                        then putStrLn "Você venceu!"
                        else do
                           novoT <- novoTab tab  
                           if tab /= novoT
                           then do novo <- retornaNovoT novoT
                                   jogo novo
                           else jogo tab
            False -> do imprimeTab tab
                        putStrLn "Você perdeu!"       
            


moveElementos :: Tab -> Direcao -> Tab
moveElementos tab Esq = map concatena tab 
moveElementos tab Dir = map (reverse . concatena . reverse) tab
moveElementos tab Cima = transpose (moveElementos(transpose tab) Esq)
moveElementos tab Baixo = transpose(moveElementos(transpose tab)Dir)


-- Verifica se existe algum movimento sobrando a ser executado no tabuleiro da partida 
existeMovimento :: Tab -> Bool
existeMovimento tab = sum possibilidades > 0 
    where possibilidades = map (length . pegaZeros . moveElementos tab) [Esq, Dir, Cima, Baixo] 


determinaQuad :: Tab -> (Int, Int) -> Int -> Tab
determinaQuad tab (lin, col) n = inicio ++ [meio] ++ fim
    where inicio  = take lin tab
          meio  = take col (tab!!lin) ++ [n] ++ drop (col + 1) (tab!!lin)
          fim = drop (lin + 1) tab

retornaNovoT :: Tab -> IO Tab
retornaNovoT tab = do e <- newStdGen
                      let candidatos      = pegaZeros tab
                          pos             = head (randoms e :: [Int]) `mod` length candidatos
                          esc             = candidatos!!pos
                          n               = [2,4,2,2,4,2,2,2,2,4] !! (head (randoms e :: [Int]) `mod` 10)
                          novoT           = determinaQuad tab esc n
                      return novoT          
                   

pegaZeros :: Tab -> [(Int, Int)]
pegaZeros tab = filter (\(lin, col) -> (tab!!lin)!!col == 0) coordenadas
    where linha n = zip (replicate 4 n) [0..3]
          coordenadas = concatMap linha [0..3]

-----------------------------------------------------------------------------------------------------------------------------------------------
main :: IO ()
main = do putStrLn "Jogo 2048"
          putStrLn "Use as teclas 'e','d','c','b' para mover os numeros de acordo com a direção indicada"
          putStrLn "Combine numeros até obter 2048"
          jogo tabInicial