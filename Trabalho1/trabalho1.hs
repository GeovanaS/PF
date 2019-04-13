-- Recebe como entrada uma string contendo um texto em várias linhas e devolve o mesmo texto justificado pela maior linha.
-- Prelude > putStr (justifica texto)

justifica :: String -> String
justifica [] = []
justifica l = justificaLinhas (retiraUltimaLinha (separaLinhas l)) (tamanhoMaiorLinha (separaLinhas l)) ++ pegaUltimaLinha (separaLinhas l) ++ "\n"

texto = "RUBIÃO fitava a enseada -- eram oito horas da manhã.\n Quem o visse com os polegares metidos no cordão do chambre à janela de uma\ngrande casa de Botafogo cuidaria que ele admirava aquele pedaço de água\nquieta mas em verdade vos digo que pensava em outra coisa.\nCotejava o passado com o presente. Que era há um ano?\nProfessor. Que é agora? Capitalista! Olha para si para as chinelas\n(umas chinelas de Túnis que lhe deu recente amigo Cristiano Palha) para a casa\npara o jardim para a enseada para os morros e para océu e tudo desde as chinelas\naté o céu tudo entra namesma sensação de propriedade."

tamanhoMaiorLinha :: [String] -> Int
tamanhoMaiorLinha [] = 0
tamanhoMaiorLinha [l] = length l
tamanhoMaiorLinha l = maior (junta tamLinha l)

maior :: [Int]  -> Int
maior [] = 0
maior [x] = x
maior(x1:x2:xS)
   | (x1 >=  x2) = maior(x1:xS)
   | otherwise = maior(x2:xS)

junta :: (String -> Int) -> [String] -> [Int]
junta l [] = []
junta l (x:xS) = l x : junta l xS   

insereEspacos :: Int -> Int -> String -> String
insereEspacos 0 0 [] = []
insereEspacos s r l 
 |(separaPalavras l) == [] = l
 |r > 0 = takeWhile (/= ' ') l ++ (completaEspaco s) ++ " "  ++ insereEspacos s (r-1) (drop 1(dropWhile (/= ' ') l))
 |otherwise = takeWhile (/= ' ') l ++ (completaEspaco s) ++ " "  ++ insereEspacos s 0 (drop 1(dropWhile (/= ' ') l))

numEspacos :: String -> Int
numEspacos [] = 0
numEspacos (x:xS) 
        | x == ' ' = 1 + numEspacos xS
        | otherwise = numEspacos xS

tamLinha :: String -> Int
tamLinha [] = 0
tamLinha (x:xS) = 1 + tamLinha xS

justificaLinhas :: [String] ->  Int -> String
justificaLinhas [] n = []
justificaLinhas (x:xS) n = justificaLinha x n ++ "\n" ++ justificaLinhas xS n

justificaLinha :: String -> Int -> String
justificaLinha [] n = []
justificaLinha l n 
             | numEspacos l == 0 = l
             | otherwise = insereEspacos (div (n - (tamLinha l)) (numEspacos l)) (mod(n - tamLinha l) (numEspacos l)) l


completaEspaco :: Int -> String
completaEspaco 0 = []
completaEspaco n = ' ' : completaEspaco(n-1)

ehFinalDeLinha l = l == '\n'

separaLinhas :: String -> [String]
separaLinhas [] = []
separaLinhas l = let(pref,suf) = break ehFinalDeLinha l
                 in pref: case suf of
                 ('\r':'\n':resto) -> separaLinhas resto
                 ('\r':resto) -> separaLinhas resto
                 ('\n':resto) -> separaLinhas resto
                 _ -> []

ehEspaco s = s == ' '

separaPalavras :: String -> [String]
separaPalavras "" = []
separaPalavras s = let(pref,suf) = break ehEspaco s
                 in pref: case suf of
                 (' ':resto) -> separaPalavras resto
                 _ -> []

pegaUltimaLinha :: [String] -> String
pegaUltimaLinha [] = []
pegaUltimaLinha [a] = a
pegaUltimaLinha (x:xS) = pegaUltimaLinha xS


retiraUltimaLinha :: [String] -> [String]
retiraUltimaLinha [] = []
retiraUltimaLinha [a] = []
retiraUltimaLinha (x:xS) = x : retiraUltimaLinha xS