-- Prelude > putStr (justifica texto)

-- justifica :: String -> String

texto = "RUBIÃO fitava a enseada -- eram oito horas da manhã.\n Quem o visse com os polegares metidos no cordão do chambre à janela de uma\ngrande casa de Botafogo cuidaria que ele admirava aquele pedaço de água\nquieta mas em verdade vos digo que pensava em outra coisa.\nCotejava o passado com o presente. Que era há um ano?\nProfessor. Que é agora? Capitalista! Olha para si para as chinelas\n(umas chinelas de Túnis que lhe deu recente amigo Cristiano Palha) para a casa\npara o jardim para a enseada para os morros e para océu e tudo desde as chinelas\naté o céu tudo entra namesma sensação de propriedade."

maior :: [String]  -> Int
maior [] = 0
maior [x] = length x
maior(x:xS)
   | (length x > length xS) = length x
   | otherwise = length xS

tamanhoMaiorLinha :: [String] -> Int
tamanhoMaiorLinha [] = 0 
tamanhoMaiorLinha [x] = length x
tamanhoMaiorLinha (x:xS) = maior (x:xS)

ehFinalDeLinha l = l == '\r' || l == '\n'

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


-- insereEspacos :: Int -> String -> String