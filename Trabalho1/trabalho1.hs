-- Trabalho 1 - Programação Funcional
-- Nome: Geovana Silveira

-- Recebe como entrada uma string contendo um texto em várias linhas e devolve o mesmo texto justificado pela maior linha.
-- Prelude > putStr (justifica texto)

justifica :: String -> String
justifica [] = []
justifica l = justificaLinhas (retiraUltimaLinha (separaLinhas l)) (tamanhoMaiorLinha (separaLinhas l)) ++ pegaUltimaLinha (separaLinhas l) ++ "\n"

texto = "RUBIÃO fitava a enseada -- eram oito horas da manhã.\nQuem o visse com os polegares metidos no cordão do chambre à janela de uma\ngrande casa de Botafogo cuidaria que ele admirava aquele pedaço de água\nquieta mas em verdade vos digo que pensava em outra coisa.\nCotejava o passado com o presente. Que era há um ano?\nProfessor. Que é agora? Capitalista! Olha para si para as chinelas\n(umas chinelas de Túnis que lhe deu recente amigo Cristiano Palha) para a casa\npara o jardim para a enseada para os morros e para o céu e tudo desde as chinelas\naté o céu tudo entra na mesma sensação de propriedade."

retiraUltimaLinha :: [String] -> [String]
retiraUltimaLinha [] = []
retiraUltimaLinha [a] = []
retiraUltimaLinha (x:xS) = x : retiraUltimaLinha xS

tamanhoMaiorLinha :: [String] -> Int
tamanhoMaiorLinha [] = 0
tamanhoMaiorLinha [l] = length l
tamanhoMaiorLinha l = maior (junta tamLinha l)

justificaLinhas :: [String] -> Int -> String
justificaLinhas [] n = []
justificaLinhas (x:xs) n = (justificaLinha x n) ++ "\n" ++ justificaLinhas xs n

justificaLinha :: String -> Int -> String
justificaLinha [] n = []
justificaLinha l n = justificaResto(justificaDiv l (div (n - (tamLinha l))  (numEspacos l)))  (mod(n - (tamLinha l)) (numEspacos l))

justificaResto :: String -> Int -> String
justificaResto l 0 = l
justificaResto (x:y:xS) n
             | (ehEspaco x) && (ehEspaco y /= True) = (insereEspaco 2) ++ (justificaResto(y:xS) (n-1))
             | otherwise = x : (justificaResto(y:xS) n)

justificaDiv :: String -> Int -> String
justificaDiv [] n = []
justificaDiv [l] n = [l]
justificaDiv (x:y:xS) n
            | (ehEspaco x) && (ehEspaco y /= True) = [x] ++ (insereEspaco n) ++ (justificaDiv(y:xS) n)
            | otherwise = x : (justificaDiv (y:xS) n) 

insereEspaco :: Int -> String
insereEspaco 0 = []
insereEspaco n = ' ' : insereEspaco(n-1)

numEspacos :: String -> Int
numEspacos [] = 0
numEspacos (x:xS) 
        | x == ' ' = 1 + numEspacos xS
        | otherwise = numEspacos xS

tamLinha :: String -> Int
tamLinha [] = 0
tamLinha (x:xS) = 1 + tamLinha xS

maior :: [Int]  -> Int
maior [] = 0
maior [x] = x
maior(x1:x2:xS)
   | (x1 >=  x2) = maior(x1:xS)
   | otherwise = maior(x2:xS)

junta :: (String -> Int) -> [String] -> [Int]
junta l [] = []
junta l (x:xS) = l x : (junta l xS)   

ehFinalDeLinha l = l == '\n'

separaLinhas :: String -> [String]
separaLinhas [] = []
separaLinhas l = let(pref,suf) = break ehFinalDeLinha l
                 in pref: case suf of
                 ('\n':resto) -> separaLinhas resto
                 _ -> []

ehEspaco s = s == ' '

pegaUltimaLinha :: [String] -> String
pegaUltimaLinha [] = []
pegaUltimaLinha [a] = a
pegaUltimaLinha (x:xS) = pegaUltimaLinha xS


--Textos de Teste
test1 = "Bien sé que suelo en ella no se halla\n\
\Y que ninguno puede vadearla\n\
\Aunque es de noche\n\
\Su claridad nunca es oscurecida\n\
\Y toda luz de ella es venida\n\
\Aunque es de noche\n\
\Y son tan caudalosas sus corrientes\n\
\Que cielos, infiernos riegan y las gentes\n\
\Aunque es de noche\n\
\Aunque es de noche\n\
\Aunque es de noche."


test2 = "I've been tearing around in my fucking nightgown\n\
\24/7 Sylvia Plath\n\
\Writing in blood on my walls\n\
\'Cause the ink in my pen don't work in my notepad\n\
\Don't ask if I'm happy, you know that I'm not\n\
\But at best I can say I'm not sad\n\
\'Cause hope is a dangerous thing for a woman like me to have."

test3 = "Well I've always had a deep respect\n\
\And I mean that most sincerely\n\
\The band is just fantastic\n\
\That is really what I think\n\
\Oh, and by the way, which one's pink?"
