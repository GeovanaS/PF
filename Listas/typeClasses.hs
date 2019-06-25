-- Aula 25/06

-- 26/06: Duvida listas/trabalho
-- 02/07: Duvida listas/trabalho
-- 03/07: Revisão/Duvidas para a prova

-- Prova 2: 10/07
-- Apresentação trabalho: 09/07


-- Prova 2 com foco em tipos algebricos


-- Type Classes e Sobrecarga

-- > :t elemento
-- > elemento :: Eq t => t -> [t] -> Bool
-- a precisa pertencer a classe Eq, que é a classe dos tipos que podem ser comparados

elemento :: Eq a => a -> [a] -> Bool
elemento n [] = False
elemento n (x:xs) = n == x || elemento n xs


data SocioClube = Socio Int String String
 --  deriving(Eq,Show)

-- > Socio 33 "Andre" "99" == Socio 22 "Joao" "77"
-- > False

eqSocio :: SocioClube -> SocioClube -> Bool
eqSocio (Socio c1 _ _) (Socio c2 _ _) = c1 == c2

-- > eqSocio (Socio 33 "Andre" "99") (Socio 33 "Joao" "99")
-- > True

instance Eq SocioClube where
    (==) = eqSocio


--class Show a where
  --  show :: a -> String

showSocio :: SocioClube -> String
showSocio (Socio i s1 s2) = "{ " ++ " Codigo: " ++ show i ++ ", Nome: " ++ show s1 ++ ", Fone: " ++ s2 ++ " }"

instance Show SocioClube where 
     show = showSocio


data TUnica a = T1 a a

showTUnica :: Show a => TUnica a -> String
showTUnica (T1 a b) = "(" ++ show a ++ "," ++ show b ++ ")"

instance Show a => Show (TUnica a) where
    show = showTUnica

-- > T1 40 1
-- > (40,1)
