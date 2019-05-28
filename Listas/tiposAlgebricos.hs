data Temperatura = Frio | Calor
    deriving(Eq,Show)

data Estacao = Verao | Outono | Inverno | Primavera
   deriving(Eq,Show)


tempo :: Estacao -> Temperatura
tempo Verao = Calor
tempo _ = Frio

--data Bool = True | False

data Funcionario = Pessoa Nome Idade
   deriving(Eq,Show)

type Nome = String
type Idade = Int

andre :: Funcionario
andre = Pessoa "Andre Du Bois" 28

-- Pessoa :: Nome -> Idade -> Funcionario

getNome :: Funcionario -> Nome
getNome (Pessoa n i) = n

getIdade :: Funcionario -> Idade
getIdade (Pessoa n i) = i

data Forma = Circulo Float | Retangulo Float Float
     deriving(Eq,Show)

redondo :: Forma -> Bool
redondo (Circulo x) = True
redondo (Retangulo x y) = False

--> redondo (Circulo 20.5)


area :: Forma -> Float
area (Circulo r) = pi * r * r
area (Retangulo b a) = b*a
