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

-- > redondo (Circulo 20.5)

area :: Forma -> Float
area (Circulo r) = pi * r * r
area (Retangulo b a) = b*a

data Arvore = Folha | Nodo Int Arvore Arvore

data ArvoreP a = Folha| Nodo A (Arvore a) (Arvore a)

minhaArvore :: Arvore
minhaArvore = Nodo 10 (Nodo 14 (Nodo 1 Folha Folha)Folha)

somaArvore :: Arvore -> Int
somaArvore Folha = 0
somaArvore (Nodo n a1 a2) = n + somaArvore a1 + somaArvore a2
