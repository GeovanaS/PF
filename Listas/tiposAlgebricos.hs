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

--data Arvore = Folha | Nodo Int Arvore Arvore

--data ArvoreP a = Folha | Nodo A (ArvoreP a) (ArvoreP a)

--minhaArvore :: Arvore
--minhaArvore = Nodo 10 (Nodo 14 (Nodo 1 Folha Folha)Folha)

--somaArvore :: Arvore -> Int
--somaArvore Folha = 0
--somaArvore (Nodo n a1 a2) = n + somaArvore a1 + somaArvore a2

-- > somaArvore (Nodo 5 Folha Folha)

data Arvore a = Folha a | Nodo a (Arvore a) (Arvore a)
    deriving(Eq,Show)

arv :: Arvore Int
arv = Nodo 10 (Nodo 14 (Folha 15) (Folha 16)) (Nodo 3 (Folha 3) (Folha 8))

somaArvore :: Arvore Int -> Int
somaArvore (Folha n) = n
somaArvore (Nodo n a1 a2) = n + somaArvore a1 + somaArvore a2

-- > somaArvore arv

-- Lista 11

-- Ex1
multArvore :: Arvore Int -> Int
multArvore (Folha n) = n * 2
multArvore (Nodo n a1 a2) = n * 2 + multArvore a1 + multArvore a2

-- Ex2
contArvore :: Arvore Int -> Int
contArvore (Folha n) = 1
contArvore (Nodo n a1 a2) = 1 + contArvore a1 + contArvore a2

-- Ex3
maiorElem :: Arvore Int -> Int
maiorElem (Folha n) = n
maiorElem (Nodo n a1 a2)
          | n > (maiorElem a1) && n > (maiorElem a2) = n
          | (maiorElem a1) > (maiorElem a2) = maiorElem a1
          | otherwise = maiorElem a2


-- Ex4          
buscaArvore :: Arvore Int -> Int -> Bool
buscaArvore (Folha n) x = (n == x)
buscaArvore (Nodo n a1 a2) x
           | (n == x) = True
           | (buscaArvore a1 x) || (buscaArvore a2 x) = True
           | otherwise = False 

-- Ex5
--quantArvore :: Arvore Int -> Int -> Int
--quantArvore (Folha n) x = if(n==x) then 1 else 0 
--quantArvore (Nodo n a1 a2) x 
  --          | (n==x) = 1 + quantArvore a1 + quantArvore a2
    --        | otherwise = 0