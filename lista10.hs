-- Exercicios sobre Tipos Algebricos Simples

-- 1
data ItemDeLocadora = CD Artista Titulo | DVD String String| Videos Duracao
     deriving(Eq,Show)

--2
data SocioLocadora = Socio Nome Telefone
     deriving(Eq,Show)

type Nome = String
type Telefone = Int
type Artista = String
type Titulo = String
type Duracao = Int

--3
type ItensDisponiveis = [ItemDeLocadora]

minhaLista :: ItensDisponiveis
minhaLista = [CD "a", DVD "b",Videos "c"]

--4 
type ItemAlugado = (SocioLocadora ItemDeLocadora)

--5
type Alugados = [ItemAlugado]


aluga :: ItemAlugado
aluga = (Socio "andre" 323897432,CD "fx" "dx")
