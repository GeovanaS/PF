-- Exercicios sobre Type Classes

-- Exercicio 1
data Lista a = Cons a (Lista a) | Vazio

instance Show a => Show (Lista a) where
   show = showLista 

showLista :: Show a => Lista a -> String
showLista (Cons x xs) = "[" ++ show x ++ "," ++ show xs  ++ "]"

-- Cons 1 (Cons 2 (Cons 3 Vazio))
-- [1,2,3]

