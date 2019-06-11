# 2048

Implementação simples do jogo 2048 em haskell

As regras são que em cada turno o jogador deve escolher uma direção (para cima, para baixo, para a esquerda ou para a direita) e todas as peças se movem o máximo possível nessa direção, algumas mais que outras. Duas peças adjacentes (somente nessa direção) com números correspondentes são combinadas em uma contendo a soma desses números. Um movimento é válido quando pelo menos um bloco pode ser movido, se apenas por combinação. Um novo bloco com o valor de 2 ou 4 é gerado no final de cada turno em um quadrado vazio escolhido aleatoriamente, se houver um. Para ganhar, o jogador deve criar uma peça com o número 2048. O jogador perde se nenhum lance válido for possível.

Compilação:

       ghc 2048.hs
       ./2048
