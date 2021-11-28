{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{- |
Module      : Tarefa4_2021li1g055
Description : Movimentação do personagem
Copyright   : Lucas Quintela <a100642@alunos.uminho.pt>;
            : José Rodrigues <a100692@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
-}
module Tarefa4_2021li1g055 where

import LI12122
import Tarefa2_2021li1g055
import Tarefa3_2021li1g055

{-|
== Move Jogador

A função 'moveJogador' é a função que recebe o jogo e o movimento. 

* AndarEsquerda: A função verifica, com recurso às funções 'movimentovalido' e 'movimentoesquerdaBloco' (caso o jogador esteja a carregar um bloco), se o jogador pode avançar para a esquerda.
Se puder andar então vai devolver o mapa com o jogador na sua nova posição, calculada pela função 'verificaposicaoesquerda'.

* Trepar: A função começa por verificar se o jogador pode trepar um bloco, testando se este se encontra na ordenada igual a 0 ou se está na ordenada igual a 1 se estiver a carregar um bloco, pois
estando numa coordenada assim, o jogador não tem como subir nenhum bloco. Para além disso, se o jogador estiver nos limites laterais do mapa e ao mesmo tempo estiver virado para esse mesmo limite,
a função devolve o mapa sem o modificar pois o jogador não pode trepar para nenhum lado. Passado estes casos a função verifica se não há blocos em cima jogador e se existe um bloco/caixa em que este
possa subir com recurso às funções 'subir' e 'subirBloco', completando assim o movimento e devolvendo o mapa com o jogador na nova posição. 

-}

moveJogador :: Jogo -> Movimento -> Jogo
moveJogador (Jogo l (Jogador (x,y) d b)) AndarEsquerda
        | movimentovalido l (x-1,y) && d == Oeste && b == False = Jogo l (Jogador (verificaposicaoesquerda l (x,y)) Oeste b)
        | movimentovalido l (x-1,y) && d == Este && b == False = Jogo l (Jogador (verificaposicaoesquerda l (x,y)) Oeste b)
        | movimentoesquerdaBloco l (x,y) && d == Oeste && b == True = Jogo l (Jogador (verificaposicaoesquerda l (x,y)) Oeste b)
        | movimentoesquerdaBloco l (x,y) && d == Este && b == True = Jogo l (Jogador (verificaposicaoesquerda l (x,y)) Oeste b)
        | otherwise = Jogo l (Jogador (x,y) Oeste b)
moveJogador (Jogo l (Jogador (x,y) d b)) Trepar
        | y == 0 = Jogo l (Jogador (x,y) d b)
        | y == 1 && b == True = Jogo l (Jogador (x,y) d b) 
        | elem (Bloco,(x,y-1)) (desconstroiMapa l) = Jogo l (Jogador (x,y) d b) 
        | (d == Oeste && x == 0) || (d == Este && x == xmax (desconstroiMapa l)) = Jogo l (Jogador (x,y) d b)
        | b == False = case d of Oeste -> if subir l (x-1,y) then Jogo l (Jogador (x-1,y-1) d b) else Jogo l (Jogador (x,y) d b)
                                 Este -> if subir l (x+1,y) then Jogo l (Jogador (x+1,y-1) d b) else Jogo l (Jogador (x,y) d b)
        | b == True = case d of Oeste -> if subirBloco l (x-1,y) then Jogo l (Jogador (x-1,y-1) d b) else Jogo l (Jogador (x,y) d b)
                                Este -> if subirBloco l (x+1,y) then Jogo l (Jogador (x+1,y-1) d b) else Jogo l (Jogador (x,y) d b)

{-|
A função 'movimentovalido' começa por verificar se o jogador se encontra a tentar mover-se para fora dos limites do mapa laterais, pelo que devolve False. De seguida verifica se existe uma Caixa ou
um bloco na posição para a qual o jogador quer ir, se existir devolve False. Se nenhum dos ultimos casos se verificar, a função irá devolver True. 
-}
movimentovalido :: Mapa -> Coordenadas -> Bool
movimentovalido l (x,y)
        | x+1 == 0 || x-1 == xmax (desconstroiMapa l) = False
        | elem (Caixa, (x,y)) (desconstroiMapa l) = False
        | elem (Bloco ,(x,y)) (desconstroiMapa l) = False
        |otherwise = True
{-|
A função 'verificaposicaoesquerda' calcula a coordenada para a qual o jogador se vai deslocar, verificando o chão à sua frente. Se não existir chão imediatamente à frete do jogador, a função
irá ser chamada recursivamente com a ordenada do jogador com mais unidade, procurando assim o chão em que o jogador irá ficar. Quando encontrar o chão, vai devolver a posição do jogador.
-}
verificaposicaoesquerda :: Mapa -> Coordenadas -> Coordenadas
verificaposicaoesquerda l (x,y) | x == 0 = (x,y)
                                | elem (Caixa, (x-1,y+1)) (desconstroiMapa l) || elem (Bloco, (x-1,y+1)) (desconstroiMapa l) = (x-1,y)
                                | otherwise = verificaposicaoesquerda l (x,y+1)
{-|
Esta função 'movimentoesquerdaBloco' é usada para verificar se o jogador pode avançar mesmo estando a carregar um bloco. Essencialmente, esta função verifica as mesmas coisas que a função 'movimentovalido'
com a adição da condição de que não deve existir um bloco nas coordenadas (x-1,y-1), sendo (x,y) as coordenadas do jogador, pois assim a caixa assim não poderá passar.
-}
movimentoesquerdaBloco :: Mapa -> Coordenadas -> Bool
movimentoesquerdaBloco l (x,y) 
        | x == 0 = False
        | elem (Caixa, (x-1,y)) (desconstroiMapa l) = False
        | elem (Bloco ,(x-1,y)) (desconstroiMapa l) = False
        | elem (Bloco , (x-1,y-1)) (desconstroiMapa l) = False
        |otherwise = True

{-|
A função 'subir' tem como objetivo verificar se o jogador tem algum bloco ou caixa que possa trepar. Para além disso, este também verifica se tem algum bloco em cima do que este quer subir, pois se 
existir o  jogador não poderá subir.
-}
subir :: Mapa -> Coordenadas -> Bool
subir l (x,y) | elem (Caixa, (x,y-1)) m || elem (Bloco, (x,y-1)) m = False
              | elem (Caixa, (x,y)) m || elem (Bloco, (x,y)) m = True
              | otherwise = False
              where m = desconstroiMapa l
{-|
A função 'subirBloco' tem praticamente o mesmo objetivo que a função anterior ('subir') com a adição da verificação se existe algum bloco no local onde a caixa irá ficar depois de o jogador trepar
pois, se existir,  o jogador não poderá trepar.
-}
subirBloco :: Mapa -> Coordenadas -> Bool
subirBloco l (x,y)
        |elem (Caixa, (x,y-1)) m || elem (Bloco, (x,y-1)) m || elem (Bloco, (x,y-2)) m = False
        |elem (Caixa, (x,y)) m || elem (Bloco, (x,y)) m = True
        |otherwise = False
        where m = desconstroiMapa l



correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos jogo movimentos = undefined

