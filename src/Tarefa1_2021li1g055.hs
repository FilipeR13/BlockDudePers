{- |
Module      : Tarefa1_2021li1g055
Description : Validação de um potencial mapa
Copyright   : Lucas Quintela <a100642@alunos.uminho.pt>;
            : José Rodrigues <a100692@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa1_2021li1g055 where

import LI12122

validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa [] = False
validaPotencialMapa [x] = False
validaPotencialMapa l = validaCoordenadas l && verificarPorta l == 1 

{-|
==Valida Coordenadas

As seguintes 3 funções têm como objetivo verificar se existem Pecas com coordenadas iguais.

* 'validaCoordenadas' é a função principal que verifica se existem funções iguais.
* 'soCoordenadas' tem como objetivo obter uma lista de só de coordenadas para facilitar a verificação.
* 'validaCoordenadasA' verifica se existem coordenadas iguais verificando se o primeiro elemento pertence ao resto da lista
e volta a fazer o mesmo para o resto da lista.
-}

validaCoordenadas ::  [(Peca, Coordenadas)] -> Bool
validaCoordenadas [] = False
validaCoordenadas l = validaCoordenadasA (soCoordenadas l)

soCoordenadas :: [(Peca, Coordenadas)] -> [Coordenadas]
soCoordenadas [] = []
soCoordenadas (x:xs) = snd x : soCoordenadas xs

validaCoordenadasA :: [Coordenadas] -> Bool
validaCoordenadasA [] = True
validaCoordenadasA (x:xs) = not (elem x xs) && validaCoordenadasA xs 

{-
-}

verificarPorta :: [(Peca, Coordenadas)] -> Int
verificarPorta [] = 0
verificarPorta ((a,b):t) | a == Porta = 1 + verificarPorta t 
                         | otherwise = verificarPorta t

