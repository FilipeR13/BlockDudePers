{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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
validaPotencialMapa l = validaCoordenadas l && verificarPorta l == 1 && caixaFlutuante l

{- |
==Valida Coordenadas

As seguintes 3 funções têm como objetivo verificar se existem Pecas com coordenadas iguais.

* 'validaCoordenadas' é a função principal que verifica se existem coordenadas iguais.
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

{- |
== Verifica Porta

A função 'verficarPorta' conta o número de portas que existem no mapa. 
-}

verificarPorta :: [(Peca, Coordenadas)] -> Int
verificarPorta [] = 0
verificarPorta ((a,b):t) | a == Porta = 1 + verificarPorta t 
                         | otherwise = verificarPorta t


{- | 
== Caixa Flutuante

A função 'caixaFlutuante' é a função "mãe" da condição que nos vai indicar se o mapa dado obedece à condição pedida ("Todas as caixas devem estar 
posicionadas em cima de outra caixa ou bloco, i.e. não podem haver caixas a "flutuar""). 
Para isso utilizamos as seguintes funções recursivas como funções auxiliares:

* 'caixaTemBase' -> verifica se a peca debaixo de uma caixa é um dos elementos que queremos, isto é, se o elemento imediatamente abaixo da caixa 
com (x,y) coordenadas é uma caixa ou um bloco.

* 'caixasTemBase' -> verifica se uma lista de caixas de um mapa dado têm todas ou uma caixa ou um bloco por baixo, dando como argumento as coordenadas 
da peça imediatamente abaixo à caixa à função 'caixaTemBase'.

* 'listCaixas' -> recebe uma lista, que vai ser o nosso mapa e filtra as caixas do mesmo, isto é, devolve uma lista com apenas as caixas do mapa, com a 
ajuda da função 'verificaCaixa'.

* 'verificaCaixa' -> verifica cada elemento de uma lista se é uma caixa ou não.

-}

caixaFlutuante :: [(Peca, Coordenadas)] -> Bool
caixaFlutuante l@((p,c):t) = caixasTemBase caixas l
    where
        caixas = listCaixas l

listCaixas :: [(Peca, Coordenadas )] -> [(Peca, Coordenadas )]
listCaixas pc = filter verificaCaixa pc

verificaCaixa :: (Peca, Coordenadas ) -> Bool 
verificaCaixa (a, _) | a == Caixa =  True 
                     | otherwise = False

caixasTemBase :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)] -> Bool
caixasTemBase [] _ = True
caixasTemBase _ [] = False
caixasTemBase ((p, (x,y)):pcs) l = caixaTemBase (x, y+1) l && caixasTemBase pcs l 

caixaTemBase :: Coordenadas -> [(Peca, Coordenadas)] -> Bool
caixaTemBase _ [] = False
caixaTemBase (x, y) ((p,(a,b)):t) | x == a && y == b && (p == Caixa || p == Bloco) = True
                                  | otherwise = caixaTemBase (x, y) t