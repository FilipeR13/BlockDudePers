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
validaPotencialMapa l = validaCoordenadas l && verificarPorta l == 1 && caixaFlutuante l && espacosVazios l && chaoContinuo l

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

{-
== Espaços Vazios

A funcão 'espacosVazios' é a função que vai verificar se existe pelo menos um Vazio no mapa

Assim a função divide-se em duas condições:

* Primeiro verifica se existe algum Vazio declarado na lista e, se existir, devolve True. Esta utiliza como função auxiliar a função 'soPecas' que serve para
fazer uma lista só das peças do mapa

* A segunda condição verifica se o número de elementos dados da lista é menor que a área do mapa. Se isto se verificar então há blocos vazios, logo retorna True.
-}

espacosVazios :: [(Peca,Coordenadas)] -> Bool
espacosVazios [] = False 
espacosVazios l | elem Vazio (soPecas l) = True 
                | otherwise = length l < ((x+1) * (y +1))
                where x = maximum (map fst (soCoordenadas l))
                      y = maximum (map snd (soCoordenadas l))

soPecas :: [(Peca,Coordenadas)] -> [Peca]
soPecas [] = []
soPecas (x:xs) = fst x : soPecas xs


{-|
== Chão Contínuo

A função 'chaoContinuo' vai verificar se o chão de um mapa é válido, i.e., se o bloco em x == 0, com o maior y (ou seja, mais em baixo) tem ligação contínua 
com o bloco com o maior x (bloco mais à direita) com o maior y (bloco mais em baixo). Para isso, são necesárias uma série de funções auxiliares: 

* 'soBlocoscomCoordenadas' -> recebe um mapa na forma de lista de peças e respetivas coordenadas e filtra apenas as coordenadas dos blocos da lista.

* 'verificacontinuidade' -> recebe uma lista de coordenadas (que na função principal vai ser a lista das coordenadas dos blocos do mapa) e uma coordenada 
e tem como objetivo verificar se existe algum bloco à frente, em cima ou em baixo da coordenada dada ( que também é um bloco) garantindo assim a continuidade 
do chão.

* 'primeirobloco' -> dá a coordenada do primeiro bloco do chão, i.e., o bloco com o maior y da primeira coluna.

* 'primeiracoluna' -> dá uma lista de coordenadas que correspondem às coordenadas de todos os elementos da primeira coluna do mapa.

* 'ultimacoluna' -> dá uma lista de coordenadas que correspondem às coordenadas de todos os elementos da última coluna do mapa. 
-}

chaoContinuo :: [(Peca, Coordenadas)] -> Bool
chaoContinuo l = verificacontinuidade (soBlocoscomCoordenadas l) (primeirobloco (primeiracoluna (soCoordenadas l)))

soBlocoscomCoordenadas :: [(Peca, Coordenadas)] -> [Coordenadas]
soBlocoscomCoordenadas [] = []
soBlocoscomCoordenadas ((a,(x,y)):t) | a == Bloco = (x,y) : soBlocoscomCoordenadas t 
                                     | otherwise = soBlocoscomCoordenadas t 

verificacontinuidade :: [Coordenadas] -> Coordenadas -> Bool
verificacontinuidade l (a,b) | a == maximum (map fst (ultimacoluna l)) = True
                             | elem (a+1,b) l = verificacontinuidade l (a+1,b)
                             | elem (a, b-1) l = verificacontinuidade l (a, b-1)
                             | elem (a, b+1) l = verificacontinuidade l (a, b+1)
                             | otherwise = False

primeirobloco :: [Coordenadas] -> Coordenadas
primeirobloco l = (0, maximum (map snd (primeiracoluna l)))

primeiracoluna :: [Coordenadas] -> [Coordenadas]
primeiracoluna [] = []
primeiracoluna ((x,y):xs) | x == 0 = (x,y) : primeiracoluna xs
                          | otherwise = primeiracoluna xs
ultimacoluna :: [Coordenadas] -> [Coordenadas]
ultimacoluna [] = []
ultimacoluna l@((x,y):xs) | x == xm = (x,y) : ultimacoluna xs
                          | otherwise = ultimacoluna xs
                       where xm = maximum (map fst l)