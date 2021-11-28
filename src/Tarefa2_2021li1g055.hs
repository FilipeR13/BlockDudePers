{- |
Module      : Tarefa2_2021li1g055
Description : Construção/Desconstrução do mapa
Copyright   : Lucas Quintela <a100642@alunos.uminho.pt>;
            : José Rodrigues <a100692@alunos.uminho.pt>;

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22.
-}
module Tarefa2_2021li1g055 where

import LI12122
import Tarefa1_2021li1g055

{-|
== Constrói Mapa

A função 'constroiMapa' é a função principal que dá os argumentos necessários para que a função 'constroilinhas' possa funcionar. Esta dá a lista de peças; um acumulador, começado em 0, que é o 
número da linha; o x maximo do Mapa; o y máximo do mapa.
-}
constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa [] = []
constroiMapa pcs = constroilinhas pcs 0 (ymax pcs) (xmax pcs)

{-| 
* A função 'constroilinhas'  verifica se a função se encontra na linha máxima através do acumulador. Se não se encontrar na linha máxima, então vai construir a linha em que se encontra com 
recurso à função 'constroilinha' que recebe a lista só com as peças, ordenadas com recurso à função 'ordenalinha', pertencentes a essa linha que provém da função 'filtralinhas';
-}
constroilinhas :: [(Peca, Coordenadas)] -> Int -> Int -> Int -> [[Peca]]
constroilinhas [] n xm ym 
                | n == xm = [constroilinha [] 0 ym]
                | otherwise = constroilinha [] 0 ym : constroilinhas [] (n+1) xm ym
constroilinhas l n xm ym
                | n /= xm = constroilinha (ordenalinha (filtralinhas l n)) 0 ym : constroilinhas l (n+1) xm ym
                | otherwise =  [constroilinha (ordenalinha (filtralinhas l n)) 0 ym]

{-|
A função 'filtralinhas' recebe o mapa e o número da linha que se quer filtrar. Assim, a função percorre a lista das peças comparando a ordenada com o número da linha
pretendida e vai selecionando-as.
-}

filtralinhas :: [(Peca, Coordenadas)] -> Int -> [(Peca, Coordenadas)]
filtralinhas [] _ = []
filtralinhas ((p,(x,y)):t) l 
                | y == l = (p,(x,y)) : filtralinhas t l 
                | otherwise = filtralinhas t l 

{- |
*'constroilinha' é a função que vai comparando a posição em que está com recurso a um acumulador (posição na coluna). A função vai comparando as coordenadas das peças com o acumulador e com o x máximo
do mapa. Se as estiverem na mesma posição e se esta não for igual ao x máximo, então irá colocar a peça correspondente. Se as coordenadas não forem iguais então esta vai por colocar um Vazio. O
caso de paragem desta função é dividido em dois casos: se receber uma lista vazia mas o acumulador ainda não for igual ao x máximo, então ainda vai colocar um Vazio e voltar a chamar a função; se 
o acumulador já for igual ao x máximo então a função vai colocar o último vazio.
-}

constroilinha :: [(Peca, Coordenadas)] -> Int -> Int -> [Peca]
constroilinha [] n xm 
                | n == xm = [Vazio]
                | otherwise = Vazio : constroilinha [] (n+1) xm
constroilinha l@((a,(x,y)):t) n xm 
                |x == n && n /= xm = a : constroilinha t (n+1) xm 
                |x == xm && n /= xm = Vazio : constroilinha l (n+1) xm 
                |x == xm && n == xm = [a]  
                |otherwise = Vazio : constroilinha l (n+1) xm 

{-|
A função 'ordenalinha' apenas irá receber uma lista com peças que têm a mesma ordenada, logo apenas precisa comparar a primeira componente das coordenadas. Para conseguir ordenar a função utiliza
a função auxiliar 'insere' que vai inserindo as peças comparando as abcissas das mesmas.
-}

ordenalinha :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
ordenalinha [] = []
ordenalinha ((a,(x,y)):t) = insere (a,(x,y)) (ordenalinha t)

insere :: (Peca,Coordenadas) -> [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
insere (p,(x,y)) [] = [(p,(x,y))]
insere (p,(x,y)) ((p1,(x1,y1)):t) | x < x1 = (p,(x,y)) : ((p1,(x1,y1)):t)
                                  | otherwise = (p1,(x1,y1)) : insere (p,(x,y)) t
{-|
=== Tamanho do Mapa
As funções 'xmax' e 'ymax' servem para calcular o tamanho do mapa todo, dando a abcissa máxima e a ordenada máxima, respetivamente

Consideremos o mapa m1:
@
  [ (Porta, (0, 3)),
    (Bloco, (0, 4)),
    (Bloco, (1, 4)),
    (Bloco, (2, 4)),
    (Bloco, (3, 4)),
    (Bloco, (4, 4)),
    (Caixa, (4, 3)),
    (Bloco, (5, 4)),
    (Bloco, (6, 4)),
    (Bloco, (6, 3)),
    (Bloco, (6, 2)),
    (Bloco, (6, 1))
  ]
@

>>> xmax m1
6
>>> ymax m1
4
-}

xmax :: [(Peca,Coordenadas)] -> Int 
xmax l = maximum (map fst (soCoordenadas l))

ymax :: [(Peca,Coordenadas)] -> Int 
ymax l = maximum (map snd (soCoordenadas l))
{-|
== Desconstroi Mapa

A função 'descontroiMapa' é a função principal que tem como objetivo transformar um mapa (uma lista de peças) numa lista de pecas e coordenadas. Para isso, vai precisar da função auxiliar 
'desconstroilinhas'.
-}

desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa [] = []
desconstroiMapa ([]:t)= desconstroiMapa t
desconstroiMapa l = desconstroilinhas l 0

{-|
A 'desconstroilinhas' vai receber uma lista de listas de peças, isto é, uma lista com as várias linhas do mapa representadas em listas de peças. Vai receber também um acumulador vindo da 
função principal que vai ditar o número da linha a "desconstruir", que começa em 0. Para isso, vai chamar a função 'desconstroilinha' para a cabeça da função fornecendo um acumulador novo
começando em 0 que representa a abcissa das peças (o número de colunas), o acumulador vindo da função principal e ainda o tamanho da lista das peças menos 1, para determinar a abcissa 
máxima que uma peça poderá ter.     
-}

desconstroilinhas :: [[Peca]] -> Int -> [(Peca,Coordenadas)]
desconstroilinhas [] _ = []
desconstroilinhas (x:t) y = desconstroilinha x 0 y (length x -1) ++ desconstroilinhas t (y+1)

{-|
A 'desconstroilinha' recebe uma lista de peças (uma linha), um acumulador x que dita o número da coluna e que vem da função 'desconstroilihas', um y que é o número da 
linha e que vem da função principal e um número xm que é o comprimento da linha menos 1. Vai transformar a linha numa lista de peças e as suas respetivas coordenadas.
-}

desconstroilinha :: [Peca] -> Int -> Int -> Int -> [(Peca,(Int,Int))]
desconstroilinha [] _ _ _ = []
desconstroilinha (p:pcs) x y xm | p == Vazio = desconstroilinha pcs (x+1) y xm
                                | x /= xm = (p,(x,y)) : desconstroilinha pcs (x+1) y xm
                                |otherwise = [(p,(x,y))]