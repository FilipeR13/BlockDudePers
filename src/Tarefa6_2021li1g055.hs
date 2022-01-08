{- |
Module      : Tarefa6_2021li1g055
Description : Resolução de um puzzle
Copyright   : Lucas Quintela <a100642@alunos.uminho.pt>;
            : José Rodrigues <a100692@alunos.uminho.pt>;

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2021/22.
-}
module Tarefa6_2021li1g055 where

import LI12122
import Tarefa4_2021li1g055
import Tarefa2_2021li1g055

{-|
A função 'resolveJogo' é a função principal que recebe o número de movimentos máximo que se quer que o puzzle seja resolvido
e o jogo o qual se quer resolver e devolve um Maybe [Movimento] que corresponde aos movimentos que resolvem o jogo.

Se o número de movimentos for 0 a função limita-se a verificar se o jogo já se encontra resolvido.

Para os outros casos, a função procura a solução do jogo com recurso à função auxiliar 'resolveJogoA'.
-}

resolveJogo :: Int -> Jogo -> Maybe [Movimento]
resolveJogo 0 (Jogo j (Jogador (x,y) b d)) | notElem (Porta,(x,y)) (desconstroiMapa j) = Nothing 
                                           | elem (Porta,(x,y)) (desconstroiMapa j) = Just []
resolveJogo n j = resolveJogoA n [([],j)]

{-|
A função 'resolveJogoA' recebe o número de movimentos que é possível fazer e uma lista de pares em que o primeiro componente do par
é a lista de movimentos ja feita e a segunda componete é o jogo associado a essa lista de movimentos.

A função testa se existe algum par que corresponda a um mapa resolvido, com auxílio à função 'verificaPorta' que procura um jogo possivelmente
resolvido. Se encontrar, enão irá devolver a lista de movimentos associada, caso contrário, a função é chamada recursivamente com (n-1) movimentos
que faltam executar e com uma lista nova de combinações de movimentos resultante da função 'corremovimentos'.
-}
resolveJogoA :: Int -> [([Movimento],Jogo)] -> Maybe [Movimento]
resolveJogoA 0 l = verificaPorta l 
        where verificaPorta [] = Nothing
              verificaPorta ((a,(Jogo j (Jogador (x,y) b d))):t) | elem (Porta,(x,y)) (desconstroiMapa j) = Just a 
                                                                 | otherwise = verificaPorta t
resolveJogoA n l = if verificaPorta l /= Nothing then verificaPorta l else resolveJogoA (n-1) (corremovimentos l)
            where verificaPorta [] = Nothing
                  verificaPorta ((a,(Jogo j (Jogador (x,y) b d))):t) | elem (Porta,(x,y)) (desconstroiMapa j) = Just a 
                                                                     | otherwise = verificaPorta  t
{-|
A função 'corremovimentos' é uma função recursiva que recebe a atual lista de pares de movimentos e jogos e devolve uma nova lista com
as novas combinações de movimentos e jogos encontrada.

Para cumprir esse objetivo, a função cria novos pares de movimentos e jogos para cada par da lista que recebe com auxílio da função 'corremovimentosA'
-}
corremovimentos :: [([Movimento],Jogo)] -> [([Movimento],Jogo)] 
corremovimentos [] = []
corremovimentos ((l,j):t) = corremovimentosA (l,j) (verificamovimentos j  [AndarEsquerda,AndarDireita,Trepar,InterageCaixa]) ++ corremovimentos t 
{-|
A função 'corremovimentosA' recebe um par do mesmo formato das funções anteriores e uma lista de movimentos que alteram o jogo presente nesse par. Essa lista de 
movimentos é calculada com a função 'verificamovimentos'.

Primeiramente a função teste uma série de condições que otimizam a maneira de como são feitas as combinações. Se alguma se verificar, então a função não adiciona
esse movimento à lista de movimentos do par: 

* Se o último movimento realizado for AndarEsquerda e o movimento que se quer realizar for AndarDireita;
* Se o último movimento realizado for AndarDireita e o movimento que se quer realizar for AndarEsquerda;
* Se o último movimento realizado for InterageCaixa e o movimento que se quer realizar for InterageCaixa;
* Se o último movimento realizado for Trepar, o jogador estiver virado para Oeste e o movimento que se quer realizar for AndarDireita;
* Se o último movimento realizado for Trepar, o jogador estiver virado para Este e o movimento que se quer realizar for AndarEsqueda.

Se o movimento tiver passado pelas anteriores condições então a função adiciona o mesmo à lista e realiza o movimento no jogo com recurso à função 'moveJogador' defenida
na Tarefa 4.
-}

corremovimentosA :: ([Movimento],Jogo) -> [Movimento] -> [([Movimento],Jogo)]
corremovimentosA _ [] = []
corremovimentosA (l,j@(Jogo m (Jogador (x,y) b d))) (h:t) | l /= [] && ((last l == AndarEsquerda && h == AndarDireita) || 
                                           (last l == AndarDireita && h == AndarEsquerda) || 
                                           (last l == InterageCaixa && h == InterageCaixa)||
                                           (last l == Trepar && b == Oeste && h == AndarDireita)||
                                           (last l == Trepar && b == Este && h == AndarEsquerda)) = corremovimentosA (l,j) t
                             |  otherwise = (l ++ [h], moveJogador j h) : corremovimentosA (l,j) t

{-|
A função 'verificamovimentos' recebe um jogo e a lista de movimentos que se poderá realizar e devolve a lista de movimentos que alteram o jogo dado.

A função filtra da lista os movimentos que alteram o jogo com recurso à função de ordem superior filter.
-}

verificamovimentos :: Jogo -> [Movimento] -> [Movimento]
verificamovimentos j l = filter (\x -> moveJogador j x /= j ) l