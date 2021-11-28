{- |
Module      : Tarefa3_2021li1g055
Description : Representação textual do jogo
Copyright   : Lucas Quintela <a100642@alunos.uminho.pt>;
            : José Rodrigues <a100692@alunos.uminho.pt>;

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2021/22.
-}
module Tarefa3_2021li1g055 where

import LI12122

instance Show Jogo where
  show = stringJogo 


{-|
A função 'stringJogo' é a função que vai devolver o Jogo totalmente transformado. Essencialmente, esta é apenas usada para criar um acumulador para que a função 
'transformaJogo' possa ser usada. O acumulador é o número da linha que a função auxiliar vai transformar, começando na linha 0.
-}
stringJogo :: Jogo -> String
stringJogo (Jogo a b) = transformaJogo (Jogo a b) 0 
{-|
* 'transformaJogo' utiliza o acumulador criado na função principal. Se o jogador pertencer à linha dada pelo acumulador, então a função utiliza uma função auxiliar
('transformalinhaComJogador') que coloca o jogador na sua posição da linha. Se ele não pertencer à linha então apenas vai transformar a linha utilizando a função 
'transformalinha'. Para além disso, se a função receber só um mapa com só uma lista de pecas, então a função manda transformar a linha uma última vez, não a chamando
recursivamente, com o objetivo de não apresentar "\n" no final da string.
-}

transformaJogo ::Jogo -> Int -> String
transformaJogo (Jogo [] _) _ = ""
transformaJogo (Jogo (p:pcs) (Jogador (x,y) d b)) n | pcs == [] && y/=n = transformalinha p 
                                                    | pcs == [] && y==n = transformalinhaComJogador p (Jogador (x,y) d b)
                                                    | y == n = transformalinhaComJogador p (Jogador (x,y) d b) ++ "\n" ++ transformaJogo (Jogo pcs (Jogador (x,y) d b)) (n+1)
                                                    | otherwise = transformalinha p ++ "\n" ++ transformaJogo (Jogo pcs (Jogador (x,y) d b)) (n+1)

{-|
* 'transformalinhaComJogador' vai percorrendo a lista verificando se está na mesma posição que o jogador. Se estiver, então verifica se o jogador está virado para
Oeste ou para Este devolvendo o caracter correspondente. Para além disso, esta função também vai transformando todas as outras peças em Strings, usando a função
'transformalinha'.
-}
transformalinhaComJogador :: [Peca] -> Jogador -> String 
transformalinhaComJogador (p:pcs) (Jogador (x,y) a b)|x == 0 = (case a of Oeste -> "<" 
                                                                          Este -> ">") ++ transformalinha pcs
                                                     | otherwise = transformalinha [p] ++ transformalinhaComJogador pcs (Jogador (x-1,y) a b)

{-|
'transformalinha' verifica peça a peça substituindo-as pela sua String correspondente.
-}
transformalinha :: [Peca] -> String
transformalinha [] = ""
transformalinha (x:xs) = case x of 
                                  Caixa -> "C" ++ transformalinha xs
                                  Porta -> "P" ++ transformalinha xs
                                  Vazio -> " " ++ transformalinha xs 
                                  Bloco -> "X" ++ transformalinha xs