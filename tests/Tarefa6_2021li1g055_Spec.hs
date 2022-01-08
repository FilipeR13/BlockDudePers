module Tarefa6_2021li1g055_Spec where

import Test.HUnit
import LI12122
import Tarefa6_2021li1g055
import Fixtures

testsT6 = 
    test 
      [ "Tarefa 6 - Testa puzzle já resolvido" ~: Just [] ~=? resolveJogo 0 m1e6'
       ,"Tarefa 6 - Testa puzzle não resolvido" ~: Nothing ~=? resolveJogo 0 nivel1
       ,"Tarefa 6 - Resolve puzzle simples" ~:Just [AndarEsquerda,AndarEsquerda]  ~=? resolveJogo 2 m1e6
       ,"Tarefa 6 - Resolve puzzle com uma caixa" ~:  Just [AndarEsquerda,InterageCaixa,AndarEsquerda,
                                                            AndarEsquerda,InterageCaixa,Trepar,Trepar,
                                                            AndarEsquerda] ~=? resolveJogo 8 nivel1
       ,"Tarefa 6 - Resolve puzzle com duas caixas" ~:Just [AndarEsquerda,AndarEsquerda,InterageCaixa,
                                                            AndarEsquerda,InterageCaixa,Trepar,Trepar, 
                                                            AndarEsquerda,InterageCaixa,AndarEsquerda,
                                                            AndarEsquerda,Trepar,AndarEsquerda,AndarEsquerda,
                                                            Trepar,InterageCaixa,Trepar,Trepar,AndarEsquerda,AndarEsquerda]  ~=? resolveJogo 20 nivel2 
       ,"Tarefa 6 - Resolve puzzle com duas caixas com número de movimentos inferior ao necessário" ~: Nothing  ~=? resolveJogo 19 nivel2
       ,"Tarefa 6 - Resolve puzzle com três caixas" ~: Just [AndarEsquerda,Trepar,InterageCaixa,AndarDireita,
                                                          AndarDireita,AndarDireita,InterageCaixa,AndarEsquerda,
                                                          AndarEsquerda,InterageCaixa,AndarDireita,InterageCaixa,
                                                          AndarEsquerda,AndarEsquerda,InterageCaixa,AndarDireita,
                                                          AndarDireita,Trepar,InterageCaixa,Trepar,Trepar] ~=? resolveJogo 21 m1e7
       ,"Tarefa 6 - Resolver puzzle com número de movimentos insuficiente" ~: Nothing ~=? resolveJogo 20 m1e7
      ]