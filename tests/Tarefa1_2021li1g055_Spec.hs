module Tarefa1_2021li1g055_Spec where

import Test.HUnit
import LI12122
import Tarefa1_2021li1g055
import Fixtures

-- Tarefa 1
testsT1 =
  test
    [ "Tarefa 1 - Teste Valida Mapa m1r" ~: validaPotencialMapa m1 ~=? True
    , "Tarefa 1 - Teste Valida Mapa vazio" ~: validaPotencialMapa [] ~=? False
    , "Tarefa 1 - Teste Valida Mapa com 2 portas" ~: validaPotencialMapa [(Porta, (0,0)), (Porta, (1,0))] ~=?  False
    , "Tarefa 1 - Teste Valida Mapa com chão não contínuo" ~: validaPotencialMapa m1' ~=? False
    , "Tarefa 1 - Teste valida Mapa com chão não contínuo 2" ~: validaPotencialMapa m2 ~=? False
    , "Tarefa 1 - Teste Valida Mapa com coordenadas iguais" ~: validaPotencialMapa [(Bloco,(0,1)),(Bloco,(1,1)), (Bloco,(0,1))]~=? False
    ]
