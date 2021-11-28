module Tarefa3_2021li1g055_Spec where

import Test.HUnit
import Tarefa3_2021li1g055
import Fixtures

testsT3 =
  test
    [ "Tarefa 3 - Teste Imprime Jogo m1e1" ~: "      <\n      X\n      X\nP   C X\nXXXXXXX" ~=?  show m1e1
    , "Tarefa 3 - Teste Imprime Jogo m1e2" ~: "       \n      X\n      X\nP < C X\nXXXXXXX" ~=?  show m1e2
    , "Tarefa 3 - Teste Imprime Jogo m1e3" ~: "X                   \nX                   \nX >C                \nX XX        X       \nXP X    X C X C     \nXXXXXX XXXXXXXXXXXXX\n     XXX            " ~=? show m1e3
    ]