{- | 
Module      : Tarefa5_2021li1g055
Description : Aplicação Gráfica
Copyright   : Lucas Quintela <a100642@alunos.uminho.pt>;
            : José Rodrigues <a100692@alunos.uminho.pt>;
            
Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
-}
module Main where

import LI12122
import Tarefa4_2021li1g055
import Tarefa2_2021li1g055
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe

{-|
== Dificuldades sentidas

Algumas das dificuldades sentidas ao longo desta fase:

* Demoramos algum tempo a descobrir como implementar a função de mudar de tema no jogo;
* Como dar load das imagens na função main;
* De vez em quando sentimos alguma dificuldade em perceber como implementar as diferentes funções no Estado de maneira a cumprir os nossos objetivos;
-}

data Opcoes = Jogar 
            | Sair
            | Temas

data Opcoes2 = Next
              |Restart
              |Quit
              |Resume

data Theme = Default
           | Minecraft
           | Natal
           | Inferno

data Niveis = Nivel1 
            | Nivel2
            | Nivel3
            | Nivel4
            | Nivel5
            | Nivel6
            | EasterEgg

data Menu = Controlador Opcoes
          | Pausa (Opcoes2, Jogo) 
          | Nivel Niveis
          | ModoJogo Jogo
          | VenceuNivel Opcoes2
          | Tema Theme

{-| 
== Estado

@
type Estado = (Menu,Int,[(Int,([(Peca,Picture)],(Picture,Picture)))],([(Peca,Picture)],(Picture,Picture)),Int,Int,Picture,Jogo)
@

Para começar temos o type 'Estado', este type é o conjunto de informações que vão ser utilizadas ao longo da tarefa.
Significado das variaveis do estado :

* Menu -> O construtor do data 'Menu' a ser utilizado nas funções;
* 1º Int -> Nível que se encontra o jogador            
* Lista -> Lista que contém os dados para o desenho do jogo pelo Gloss
    * Int -> Tema 
    * Par -> Ficheiros do Tema para o Gloss
        * Lista -> Lista das pictures das peças
            * Par -> Par que fornece ao Gloss as Pictures das peças
                * designa a peça
                * atribui Picture á Peça
        * Par -> Par que fornece ao Gloss as Pictures do personagem
                * Imagem do personagem orientado para Oeste
                * Imagem do personagem orientado para Este
* Par -> Pictures do tema atual
    * Lista -> Pictures das peças do tema escolhido
    * Par -> Pictures do personagem do tema escolhido
-}

type Estado = (Menu,Int,[(Int,([(Peca,Picture)],(Picture,Picture)))],([(Peca,Picture)],(Picture,Picture)),Int,Int,Picture,Jogo)

{-|
== Desenha Estado

A função 'transformaJogo' é a função que vai transformar o Estado numa só Picture (com o auxílio das funções drawOption's)

As variáveis desta função são os vários construtores do data 'Menu':

* Com o construtor "Controlador" , a função desenha as variáveis do construtor (Jogar, Tema e Sair) e torna azul o texto da variável que recebeu.
* Com o construtor "Nivel", a função vai também desenhar as variaãveis do construtor (Nível 1 , Nível 2,...) e vai também tornar azul o texto da variável selecionada.
* O construtor "Pausa" tem como variável um par que remete para os datas 'Opcoes2' e 'Menu'. Ou seja, este par vai buscar as variáveis "Resume" e "Quit" ao primeiro data e "Jogo" ao segundo. 
O que vai fazer é desenhar um menu de pausa em cada nível com as opções "Resume" e "Quit" e vai colorir de azul a opção que estiver selecionada.
-}

transformaJogo :: Estado -> Picture
transformaJogo (Controlador Jogar,_,_,_,_,_,_,_) = Pictures [Color blue $ Translate 0 70 $ drawOption "Play",drawOption "Themes", Translate (0) (-70) $ drawOption "Exit"]
transformaJogo (Controlador Temas,_,_,_,_,_,_,_) =  Pictures [Translate 0 70 $ drawOption "Play",Color blue $ drawOption "Themes", Translate (0) (-70) $ drawOption "Exit"]
transformaJogo (Controlador Sair,_,_,_,_,_,_,_) = Pictures [Translate 0 70 $ drawOption "Play",drawOption "Themes",color blue $ Translate (0) (-70) $ drawOption "Exit"]

transformaJogo (Tema Default,_,_,_,n,_,_,_) | n == 1 = Pictures [translate 12 130 $ rectangleWire 285 68 , Color blue $ Translate 0 105 $ drawOption4 "Default", Translate 0 35 $ drawOption4 "Minecraft", Translate 0 (-35) $ drawOption4 "Natal", Translate 0 (-105) $ drawOption4 "Inferno"]
                                            | n == 2 = Pictures [translate 12 60 $ rectangleWire 285 68 ,Color blue $ Translate 0 105 $ drawOption4 "Default", Translate 0 35 $ drawOption4 "Minecraft", Translate 0 (-35) $ drawOption4 "Natal", Translate 0 (-105) $ drawOption4 "Inferno"]
                                            | n == 3 = Pictures [translate 12 (-10) $ rectangleWire 285 68 ,Color blue $ Translate 0 105 $ drawOption4 "Default", Translate 0 35 $ drawOption4 "Minecraft", Translate 0 (-35) $ drawOption4 "Natal", Translate 0 (-105) $ drawOption4 "Inferno"]
                                            | n == 4 = Pictures [translate 12 (-80) $ rectangleWire 285 68 ,Color blue $ Translate 0 105 $ drawOption4 "Default", Translate 0 35 $ drawOption4 "Minecraft", Translate 0 (-35) $ drawOption4 "Natal", Translate 0 (-105) $ drawOption4 "Inferno"]
transformaJogo (Tema Minecraft,_,_,_,n,_,_,_) | n == 1 = Pictures [translate 12 130 $ rectangleWire 285 68, Translate 0 105 $ drawOption4 "Default", Color blue $ Translate 0 35 $ drawOption4 "Minecraft", Translate 0 (-35) $ drawOption4 "Natal", Translate 0 (-105) $ drawOption4 "Inferno"]
                                              | n == 2 = Pictures [translate 12 60 $ rectangleWire 285 68, Translate 0 105 $ drawOption4 "Default", Color blue $ Translate 0 35 $ drawOption4 "Minecraft", Translate 0 (-35) $ drawOption4 "Natal", Translate 0 (-105) $ drawOption4 "Inferno"]
                                              | n == 3 = Pictures [translate 12 (-10) $ rectangleWire 285 68, Translate 0 105 $ drawOption4 "Default", Color blue $ Translate 0 35 $ drawOption4 "Minecraft", Translate 0 (-35) $ drawOption4 "Natal", Translate 0 (-105) $ drawOption4 "Inferno"]
                                              | n == 4 = Pictures [translate 12 (-80) $ rectangleWire 285 68, Translate 0 105 $ drawOption4 "Default", Color blue $ Translate 0 35 $ drawOption4 "Minecraft", Translate 0 (-35) $ drawOption4 "Natal", Translate 0 (-105) $ drawOption4 "Inferno"]
transformaJogo (Tema Natal,_,_,_,n,_,_,_) | n == 1 = Pictures [translate 12 130 $ rectangleWire 285 68,Translate 0 105 $ drawOption4 "Default", Translate 0 35 $ drawOption4 "Minecraft", Color blue $ Translate 0 (-35) $ drawOption4 "Natal", Translate 0 (-105) $ drawOption4 "Inferno"]
                                          | n == 2 = Pictures [translate 12 60 $ rectangleWire 285 68,Translate 0 105 $ drawOption4 "Default", Translate 0 35 $ drawOption4 "Minecraft", Color blue $ Translate 0 (-35) $ drawOption4 "Natal", Translate 0 (-105) $ drawOption4 "Inferno"]
                                          | n == 3 = Pictures [translate 12 (-10) $ rectangleWire 285 68,Translate 0 105 $ drawOption4 "Default", Translate 0 35 $ drawOption4 "Minecraft", Color blue $ Translate 0 (-35) $ drawOption4 "Natal", Translate 0 (-105) $ drawOption4 "Inferno"]
                                          | n == 4 = Pictures [translate 12 (-80) $ rectangleWire 285 68,Translate 0 105 $ drawOption4 "Default", Translate 0 35 $ drawOption4 "Minecraft", Color blue $ Translate 0 (-35) $ drawOption4 "Natal", Translate 0 (-105) $ drawOption4 "Inferno"]
transformaJogo (Tema Inferno,_,_,_,n,_,_,_) | n == 1 = Pictures [translate 12 130 $ rectangleWire 285 68,Translate 0 105 $ drawOption4 "Default", Translate 0 35 $ drawOption4 "Minecraft", Translate 0 (-35) $ drawOption4 "Natal",Color blue $  Translate 0 (-105) $ drawOption4 "Inferno"]
                                            | n == 2 = Pictures [translate 12 60 $ rectangleWire 285 68,Translate 0 105 $ drawOption4 "Default", Translate 0 35 $ drawOption4 "Minecraft", Translate 0 (-35) $ drawOption4 "Natal",Color blue $  Translate 0 (-105) $ drawOption4 "Inferno"]
                                            | n == 3 = Pictures [translate 12 (-10) $ rectangleWire 285 68,Translate 0 105 $ drawOption4 "Default", Translate 0 35 $ drawOption4 "Minecraft", Translate 0 (-35) $ drawOption4 "Natal",Color blue $  Translate 0 (-105) $ drawOption4 "Inferno"]
                                            | n == 4 = Pictures [translate 12 (-80) $ rectangleWire 285 68,Translate 0 105 $ drawOption4 "Default", Translate 0 35 $ drawOption4 "Minecraft", Translate 0 (-35) $ drawOption4 "Natal",Color blue $  Translate 0 (-105) $ drawOption4 "Inferno"]

transformaJogo (Nivel Nivel1,_,_,_,_,_,_,_) = Pictures [Color blue $ Translate 0 175 $ drawOption "Level 1",translate 0 105 $ drawOption "Level 2", Translate 0 35 $ drawOption "Level 3",translate 0 (-35) $ drawOption "Level 4", Translate 0 (-105) $ drawOption "Level 5", translate 0 (-175) $ drawOption "Level 6"]
transformaJogo (Nivel Nivel2,_,_,_,_,_,_,_) = Pictures [Translate 0 175 $ drawOption "Level 1",color blue  $ translate 0 105 $ drawOption "Level 2", Translate 0 35 $ drawOption "Level 3",translate 0 (-35) $ drawOption "Level 4", Translate 0 (-105) $ drawOption "Level 5", translate 0 (-175) $ drawOption "Level 6"]
transformaJogo (Nivel Nivel3,_,_,_,_,_,_,_) = Pictures [Translate 0 175 $ drawOption "Level 1", translate 0 105 $ drawOption "Level 2", color blue $ Translate 0 35 $ drawOption "Level 3",translate 0 (-35) $ drawOption "Level 4", Translate 0 (-105) $ drawOption "Level 5", translate 0 (-175) $ drawOption "Level 6"] 
transformaJogo (Nivel Nivel4,_,_,_,_,_,_,_) = Pictures [Translate 0 175 $ drawOption "Level 1", translate 0 105 $ drawOption "Level 2", Translate 0 35 $ drawOption "Level 3", color blue $ translate 0 (-35) $ drawOption "Level 4", Translate 0 (-105) $ drawOption "Level 5", translate 0 (-175) $ drawOption "Level 6"]
transformaJogo (Nivel Nivel5,_,_,_,_,_,_,_) = Pictures [Translate 0 175 $ drawOption "Level 1", translate 0 105 $ drawOption "Level 2", Translate 0 35 $ drawOption "Level 3", translate 0 (-35) $ drawOption "Level 4", color blue $ Translate 0 (-105) $ drawOption "Level 5", translate 0 (-175) $ drawOption "Level 6"]
transformaJogo (Nivel Nivel6,_,_,_,_,_,_,_) = Pictures [Translate 0 175 $ drawOption "Level 1", translate 0 105 $ drawOption "Level 2", Translate 0 35 $ drawOption "Level 3", translate 0 (-35) $ drawOption "Level 4",Translate 0 (-105) $ drawOption "Level 5",color blue $ translate 0 (-175) $ drawOption "Level 6"]

transformaJogo (VenceuNivel Next,1,l,_,_,m,p,_) |m <= 10 = Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed", color blue $ Translate (-267.5) (0) $ drawOption2 "Next Level", Translate (35) (0) $ drawOption2 "Restart Level", Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-160) 0 $ Scale (1.5) (1.5) $ p,translate (0) 0 $ Scale (1.5) (1.5) $ p,translate (160) 0 $ Scale (1.5) (1.5) $ p]
                                                |m <= 14 =  Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed", color blue $ Translate (-267.5) (0) $ drawOption2 "Next Level", Translate (35) (0) $ drawOption2 "Restart Level", Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-65) 0 $ Scale (1.5) (1.5) $ p,translate 65 0 $ Scale (1.5) (1.5) $ p]
                                                |otherwise =  Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed", color blue $ Translate (-267.5) (0) $ drawOption2 "Next Level", Translate (35) (0) $ drawOption2 "Restart Level", Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-3) 0 $ Scale (1.5) (1.5) $ p]
transformaJogo (VenceuNivel Restart ,1,_,_,_,m,p,_) | m <= 10 = Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed",Translate (-267.5) (0) $ drawOption2 "Next Level",color blue $  Translate (35) (0) $ drawOption2 "Restart Level", Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-160) 0 $ Scale (1.5) (1.5) $ p,translate (0) 0 $ Scale (1.5) (1.5) $ p,translate (160) 0 $ Scale (1.5) (1.5) $ p]
                                                    | m <= 14 = Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed",Translate (-267.5) (0) $ drawOption2 "Next Level",color blue $  Translate (35) (0) $ drawOption2 "Restart Level", Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450, translate (-65) 0 $ Scale (1.5) (1.5) $ p,translate 65 0 $ Scale (1.5) (1.5) $ p]
                                                    | otherwise = Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed",Translate (-267.5) (0) $ drawOption2 "Next Level",color blue $  Translate (35) (0) $ drawOption2 "Restart Level", Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450, translate (-3) 0 $ Scale (1.5) (1.5) $ p]
transformaJogo (VenceuNivel Quit,1,_,_,_,m,p,_) | m <= 10 =  Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed", Translate (-267.5) (0) $ drawOption2 "Next Level",Translate (35) (0) $ drawOption2 "Restart Level", color blue $  Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-160) 0 $ Scale (1.5) (1.5) $ p,translate (0) 0 $ Scale (1.5) (1.5) $ p,translate (160) 0 $ Scale (1.5) (1.5) $ p]
                                                | m <= 14 =  Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed", Translate (-267.5) (0) $ drawOption2 "Next Level",Translate (35) (0) $ drawOption2 "Restart Level", color blue $  Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-65) 0 $ Scale (1.5) (1.5) $ p,translate 65 0 $ Scale (1.5) (1.5) $ p]
                                                | otherwise =  Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed", Translate (-267.5) (0) $ drawOption2 "Next Level",Translate (35) (0) $ drawOption2 "Restart Level", color blue $  Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-3) 0 $ Scale (1.5) (1.5) $ p]

transformaJogo (VenceuNivel Next,2,l,_,_,m,p,_) |m <= 22 = Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed", color blue $ Translate (-267.5) (0) $ drawOption2 "Next Level", Translate (35) (0) $ drawOption2 "Restart Level", Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-160) 0 $ Scale (1.5) (1.5) $ p,translate (0) 0 $ Scale (1.5) (1.5) $ p,translate (160) 0 $ Scale (1.5) (1.5) $ p]
                                                |m <= 28 =  Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed", color blue $ Translate (-267.5) (0) $ drawOption2 "Next Level", Translate (35) (0) $ drawOption2 "Restart Level", Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-65) 0 $ Scale (1.5) (1.5) $ p,translate 65 0 $ Scale (1.5) (1.5) $ p]
                                                |otherwise =  Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed", color blue $ Translate (-267.5) (0) $ drawOption2 "Next Level", Translate (35) (0) $ drawOption2 "Restart Level", Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-3) 0 $ Scale (1.5) (1.5) $ p]
transformaJogo (VenceuNivel Restart ,2,_,_,_,m,p,_) | m <= 22 = Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed",Translate (-267.5) (0) $ drawOption2 "Next Level",color blue $  Translate (35) (0) $ drawOption2 "Restart Level", Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-160) 0 $ Scale (1.5) (1.5) $ p,translate (0) 0 $ Scale (1.5) (1.5) $ p,translate (160) 0 $ Scale (1.5) (1.5) $ p]
                                                    | m <= 28 = Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed",Translate (-267.5) (0) $ drawOption2 "Next Level",color blue $  Translate (35) (0) $ drawOption2 "Restart Level", Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450, translate (-65) 0 $ Scale (1.5) (1.5) $ p,translate 65 0 $ Scale (1.5) (1.5) $ p]
                                                    | otherwise = Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed",Translate (-267.5) (0) $ drawOption2 "Next Level",color blue $  Translate (35) (0) $ drawOption2 "Restart Level", Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450, translate (-3) 0 $ Scale (1.5) (1.5) $ p]
transformaJogo (VenceuNivel Quit,2,_,_,_,m,p,_) | m <= 22 =  Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed", Translate (-267.5) (0) $ drawOption2 "Next Level",Translate (35) (0) $ drawOption2 "Restart Level", color blue $  Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-160) 0 $ Scale (1.5) (1.5) $ p,translate (0) 0 $ Scale (1.5) (1.5) $ p,translate (160) 0 $ Scale (1.5) (1.5) $ p]
                                                | m <= 28 =  Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed", Translate (-267.5) (0) $ drawOption2 "Next Level",Translate (35) (0) $ drawOption2 "Restart Level", color blue $  Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-65) 0 $ Scale (1.5) (1.5) $ p,translate 65 0 $ Scale (1.5) (1.5) $ p]
                                                | otherwise =  Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed", Translate (-267.5) (0) $ drawOption2 "Next Level",Translate (35) (0) $ drawOption2 "Restart Level", color blue $  Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-3) 0 $ Scale (1.5) (1.5) $ p]
                          
transformaJogo (VenceuNivel Next,3,l,_,_,m,p,_) |m <= 225 = Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed", color blue $ Translate (-267.5) (0) $ drawOption2 "Next Level", Translate (35) (0) $ drawOption2 "Restart Level", Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-160) 0 $ Scale (1.5) (1.5) $ p,translate (0) 0 $ Scale (1.5) (1.5) $ p,translate (160) 0 $ Scale (1.5) (1.5) $ p]
                                                |m <= 255 =  Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed", color blue $ Translate (-267.5) (0) $ drawOption2 "Next Level", Translate (35) (0) $ drawOption2 "Restart Level", Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-65) 0 $ Scale (1.5) (1.5) $ p,translate 65 0 $ Scale (1.5) (1.5) $ p]
                                                |otherwise =  Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed", color blue $ Translate (-267.5) (0) $ drawOption2 "Next Level", Translate (35) (0) $ drawOption2 "Restart Level", Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-3) 0 $ Scale (1.5) (1.5) $ p]
transformaJogo (VenceuNivel Restart ,3,_,_,_,m,p,_) | m <= 225 = Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed",Translate (-267.5) (0) $ drawOption2 "Next Level",color blue $  Translate (35) (0) $ drawOption2 "Restart Level", Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-160) 0 $ Scale (1.5) (1.5) $ p,translate (0) 0 $ Scale (1.5) (1.5) $ p,translate (160) 0 $ Scale (1.5) (1.5) $ p]
                                                    | m <= 255 = Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed",Translate (-267.5) (0) $ drawOption2 "Next Level",color blue $  Translate (35) (0) $ drawOption2 "Restart Level", Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450, translate (-65) 0 $ Scale (1.5) (1.5) $ p,translate 65 0 $ Scale (1.5) (1.5) $ p]
                                                    | otherwise = Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed",Translate (-267.5) (0) $ drawOption2 "Next Level",color blue $  Translate (35) (0) $ drawOption2 "Restart Level", Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450, translate (-3) 0 $ Scale (1.5) (1.5) $ p]
transformaJogo (VenceuNivel Quit,3,_,_,_,m,p,_) | m <= 225 =  Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed", Translate (-267.5) (0) $ drawOption2 "Next Level",Translate (35) (0) $ drawOption2 "Restart Level", color blue $  Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-160) 0 $ Scale (1.5) (1.5) $ p,translate (0) 0 $ Scale (1.5) (1.5) $ p,translate (160) 0 $ Scale (1.5) (1.5) $ p]
                                                | m <= 255 =  Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed", Translate (-267.5) (0) $ drawOption2 "Next Level",Translate (35) (0) $ drawOption2 "Restart Level", color blue $  Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-65) 0 $ Scale (1.5) (1.5) $ p,translate 65 0 $ Scale (1.5) (1.5) $ p]
                                                | otherwise =  Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed", Translate (-267.5) (0) $ drawOption2 "Next Level",Translate (35) (0) $ drawOption2 "Restart Level", color blue $  Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-3) 0 $ Scale (1.5) (1.5) $ p]

transformaJogo (VenceuNivel Next,4,l,_,_,m,p,_) |m <= 91 = Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed", color blue $ Translate (-267.5) (0) $ drawOption2 "Next Level", Translate (35) (0) $ drawOption2 "Restart Level", Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-160) 0 $ Scale (1.5) (1.5) $ p,translate (0) 0 $ Scale (1.5) (1.5) $ p,translate (160) 0 $ Scale (1.5) (1.5) $ p]
                                                |m <= 99 =  Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed", color blue $ Translate (-267.5) (0) $ drawOption2 "Next Level", Translate (35) (0) $ drawOption2 "Restart Level", Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-65) 0 $ Scale (1.5) (1.5) $ p,translate 65 0 $ Scale (1.5) (1.5) $ p]
                                                |otherwise =  Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed", color blue $ Translate (-267.5) (0) $ drawOption2 "Next Level", Translate (35) (0) $ drawOption2 "Restart Level", Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-3) 0 $ Scale (1.5) (1.5) $ p]
transformaJogo (VenceuNivel Restart ,4,_,_,_,m,p,_) | m <= 91 = Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed",Translate (-267.5) (0) $ drawOption2 "Next Level",color blue $  Translate (35) (0) $ drawOption2 "Restart Level", Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-160) 0 $ Scale (1.5) (1.5) $ p,translate (0) 0 $ Scale (1.5) (1.5) $ p,translate (160) 0 $ Scale (1.5) (1.5) $ p]
                                                    | m <= 99 = Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed",Translate (-267.5) (0) $ drawOption2 "Next Level",color blue $  Translate (35) (0) $ drawOption2 "Restart Level", Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450, translate (-65) 0 $ Scale (1.5) (1.5) $ p,translate 65 0 $ Scale (1.5) (1.5) $ p]
                                                    | otherwise = Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed",Translate (-267.5) (0) $ drawOption2 "Next Level",color blue $  Translate (35) (0) $ drawOption2 "Restart Level", Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450, translate (-3) 0 $ Scale (1.5) (1.5) $ p]
transformaJogo (VenceuNivel Quit,4,_,_,_,m,p,_) | m <= 91 =  Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed", Translate (-267.5) (0) $ drawOption2 "Next Level",Translate (35) (0) $ drawOption2 "Restart Level", color blue $  Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-160) 0 $ Scale (1.5) (1.5) $ p,translate (0) 0 $ Scale (1.5) (1.5) $ p,translate (160) 0 $ Scale (1.5) (1.5) $ p]
                                                | m <= 99 =  Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed", Translate (-267.5) (0) $ drawOption2 "Next Level",Translate (35) (0) $ drawOption2 "Restart Level", color blue $  Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-65) 0 $ Scale (1.5) (1.5) $ p,translate 65 0 $ Scale (1.5) (1.5) $ p]
                                                | otherwise =  Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed", Translate (-267.5) (0) $ drawOption2 "Next Level",Translate (35) (0) $ drawOption2 "Restart Level", color blue $  Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-3) 0 $ Scale (1.5) (1.5) $ p]

transformaJogo (VenceuNivel Next,5,l,_,_,m,p,_) |m <= 380 = Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed", color blue $ Translate (-267.5) (0) $ drawOption2 "Next Level", Translate (35) (0) $ drawOption2 "Restart Level", Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-160) 0 $ Scale (1.5) (1.5) $ p,translate (0) 0 $ Scale (1.5) (1.5) $ p,translate (160) 0 $ Scale (1.5) (1.5) $ p]
                                                |m <= 395 =  Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed", color blue $ Translate (-267.5) (0) $ drawOption2 "Next Level", Translate (35) (0) $ drawOption2 "Restart Level", Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-65) 0 $ Scale (1.5) (1.5) $ p,translate 65 0 $ Scale (1.5) (1.5) $ p]
                                                |otherwise =  Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed", color blue $ Translate (-267.5) (0) $ drawOption2 "Next Level", Translate (35) (0) $ drawOption2 "Restart Level", Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-3) 0 $ Scale (1.5) (1.5) $ p]
transformaJogo (VenceuNivel Restart ,5,_,_,_,m,p,_) | m <= 380 = Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed",Translate (-267.5) (0) $ drawOption2 "Next Level",color blue $  Translate (35) (0) $ drawOption2 "Restart Level", Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-160) 0 $ Scale (1.5) (1.5) $ p,translate (0) 0 $ Scale (1.5) (1.5) $ p,translate (160) 0 $ Scale (1.5) (1.5) $ p]
                                                    | m <= 395 = Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed",Translate (-267.5) (0) $ drawOption2 "Next Level",color blue $  Translate (35) (0) $ drawOption2 "Restart Level", Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450, translate (-65) 0 $ Scale (1.5) (1.5) $ p,translate 65 0 $ Scale (1.5) (1.5) $ p]
                                                    | otherwise = Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed",Translate (-267.5) (0) $ drawOption2 "Next Level",color blue $  Translate (35) (0) $ drawOption2 "Restart Level", Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450, translate (-3) 0 $ Scale (1.5) (1.5) $ p]
transformaJogo (VenceuNivel Quit,5,_,_,_,m,p,_) | m <= 380 =  Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed", Translate (-267.5) (0) $ drawOption2 "Next Level",Translate (35) (0) $ drawOption2 "Restart Level", color blue $  Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-160) 0 $ Scale (1.5) (1.5) $ p,translate (0) 0 $ Scale (1.5) (1.5) $ p,translate (160) 0 $ Scale (1.5) (1.5) $ p]
                                                | m <= 395 =  Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed", Translate (-267.5) (0) $ drawOption2 "Next Level",Translate (35) (0) $ drawOption2 "Restart Level", color blue $  Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-65) 0 $ Scale (1.5) (1.5) $ p,translate 65 0 $ Scale (1.5) (1.5) $ p]
                                                | otherwise =  Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed", Translate (-267.5) (0) $ drawOption2 "Next Level",Translate (35) (0) $ drawOption2 "Restart Level", color blue $  Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-3) 0 $ Scale (1.5) (1.5) $ p]
transformaJogo (VenceuNivel Restart ,6,_,_,_,m,p,_) | m <= 120 = Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed",color blue $  Translate (-200) (0) $ drawOption2 "Restart Level", Translate (300) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-160) 0 $ Scale (1.5) (1.5) $ p,translate (0) 0 $ Scale (1.5) (1.5) $ p,translate (160) 0 $ Scale (1.5) (1.5) $ p]
                                                    | m <= 126 = Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed",color blue $  Translate (-200) (0) $ drawOption2 "Restart Level", Translate (300) (0) $ drawOption2 "Quit", rectangleWire 815 450, translate (-65) 0 $ Scale (1.5) (1.5) $ p,translate 65 0 $ Scale (1.5) (1.5) $ p]
                                                    | otherwise = Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed",color blue $  Translate (-200) (0) $ drawOption2 "Restart Level", Translate (300) (0) $ drawOption2 "Quit", rectangleWire 815 450, translate (-3) 0 $ Scale (1.5) (1.5) $ p]
transformaJogo (VenceuNivel Quit,6,_,_,_,m,p,_) | m <= 120 =  Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed",Translate (-200) (0) $ drawOption2 "Restart Level", color blue $  Translate (300) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-160) 0 $ Scale (1.5) (1.5) $ p,translate (0) 0 $ Scale (1.5) (1.5) $ p,translate (160) 0 $ Scale (1.5) (1.5) $ p]
                                                | m <= 126 =  Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed",Translate (-200) (0) $ drawOption2 "Restart Level", color blue $  Translate (300) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-65) 0 $ Scale (1.5) (1.5) $ p,translate 65 0 $ Scale (1.5) (1.5) $ p]
                                                | otherwise =  Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed",Translate (-200) (0) $ drawOption2 "Restart Level",color blue $  Translate (300) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-3) 0 $ Scale (1.5) (1.5) $ p]

transformaJogo (Pausa (Resume,j),_,_,_,_,_,_,_) = Pictures [rectangleWire 650 150, color blue $ Translate (-147.5) (-12) $ drawOption3 "Resume", Translate (250) (-12) $ drawOption3 "Quit"]
transformaJogo (Pausa (Quit,j),_,_,_,_,_,_,_) = Pictures [rectangleWire 650 150, Translate (-147.5) (-12) $ drawOption3 "Resume",color blue $ Translate (250) (-12) $ drawOption3 "Quit"]


transformaJogo (ModoJogo ((Jogo j (Jogador (x,y) b d))),_,l,t,_,_,_,_) = translate adjustx adjusty (pictures $ transformaJogoAux (desconstroiMapa j) (Jogador (x,y) b d) t) 
                                              where adjusty = 32 * fromIntegral (ymax (desconstroiMapa j)) / 2 
                                                    adjustx = -32 * fromIntegral (xmax (desconstroiMapa j)) / 2
transformaJogoAux :: [(Peca,Coordenadas)] -> Jogador -> ([(Peca,Picture)],(Picture,Picture)) -> [Picture]
transformaJogoAux [] (Jogador (x,y) dir b) l = case dir of Este -> if b then [translate z w (snd (snd l)), translate z w' (fromJust (lookup Caixa (fst l)))] else [translate z w (snd (snd l))]
                                                           Oeste -> if b then [translate z w (fst (snd l)), translate z w' (fromJust (lookup Caixa (fst l)))] else [translate z w (fst (snd l))]    
      where  z = 32 * fromIntegral x
             w = 32 * fromIntegral (-y)
             w' = 32 * fromIntegral (-(y-1))
transformaJogoAux m@((p,(a,b)):t) (Jogador (x,y) dir s) l
                                 | p == Bloco = translate c d (fromJust (lookup Bloco (fst l))) : transformaJogoAux t (Jogador (x,y) dir s) l
                                 | p == Porta = translate c d (fromJust (lookup Porta (fst l))) : transformaJogoAux t (Jogador (x,y) dir s) l
                                 | p == Caixa = translate c d (fromJust (lookup Caixa (fst l))) : transformaJogoAux t (Jogador (x,y) dir s) l
                                 | otherwise = transformaJogoAux t (Jogador (x,y) dir s) l
   where c = 32 * fromIntegral a
         d = 32 * fromIntegral (-b)

{-|
=== DrawOption

Estas funções tẽm todas o mesmo objetivo -> Aplicam um conjunto de modificações a um texto e transforma o numa Picture.
-}

drawOption option = Translate (-50) 0 $ Scale (0.5) (0.5) $ Text option
drawOption2 option = Translate (-100) (-190) $ Scale (0.3) (0.3) $ Text option
drawOption3 option = Translate (-100) (0) $ Scale (0.4) (0.4) $ Text option
drawOption4 option = Translate (-120) 0 $ Scale (0.5) (0.5) $ Text option

estadoGlossInicial :: [(Int,([(Peca,Picture)],(Picture,Picture)))]-> Picture -> Estado 
estadoGlossInicial l p = (Controlador Jogar,0,l,fromJust (lookup 1 l),1,0,p, easteregg)

nivel1 :: Jogo
nivel1 = 
  Jogo   [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Bloco, Vazio, Vazio, Caixa, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ] (Jogador (6, 0) Oeste False)

nivel2 :: Jogo
nivel2 =
  Jogo [ [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
    [Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
    [Bloco,Porta,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Caixa,Vazio,Bloco,Vazio,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio],
    [Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco],
    [Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
  ] (Jogador (17,4) Este False)

nivel3 :: Jogo
nivel3 = 
  Jogo   [ [Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Porta,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco],
    [Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Caixa,Caixa,Bloco],
    [Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Caixa,Vazio,Bloco,Vazio,Vazio,Caixa,Vazio,Vazio,Bloco,Vazio,Caixa,Vazio,Bloco,Vazio,Vazio,Caixa,Caixa,Caixa,Bloco],
    [Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]
  ] (Jogador (13, 12) Oeste False)

nivel4 :: Jogo 
nivel4 = 
  Jogo[ [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Vazio,Vazio,Porta,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Caixa,Caixa,Bloco],
        [Bloco,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco],
        [Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco,Vazio,Vazio],
        [Vazio,Bloco,Caixa,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Caixa,Caixa,Bloco,Vazio,Vazio],
        [Vazio,Bloco,Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio],
        [Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
        [Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
        [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
      ] (Jogador (8,7) Este False)

nivel5 :: Jogo 
nivel5 = 
  Jogo [   [Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Porta,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Vazio,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Caixa,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Caixa,Caixa,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Caixa,Caixa,Caixa,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Caixa,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]
    ] (Jogador (12,13) Este False)

nivel6 :: Jogo 
nivel6 = 
  Jogo [ [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
         [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
         [Bloco,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
         [Bloco,Caixa,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
         [Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
         [Vazio,Vazio,Vazio,Bloco,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Porta,Bloco],
         [Vazio,Vazio,Vazio,Bloco,Caixa,Caixa,Vazio,Vazio,Vazio,Caixa,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco],
         [Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio],
         [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio],
         [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio],
         [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Caixa,Caixa,Vazio,Vazio,Vazio,Caixa,Bloco,Vazio,Vazio],
         [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio]
  ] (Jogador (7,6) Este False)

easteregg :: Jogo 
easteregg = 
  Jogo [[Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco],
        [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Bloco,Vazio,Bloco,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Porta,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]
  ] (Jogador (30,20) Oeste False)

{-|
== Reage Evento

A função 'reageEventoGloss' é a função que vai permitir jogar o jogo em si, isto é, vai atribuir comandos ao pressionar das teclas.

Se a função receber o construtor "Controlador" vai permitir ao jogador viajar entre as opções desse construtor. Isto é, vai poder
mudar, no menu inicial, entre as opções "Play", " Themes" e "Exit" e selecionar a que pretender. Se selecionar a opção "Play" a
função muda o construtor do Estado para "Nivel ..", ou seja, inicia o menu dos níveis em que vai ser possível selecionar o nível
pretendido. Se selecionar a opção "Themes" a função leva o jogador para o menu dos temas, trocando a variável do construtor inicial 
para "Temas". Por fim, se selecionar a opcão "Sair", o jogo fecha.

Quando recebe o construtor "Nivel ..." é o que permite escolher qual nível jogar. Resumidamente, ao pressionar a tecla
"Enter" na opção de algum nível, o que a função vai fazer é mudar o construtor do Estado para "Modo Jogo" sendo o argumento do 
construtor o nível escolhido.

Com o construtor "Tema" é o que permite selecionar com qual tema se vai jogar. O que a função faz quando se pressiona "Enter" é
mudar o argumento dos temas no Estado para o tema correspondente. Para isso vai procurar o Int referente a cada tema na lista
de todas os temas.

Se o jogador quando se encontrar em algum nível pressionar a tecla "p", é levado para o menu Pausa em que vai poder escolher as
opções "Resume" se quiser voltar ao jogo, ou "Quit" se quiser voltar para o menu principal. A função faz isso através do mesmo 
método descrito em cima, isto é, mudar o construtor do Estado.
-}


reageEventoGloss :: Event -> Estado -> Estado
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Jogar,0,l,t,n,0,p,e) = (Nivel Nivel1,0,l,t,n,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Jogar,0,l,t,n,0,p,e) = (Controlador Sair,0,l,t,n,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Jogar,0,l,t,n,0,p,e) = (Controlador Temas,0,l,t,n,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Sair,0,l,t,n,0,p,e) =(Controlador Temas,0,l,t,n,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Sair,0,l,t,n,0,p,e) = (Controlador Jogar,0,l,t,n,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Temas,0,l,t,n,0,p,e) = (Controlador Sair,0,l,t,n,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Temas,0,l,t,n,0,p,e) = (Controlador Jogar,0,l,t,n,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Temas,0,l,t,n,0,p,e) = (Tema Default,0,l,t,n,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Sair,0,l,t,n,0,p,e) = undefined

reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (Nivel Nivel1,0,l,t,n,0,p,e) =( Nivel Nivel6,0,l,t,n,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (Nivel Nivel1,0,l,t,n,0,p,e) = (Nivel Nivel2,0,l,t,n,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (Nivel Nivel2,0,l,t,n,0,p,e) =( Nivel Nivel1,0,l,t,n,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (Nivel Nivel2,0,l,t,n,0,p,e) = (Nivel Nivel3,0,l,t,n,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Nivel Nivel1,0,l,t,n,0,p,e) =(ModoJogo nivel1,1,l,t,n,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Nivel Nivel2,0,l,t,n,0,p,e) = (ModoJogo nivel2,2,l,t,n,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyDelete) Down _ _) (Nivel _,0,l,t,n,0,p,e) = (Controlador Jogar,0,l,t,n,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (Nivel Nivel3,0,l,t,n,0,p,e) = (Nivel Nivel4,0,l,t,n,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (Nivel Nivel3,0,l,t,n,0,p,e) =( Nivel Nivel2,0,l,t,n,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Nivel Nivel3,0,l,t,n,0,p,e) = (ModoJogo nivel3,3,l,t,n,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (Nivel Nivel4,0,l,t,n,0,p,e) =( Nivel Nivel3,0,l,t,n,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (Nivel Nivel4,0,l,t,n,0,p,e) =( Nivel Nivel5,0,l,t,n,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Nivel Nivel4,0,l,t,n,0,p,e) = (ModoJogo nivel4,4,l,t,n,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (Nivel Nivel5,0,l,t,n,0,p,e) =( Nivel Nivel4,0,l,t,n,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (Nivel Nivel5,0,l,t,n,0,p,e) =( Nivel Nivel6,0,l,t,n,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Nivel Nivel5,0,l,t,n,0,p,e) = (ModoJogo nivel5,5,l,t,n,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (Nivel Nivel6,0,l,t,n,0,p,e) =( Nivel Nivel5,0,l,t,n,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (Nivel Nivel6,0,l,t,n,0,p,e) =( Nivel Nivel1,0,l,t,n,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Nivel Nivel6,0,l,t,n,0,p,e) =( ModoJogo nivel6,6,l,t,n,0,p,e)

reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (Tema Default,0,l,t,n,0,p,e) = (Tema Minecraft,0,l,t,n,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (Tema Minecraft,0,l,t,n,0,p,e) = (Tema Natal,0,l,t,n,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (Tema Natal,0,l,t,n,0,p,e) = (Tema Inferno,0,l,t,n,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (Tema Inferno,0,l,t,n,0,p,e) = (Tema Default,0,l,t,n,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (Tema Default,0,l,t,n,0,p,e) = (Tema Inferno,0,l,t,n,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (Tema Minecraft,0,l,t,n,0,p,e) = (Tema Default,0,l,t,n,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (Tema Natal,0,l,t,n,0,p,e) = (Tema Minecraft,0,l,t,n,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (Tema Inferno,0,l,t,n,0,p,e) = (Tema Natal,0,l,t,n,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Tema Default,0,l,t,n,0,p,e) = (Tema Default,0,l,fromJust (lookup 1 l),1,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Tema Minecraft,0,l,t,n,0,p,e) = (Tema Minecraft,0,l,fromJust (lookup 2 l),2,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Tema Natal,0,l,t,n,0,p,e) = (Tema Natal,0,l,fromJust (lookup 3 l),3,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Tema Inferno,0,l,t,n,0,p,e) = (Tema Inferno,0,l,fromJust (lookup 4 l),4,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyDelete) Down _ _) (Tema _,0,l,t,n,0,p,e) = (Controlador Temas,0,l,t,n,0,p,e)

reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (VenceuNivel Restart,6,l,t,a,n,p,e) = (VenceuNivel Quit,6,l,t,a,n,p,e)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (VenceuNivel Quit,6,l,t,a,n,p,e) = (VenceuNivel Restart,6,l,t,a,n,p,e)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (VenceuNivel Next,o,l,t,a,n,p,e) = (VenceuNivel Restart,o,l,t,a,n,p,e)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (VenceuNivel Restart,o,l,t,a,n,p,e) = (VenceuNivel Quit,o,l,t,a,n,p,e)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (VenceuNivel Quit,o,l,t,a,n,p,e) = (VenceuNivel Next,o,l,t,a,n,p,e)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (VenceuNivel Next,o,l,t,a,n,p,e) = (VenceuNivel Quit,o,l,t,a,n,p,e)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (VenceuNivel Restart,o,l,t,a,n,p,e) = (VenceuNivel Next,o,l,t,a,n,p,e)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (VenceuNivel Quit,o,l,t,a,n,p,e) = (VenceuNivel Restart,o,l,t,a,n,p,e)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuNivel Next,1,l,t,a,n,p,e) = (ModoJogo nivel2,2,l,t,a,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuNivel Restart,1,l,t,a,n,p,e) = (ModoJogo nivel1,1,l,t,a,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuNivel Quit,o,l,t,a,n,p,e) = (Controlador Jogar,0,l,t,a,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuNivel Next,2,l,t,a,n,p,e) = (ModoJogo nivel3,3,l,t,a,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuNivel Restart,2,l,t,a,n,p,e) = (ModoJogo nivel2,2,l,t,a,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuNivel Restart,3,l,t,a,n,p,e) = (ModoJogo nivel3,3,l,t,a,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuNivel Next,3,l,t,a,n,p,e) = (ModoJogo nivel4,4,l,t,a,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuNivel Restart,4,l,t,a,n,p,e) = (ModoJogo nivel4,4,l,t,a,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuNivel Next,4,l,t,a,n,p,e) = (ModoJogo nivel5,5,l,t,a,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuNivel Restart,5,l,t,a,n,p,e) = (ModoJogo nivel5,5,l,t,a,0,p,e)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuNivel Restart,6,l,t,a,n,p,e) = (ModoJogo nivel6,6,l,t,a,0,p,e)

reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo j@(Jogo mapa (Jogador (0,8) Oeste False)),3,l,t,a,m,p,e) = (ModoJogo e,7,l,t,a,m,p,j)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo j,7,l,t,a,m,p,e) = (ModoJogo (moveJogador j Trepar),7,l,t,a,m,p,e)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo j,7,l,t,a,m,p,e) =(ModoJogo (moveJogador j InterageCaixa),7,l,t,a,m,p,e) 
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo j,7,l,t,a,m,p,e) =(ModoJogo (moveJogador j AndarDireita),7,l,t,a,m,p,e)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo j,7,l,t,a,m,p,e) =(ModoJogo (moveJogador j AndarEsquerda),7,l,t,a,m,p,e)

reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo j,n,l,t,a,m,p,e) = if j == moveJogador j Trepar then (ModoJogo (moveJogador j Trepar),n,l,t,a,m,p,e) else (ModoJogo (moveJogador j Trepar),n,l,t,a,m+1,p,e)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo j,n,l,t,a,m,p,e) =if j == moveJogador j InterageCaixa then (ModoJogo (moveJogador j InterageCaixa),n,l,t,a,m,p,e) else (ModoJogo (moveJogador j InterageCaixa),n,l,t,a,m+1,p,e)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo j,n,l,t,a,m,p,e) =if j == moveJogador j AndarDireita then (ModoJogo (moveJogador j AndarDireita),n,l,t,a,m,p,e) else (ModoJogo (moveJogador j AndarDireita),n,l,t,a,m+1,p,e)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo j,n,l,t,a,m,p,e) =if j == moveJogador j AndarEsquerda then (ModoJogo (moveJogador j AndarEsquerda),n,l,t,a,m,p,e) else (ModoJogo (moveJogador j AndarEsquerda),n,l,t,a,m+1,p,e)

reageEventoGloss (EventKey (Char 'r') Down _ _) (ModoJogo j,n,l,t,a,m,p,e) | n == 1 = (ModoJogo nivel1,n,l,t,a,0,p,e) 
                                                                           | n == 2 = (ModoJogo nivel2,n,l,t,a,0,p,e)
                                                                           | n == 3 = (ModoJogo nivel3,n,l,t,a,0,p,e)
                                                                           | n == 4 = (ModoJogo nivel4,n,l,t,a,0,p,e)
                                                                           | n == 5 = (ModoJogo nivel5,n,l,t,a,0,p,e)
                                                                           | n == 6 = (ModoJogo nivel6,n,l,t,a,0,p,e)

reageEventoGloss (EventKey (Char 'p') Down _ _) (ModoJogo j,n,l,t,a,m,p,e) = (Pausa (Resume,j),n,l,t,a,m,p,e)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (Pausa (Resume,j),n,l,t,a,m,p,e) = (Pausa (Quit,j),n,l,t,a,m,p,e)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (Pausa (Quit,j),n,l,t,a,m,p,e) = (Pausa (Resume,j),n,l,t,a,m,p,e)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (Pausa (Resume,j),n,l,t,a,m,p,e) = (Pausa (Quit,j),n,l,t,a,m,p,e)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (Pausa (Quit,j),n,l,t,a,m,p,e) = (Pausa (Resume,j),n,l,t,a,m,p,e)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa (Resume,j),n,l,t,a,m,p,e) = (ModoJogo j,n,l,t,a,m,p,e)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa (Quit,j),n,l,t,a,m,p,e) = (Controlador Jogar,0,l,t,a,0,p,e)
reageEventoGloss _ (ModoJogo v@((Jogo j (Jogador (x,y) b d))), n,l,t,a,m,p,e) | elem (Porta,(x,y)) (desconstroiMapa j) && n == 1 = (VenceuNivel Next,1,l,t,a,m,p,e)
                                                                              | elem (Porta,(x,y)) (desconstroiMapa j) && n == 2 = (VenceuNivel Next,2,l,t,a,m,p,e)
                                                                              | elem (Porta,(x,y)) (desconstroiMapa j) && n == 3 = (VenceuNivel Next,3,l,t,a,m,p,e)
                                                                              | elem (Porta,(x,y)) (desconstroiMapa j) && n == 4 = (VenceuNivel Next,4,l,t,a,m,p,e)
                                                                              | elem (Porta,(x,y)) (desconstroiMapa j) && n == 5 = (VenceuNivel Next,5,l,t,a,m,p,e)
                                                                              | elem (Porta,(x,y)) (desconstroiMapa j) && n == 6 = (VenceuNivel Restart,6,l,t,a,m,p,e)
                                                                              | elem (Porta,(x,y)) (desconstroiMapa j) && n == 7 = (ModoJogo e,3,l,t,a,m,p,easteregg)
                                                                              | otherwise = (ModoJogo v,n,l,t,a,m,p,e) 
reageEventoGloss _ j = j 

reageTempoGloss:: Float -> Estado -> Estado
reageTempoGloss _ w = w

fr :: Int
fr = 50

dm :: Display
dm = FullScreen


main :: IO ()
main = do
  caixa <- loadBMP "caixaDefault.bmp"
  porta <- loadBMP "portadefault.bmp"
  bloco <- loadBMP "BlockDefault.bmp" 
  bonecoD <- loadBMP "jogadorDD.bmp"
  bonecoE <- loadBMP "jogadorDE.bmp"
  caixaM <- loadBMP "caixaMinecraft.bmp"
  portaM <- loadBMP "portaMinecraft.bmp"
  blocoM <- loadBMP "blocominecraft.bmp"
  bonecoMD <- loadBMP "steveright.bmp"
  bonecoME <- loadBMP "steve.bmp"
  caixaN <- loadBMP "caixaNatal.bmp"
  portaN <- loadBMP "portaNatal.bmp"
  blocoN <- loadBMP "blocoNatal.bmp"
  bonecoNE <- loadBMP "jogadorNatalE.bmp"
  bonecoND <- loadBMP "jogadorNatalD.bmp"
  caixaI <- loadBMP "caixaInferno.bmp"
  portaI <- loadBMP "portaInferno.bmp"
  blocoI <- loadBMP "blocoInferno.bmp"
  bonecoIE <- loadBMP "jogadorInfernoE.bmp"
  bonecoID <- loadBMP "jogadorInfernoD.bmp"
  estrela <- loadBMP "estrela.bmp"
  estrela <- loadBMP "estrela.bmp"
  play dm (greyN 0.8) fr (estadoGlossInicial [(1,([(Bloco,bloco),(Caixa,caixa),(Porta,porta)],(bonecoE,bonecoD)))
                                             ,(2,([(Bloco,blocoM),(Caixa,caixaM),(Porta,portaM)],(bonecoME,bonecoMD)))
                                             ,(3,([(Bloco,blocoN),(Caixa,caixaN),(Porta,portaN)],(bonecoNE,bonecoND))) 
                                             ,(4,([(Bloco,blocoI),(Caixa,caixaI),(Porta,portaI)],(bonecoIE,bonecoID)))] estrela) transformaJogo reageEventoGloss reageTempoGloss