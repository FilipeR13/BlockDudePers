{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{- | 
Module      : Tarefa5_2021li1g055
Description : Aplicação Gráfica

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
-}
module Main where

import LI12122
import Tarefa4_2021li1g055
import Tarefa2_2021li1g055
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data Opcoes = Jogar 
            | Sair

data Menu = Controlador Opcoes
          | ModoJogo Jogo
          | VenceuJogo

type Estado = Menu

transformaJogo :: Estado -> Picture
transformaJogo (Controlador Jogar) = Pictures [Color blue $ drawOption "Jogar", Translate (0) (-70) $ drawOption "Sair"]
transformaJogo (Controlador Sair) = Pictures [drawOption "Jogar",color blue $ Translate (0) (-70) $ drawOption "Sair"]
transformaJogo (ModoJogo (Jogo j (Jogador (x,y) b d))) = translate adjustx adjusty (pictures $ transformaJogoAux (desconstroiMapa j) (Jogador (x,y) b d))                                              
                                              where adjusty = -32 * fromIntegral (ymax (desconstroiMapa j)) / 2 
                                                    adjustx = -32 * fromIntegral (xmax (desconstroiMapa j)) / 2
transformaJogoAux :: [(Peca,Coordenadas)] -> Jogador -> [Picture]
transformaJogoAux [] (Jogador (x,y) dir b) = case dir of Este -> if b then [translate z w jogadorE, translate z w' caixa] else [translate z w jogadorE]
                                                         Oeste -> if b then [translate z w jogadorO, translate z w' caixa] else [translate z w jogadorO]      
      where  jogadorE = color yellow $ rectangleSolid 32 32
             jogadorO = rectangleSolid 32 32 
             caixa = color blue $ rectangleSolid 32 32
             z = 32 * fromIntegral x
             w = 32 * fromIntegral (-y)
             w' = 32 * fromIntegral (-(y-1))
transformaJogoAux m@((p,(a,b)):t) (Jogador (x,y) dir s)
                                 | p == Bloco = translate c d bloco : transformaJogoAux t (Jogador (x,y) dir s) 
                                 | p == Porta = translate c d porta : transformaJogoAux t (Jogador (x,y) dir s) 
                                 | p == Caixa = translate c d caixa : transformaJogoAux t (Jogador (x,y) dir s) 
                                 | otherwise = transformaJogoAux t (Jogador (x,y) dir s) 
   where bloco = color red $ rectangleSolid 32 32
         porta = color green $ rectangleSolid 32 32
         caixa = color blue $ rectangleSolid 32 32
         c = 32 * fromIntegral a
         d = 32 * fromIntegral (-b)

drawOption option = Translate (-50) 0 $ Scale (0.5) (0.5) $ Text option

estadoGlossInicial :: Estado 
estadoGlossInicial = Controlador Jogar

nivel1 :: Jogo
nivel1 = 
  Jogo   [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Bloco, Vazio, Vazio, Caixa, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ] (Jogador (6, 0) Oeste False)


desenhaEstadoGloss :: Estado -> Picture
desenhaEstadoGloss = transformaJogo

reageEventoGloss :: Event -> Estado -> Estado
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Jogar) = ModoJogo nivel1 
reageEventoGloss  (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Jogar) =Controlador Sair
reageEventoGloss   (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Jogar) = Controlador Sair
reageEventoGloss  (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Sair) =Controlador Jogar
reageEventoGloss   (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Sair) = Controlador Jogar
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Sair) = undefined

reageEventoGloss  (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo j) =ModoJogo (moveJogador j Trepar)
reageEventoGloss  (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo j) =ModoJogo (moveJogador j InterageCaixa)
reageEventoGloss  (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo j) =ModoJogo (moveJogador j AndarDireita)
reageEventoGloss  (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo j) =ModoJogo (moveJogador j AndarEsquerda)
reageEventoGloss _ j = j 

reageTempoGloss:: Float -> Estado -> Estado
reageTempoGloss _ w = w

fr :: Int
fr = 50

dm :: Display
dm = InWindow "BlocK Dude" (1000,1000) (0,0)


main :: IO ()
main = play dm (greyN 0.8) fr estadoGlossInicial desenhaEstadoGloss reageEventoGloss reageTempoGloss
