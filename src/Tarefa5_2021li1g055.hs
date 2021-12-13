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

data Niveis = Nivel1 
            | Nivel2

data Menu = Controlador Opcoes
          | Nivel Niveis
          | ModoJogo Jogo
          | VenceuJogo

type Estado = (Menu,Int)

transformaJogo :: Estado -> Picture
transformaJogo (Controlador Jogar,_) = Pictures [Color blue $ drawOption "Jogar", Translate (0) (-70) $ drawOption "Sair"]
transformaJogo (Controlador Sair,_) = Pictures [drawOption "Jogar",color blue $ Translate (0) (-70) $ drawOption "Sair"]
transformaJogo (Nivel Nivel1,_) = Pictures [Color blue $ drawOption "Nivel 1", Translate (0) (-70) $ drawOption "Nivel 2"]
transformaJogo (Nivel Nivel2,_) = Pictures [drawOption "Nivel 1",color blue $ Translate (0) (-70) $ drawOption "Nivel 2"]
transformaJogo (VenceuJogo,2) = Translate (-200) 0 $ Color red $ Text "Ganhou"
transformaJogo (ModoJogo (Jogo j (Jogador (x,y) b d)),_) = translate adjustx adjusty (pictures $ transformaJogoAux (desconstroiMapa j) (Jogador (x,y) b d))                                              
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
estadoGlossInicial = (Controlador Jogar,0)

nivel1 :: Jogo
nivel1 = 
  Jogo   [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
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

desenhaEstadoGloss :: Estado -> Picture
desenhaEstadoGloss = transformaJogo

reageEventoGloss :: Event -> Estado -> Estado
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Jogar,0) = (Nivel Nivel1,0)
reageEventoGloss  (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Jogar,0) = (Controlador Sair,0)
reageEventoGloss   (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Jogar,0) = (Controlador Sair,0)
reageEventoGloss  (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Sair,0) = (Controlador Jogar,0)
reageEventoGloss   (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Sair,0) = (Controlador Jogar,0)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Sair,0) = undefined

reageEventoGloss  (EventKey (SpecialKey KeyUp) Down _ _) (Nivel Nivel1,0) = (Nivel Nivel1,0)
reageEventoGloss   (EventKey (SpecialKey KeyDown) Down _ _) (Nivel Nivel1,0) = (Nivel Nivel2,0)
reageEventoGloss  (EventKey (SpecialKey KeyUp) Down _ _) (Nivel Nivel2,0) = (Nivel Nivel1,0)
reageEventoGloss   (EventKey (SpecialKey KeyDown) Down _ _) (Nivel Nivel2,0) = (Nivel Nivel1,0)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Nivel Nivel1,0) = (ModoJogo nivel1,1)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Nivel Nivel2,0) = (ModoJogo nivel2,2)

reageEventoGloss  (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo j,n) = (ModoJogo (moveJogador j Trepar),n)
reageEventoGloss  (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo j,n) = (ModoJogo (moveJogador j InterageCaixa),n)
reageEventoGloss  (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo j,n) =(ModoJogo (moveJogador j AndarDireita),n)
reageEventoGloss  (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo j,n) = (ModoJogo (moveJogador j AndarEsquerda),n)
reageEventoGloss (EventKey r Down _ _) (ModoJogo j,n) | n == 1 = (ModoJogo nivel1,n) 
                                                      | n == 2 = (ModoJogo nivel2,n)

reageEventoGloss _ (ModoJogo v@((Jogo j (Jogador (x,y) b d))), n) | elem (Porta,(x,y)) (desconstroiMapa j) && n == 1 = (ModoJogo nivel2,2)
                                                                  | elem (Porta,(x,y)) (desconstroiMapa j) && n == 2 = (VenceuJogo,2)
                                                                  |otherwise = (ModoJogo v,n)
reageEventoGloss _ j = j 

reageTempoGloss:: Float -> Estado -> Estado
reageTempoGloss _ w = w

fr :: Int
fr = 50

dm :: Display
dm = InWindow "BlocK Dude" (1000,1000) (0,0)


main :: IO ()
main = play dm (greyN 0.8) fr estadoGlossInicial desenhaEstadoGloss reageEventoGloss reageTempoGloss
