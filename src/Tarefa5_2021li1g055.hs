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
import Data.Maybe

data Opcoes = Jogar 
            | Sair

data Niveis = Nivel1 
            | Nivel2

data Menu = Controlador Opcoes
          | Nivel Niveis
          | ModoJogo Jogo
          | VenceuJogo

type Estado = (Menu,Int,([(Peca,Picture)],(Picture,Picture)))

transformaJogo :: Estado -> Picture
transformaJogo (Controlador Jogar,_,_) = Pictures [Color blue $ drawOption "Jogar", Translate (0) (-70) $ drawOption "Sair"]
transformaJogo (Controlador Sair,_,_) = Pictures [drawOption "Jogar",color blue $ Translate (0) (-70) $ drawOption "Sair"]
transformaJogo (Nivel Nivel1,_,_) = Pictures [Color blue $ drawOption "Nivel 1", Translate (0) (-70) $ drawOption "Nivel 2"]
transformaJogo (Nivel Nivel2,_,_) = Pictures [drawOption "Nivel 1",color blue $ Translate (0) (-70) $ drawOption "Nivel 2"]
transformaJogo (VenceuJogo,2,_) = Translate (-200) 0 $ Color red $ Text "Ganhou"
transformaJogo (ModoJogo (Jogo j (Jogador (x,y) b d)),_,l) = translate adjustx adjusty (pictures $ transformaJogoAux (desconstroiMapa j) (Jogador (x,y) b d) l)                                              
                                              where adjusty = -32 * fromIntegral (ymax (desconstroiMapa j)) / 2 
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

drawOption option = Translate (-50) 0 $ Scale (0.5) (0.5) $ Text option

estadoGlossInicial :: ([(Peca,Picture)],(Picture,Picture)) -> Estado 
estadoGlossInicial l = (Controlador Jogar,0,l)

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
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Jogar,0,l) = (Nivel Nivel1,0,l) --
reageEventoGloss  (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Jogar,0,l) =( Controlador Sair,0,l)
reageEventoGloss   (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Jogar,0,l) = (Controlador Sair,0,l)
reageEventoGloss  (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Sair,0,l) =(Controlador Jogar,0,l)
reageEventoGloss   (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Sair,0,l) = (Controlador Jogar,0,l)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Sair,0,l) = undefined

reageEventoGloss  (EventKey (SpecialKey KeyUp) Down _ _) (Nivel Nivel1,0,l) =( Nivel Nivel2 ,0,l)
reageEventoGloss   (EventKey (SpecialKey KeyDown) Down _ _) (Nivel Nivel1,0,l) = (Nivel Nivel2,0,l)
reageEventoGloss  (EventKey (SpecialKey KeyUp) Down _ _) (Nivel Nivel2,0,l) =( Nivel Nivel1,0,l)
reageEventoGloss   (EventKey (SpecialKey KeyDown) Down _ _) (Nivel Nivel2,0,l) = (Nivel Nivel1,0,l)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Nivel Nivel1,0,l) =(ModoJogo nivel1,1,l)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Nivel Nivel2,0,l) = (ModoJogo nivel2,2,l)

reageEventoGloss  (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo j,n,l) = (ModoJogo (moveJogador j Trepar),n,l)
reageEventoGloss   (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo j,n,l) =( ModoJogo (moveJogador j InterageCaixa),n,l)
reageEventoGloss  (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo j,n,l) = (ModoJogo (moveJogador j AndarDireita),n,l)
reageEventoGloss  (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo j,n,l) =(ModoJogo (moveJogador j AndarEsquerda),n,l)
--
reageEventoGloss (EventKey (Char 'r') Down _ _) (ModoJogo j,n,l) | n == 1 = (ModoJogo nivel1,n,l) 
                                                        | n == 2 = (ModoJogo nivel2,n,l)

reageEventoGloss _ (ModoJogo v@((Jogo j (Jogador (x,y) b d))), n,l) | elem (Porta,(x,y)) (desconstroiMapa j) && n == 1 = (ModoJogo nivel2,2,l)
                                                                  | elem (Porta,(x,y)) (desconstroiMapa j) && n == 2 = (VenceuJogo,2,l)
                                                                  |otherwise = (ModoJogo v,n,l)
reageEventoGloss _ j = j 

reageTempoGloss:: Float -> Estado -> Estado
reageTempoGloss _ w = w

fr :: Int
fr = 50

dm :: Display
dm = InWindow "BlocK Dude" (1000,1000) (0,0)


main :: IO ()
main = do
  caixa <- loadBMP "caixaDefault.bmp"
  porta <- loadBMP "portadefault.bmp"
  bloco <- loadBMP "BlockDefault.bmp" 
  bonecoD <- loadBMP "jogadorDD.bmp"
  bonecoE <- loadBMP "jogadorDE.bmp"
  play dm (greyN 0.8) fr (estadoGlossInicial ([(Bloco,bloco),(Caixa,caixa),(Porta,porta)],(bonecoE,bonecoD))) desenhaEstadoGloss reageEventoGloss reageTempoGloss
