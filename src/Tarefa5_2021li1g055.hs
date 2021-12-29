{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
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

data Menu = Controlador Opcoes
          | Pausa (Opcoes2, Jogo) 
          | Nivel Niveis
          | ModoJogo Jogo
          | VenceuNivel Opcoes2
          | Tema Theme

type Estado = (Menu,Int,[(Int,([(Peca,Picture)],(Picture,Picture)))],([(Peca,Picture)],(Picture,Picture)),Int,Picture)

transformaJogo :: Estado -> Picture
transformaJogo (Controlador Jogar,_,_,_,_,_) = Pictures [Color blue $ Translate 0 70 $ drawOption "Jogar",drawOption "Temas", Translate (0) (-70) $ drawOption "Sair"]
transformaJogo (Controlador Temas,_,_,_,_,_) =  Pictures [Translate 0 70 $ drawOption "Jogar",Color blue $ drawOption "Temas", Translate (0) (-70) $ drawOption "Sair"]
transformaJogo (Controlador Sair,_,_,_,_,_) = Pictures [Translate 0 70 $ drawOption "Jogar",drawOption "Temas",color blue $ Translate (0) (-70) $ drawOption "Sair"]

transformaJogo (Tema Default,_,_,_,_,_) = Pictures [Color blue $ Translate 0 105 $ drawOption4 "Default", Translate 0 35 $ drawOption4 "Minecraft", Translate 0 (-35) $ drawOption4 "Natal", Translate 0 (-105) $ drawOption4 "Inferno"]
transformaJogo (Tema Minecraft,_,_,_,_,_) = Pictures [Translate 0 105 $ drawOption4 "Default", Color blue $ Translate 0 35 $ drawOption4 "Minecraft", Translate 0 (-35) $ drawOption4 "Natal", Translate 0 (-105) $ drawOption4 "Inferno"]
transformaJogo (Tema Natal,_,_,_,_,_) = Pictures [Translate 0 105 $ drawOption4 "Default", Translate 0 35 $ drawOption4 "Minecraft", Color blue $ Translate 0 (-35) $ drawOption4 "Natal", Translate 0 (-105) $ drawOption4 "Inferno"]
transformaJogo (Tema Inferno,_,_,_,_,_) = Pictures [Translate 0 105 $ drawOption4 "Default", Translate 0 35 $ drawOption4 "Minecraft", Translate 0 (-35) $ drawOption4 "Natal",Color blue $  Translate 0 (-105) $ drawOption4 "Inferno"]

transformaJogo (Nivel Nivel1,_,_,_,_,_) = Pictures [Color blue $ drawOption "Nivel 1", Translate (0) (-70) $ drawOption "Nivel 2"]
transformaJogo (Nivel Nivel2,_,_,_,_,_) = Pictures [drawOption "Nivel 1",color blue $ Translate (0) (-70) $ drawOption "Nivel 2"]
transformaJogo (VenceuNivel Next,1,l,_,m,p) |m <= 9 = Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed", color blue $ Translate (-267.5) (0) $ drawOption2 "Next Level", Translate (35) (0) $ drawOption2 "Restart Level", Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-160) 0 $ Scale (1.5) (1.5) $ p,translate (0) 0 $ Scale (1.5) (1.5) $ p,translate (160) 0 $ Scale (1.5) (1.5) $ p]
                                        |m <= 15 =  Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed", color blue $ Translate (-267.5) (0) $ drawOption2 "Next Level", Translate (35) (0) $ drawOption2 "Restart Level", Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-65) 0 $ Scale (1.5) (1.5) $ p,translate 65 0 $ Scale (1.5) (1.5) $ p]
                                        |otherwise =  Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed", color blue $ Translate (-267.5) (0) $ drawOption2 "Next Level", Translate (35) (0) $ drawOption2 "Restart Level", Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-3) 0 $ Scale (1.5) (1.5) $ p]

transformaJogo (VenceuNivel Restart ,1,_,_,m,p) | m <= 9 = Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed",Translate (-267.5) (0) $ drawOption2 "Next Level",color blue $  Translate (35) (0) $ drawOption2 "Restart Level", Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-160) 0 $ Scale (1.5) (1.5) $ p,translate (0) 0 $ Scale (1.5) (1.5) $ p,translate (160) 0 $ Scale (1.5) (1.5) $ p]
                                            | m <= 15 = Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed",Translate (-267.5) (0) $ drawOption2 "Next Level",color blue $  Translate (35) (0) $ drawOption2 "Restart Level", Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450, translate (-65) 0 $ Scale (1.5) (1.5) $ p,translate 65 0 $ Scale (1.5) (1.5) $ p]
                                            | otherwise = Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed",Translate (-267.5) (0) $ drawOption2 "Next Level",color blue $  Translate (35) (0) $ drawOption2 "Restart Level", Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450, translate (-3) 0 $ Scale (1.5) (1.5) $ p]
transformaJogo (VenceuNivel Quit,1,_,_,m,p) | m <= 9 =  Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed", Translate (-267.5) (0) $ drawOption2 "Next Level",Translate (35) (0) $ drawOption2 "Restart Level", color blue $  Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-160) 0 $ Scale (1.5) (1.5) $ p,translate (0) 0 $ Scale (1.5) (1.5) $ p,translate (160) 0 $ Scale (1.5) (1.5) $ p]
                                        | m <= 15 =  Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed", Translate (-267.5) (0) $ drawOption2 "Next Level",Translate (35) (0) $ drawOption2 "Restart Level", color blue $  Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-65) 0 $ Scale (1.5) (1.5) $ p,translate 65 0 $ Scale (1.5) (1.5) $ p]
                                        | otherwise =  Pictures [Translate (-250) (150)$ Scale (0.5) (0.5) $ Text "Level Completed", Translate (-267.5) (0) $ drawOption2 "Next Level",Translate (35) (0) $ drawOption2 "Restart Level", color blue $  Translate (400) (0) $ drawOption2 "Quit", rectangleWire 815 450,translate (-3) 0 $ Scale (1.5) (1.5) $ p]
transformaJogo (VenceuNivel Next,2,_,_,m,p) = Translate (-200) 0 $ Color red $ Text "Ganhou"
--
transformaJogo (Pausa (Resume,j),_,_,_,_,_) = Pictures [rectangleWire 650 150, color blue $ Translate (-147.5) (-12) $ drawOption3 "Resume", Translate (250) (-12) $ drawOption3 "Quit"]
transformaJogo (Pausa (Quit,j),_,_,_,_,_) = Pictures [rectangleWire 650 150, Translate (-147.5) (-12) $ drawOption3 "Resume",color blue $ Translate (250) (-12) $ drawOption3 "Quit"]

--
transformaJogo (ModoJogo ((Jogo j (Jogador (x,y) b d))),_,l,t,_,_) = translate adjustx adjusty (pictures $ transformaJogoAux (desconstroiMapa j) (Jogador (x,y) b d) t) 
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

drawOption option = Translate (-50) 0 $ Scale (0.5) (0.5) $ Text option
drawOption2 option = Translate (-100) (-190) $ Scale (0.3) (0.3) $ Text option
drawOption3 option = Translate (-100) (0) $ Scale (0.4) (0.4) $ Text option
drawOption4 option = Translate (-120) 0 $ Scale (0.5) (0.5) $ Text option

estadoGlossInicial :: [(Int,([(Peca,Picture)],(Picture,Picture)))]-> Picture -> Estado 
estadoGlossInicial l p = (Controlador Jogar,0,l,fromJust (lookup 1 l),0,p)

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

desenhaEstadoGloss :: Estado -> Picture
desenhaEstadoGloss = transformaJogo

reageEventoGloss :: Event -> Estado -> Estado
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Jogar,0,l,t,0,p) = (Nivel Nivel1,0,l,t,0,p) --
reageEventoGloss  (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Jogar,0,l,t,0,p) = (Controlador Sair,0,l,t,0,p)
reageEventoGloss   (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Jogar,0,l,t,0,p) = (Controlador Temas,0,l,t,0,p)
reageEventoGloss  (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Sair,0,l,t,0,p) =(Controlador Temas,0,l,t,0,p)
reageEventoGloss   (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Sair,0,l,t,0,p) = (Controlador Jogar,0,l,t,0,p)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Temas,0,l,t,0,p) = (Controlador Sair,0,l,t,0,p)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Temas,0,l,t,0,p) = (Controlador Jogar,0,l,t,0,p)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Temas,0,l,t,0,p) = (Tema Default,0,l,t,0,p)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Sair,0,l,t,0,p) = undefined

reageEventoGloss  (EventKey (SpecialKey KeyUp) Down _ _) (Nivel Nivel1,0,l,t,0,p) =( Nivel Nivel2 ,0,l,t,0,p)
reageEventoGloss   (EventKey (SpecialKey KeyDown) Down _ _) (Nivel Nivel1,0,l,t,0,p) = (Nivel Nivel2,0,l,t,0,p)
reageEventoGloss  (EventKey (SpecialKey KeyUp) Down _ _) (Nivel Nivel2,0,l,t,0,p) =( Nivel Nivel1,0,l,t,0,p)
reageEventoGloss   (EventKey (SpecialKey KeyDown) Down _ _) (Nivel Nivel2,0,l,t,0,p) = (Nivel Nivel1,0,l,t,0,p)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Nivel Nivel1,0,l,t,0,p) =(ModoJogo nivel1,1,l,t,0,p)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Nivel Nivel2,0,l,t,0,p) = (ModoJogo nivel2,2,l,t,0,p)
reageEventoGloss (EventKey (SpecialKey KeyDelete) Down _ _) (Nivel _,0,l,t,0,p) = (Controlador Jogar,0,l,t,0,p)

reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (Tema Default,0,l,t,0,p) = (Tema Minecraft,0,l,t,0,p)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (Tema Minecraft,0,l,t,0,p) = (Tema Natal,0,l,t,0,p)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (Tema Natal,0,l,t,0,p) = (Tema Inferno,0,l,t,0,p)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (Tema Inferno,0,l,t,0,p) = (Tema Default,0,l,t,0,p)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (Tema Default,0,l,t,0,p) = (Tema Inferno,0,l,t,0,p)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (Tema Minecraft,0,l,t,0,p) = (Tema Default,0,l,t,0,p)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (Tema Natal,0,l,t,0,p) = (Tema Minecraft,0,l,t,0,p)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (Tema Inferno,0,l,t,0,p) = (Tema Natal,0,l,t,0,p)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Tema Default,0,l,t,0,p) = (Tema Default,0,l,fromJust (lookup 1 l),0,p)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Tema Minecraft,0,l,t,0,p) = (Tema Minecraft,0,l,fromJust (lookup 2 l),0,p)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Tema Natal,0,l,t,0,p) = (Tema Natal,0,l,fromJust (lookup 3 l),0,p)
reageEventoGloss (EventKey (SpecialKey KeyDelete) Down _ _) (Tema _,0,l,t,0,p) = (Controlador Temas,0,l,t,0,p)

reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (VenceuNivel Next,1,l,t,n,p) = (VenceuNivel Restart,1,l,t,n,p)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (VenceuNivel Restart,1,l,t,n,p) = (VenceuNivel Quit,1,l,t,n,p)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (VenceuNivel Quit,1,l,t,n,p) = (VenceuNivel Next,1,l,t,n,p)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (VenceuNivel Next,1,l,t,n,p) = (VenceuNivel Quit,1,l,t,n,p)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (VenceuNivel Restart,1,l,t,n,p) = (VenceuNivel Next,1,l,t,n,p)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (VenceuNivel Quit,1,l,t,n,p) = (VenceuNivel Restart,1,l,t,n,p)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuNivel Next,1,l,t,n,p) = (ModoJogo nivel2,2,l,t,0,p)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuNivel Restart,1,l,t,n,p) = (ModoJogo nivel1,1,l,t,0,p)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuNivel Quit,1,l,t,n,p) = (Controlador Jogar,0,l,t,0,p)

reageEventoGloss  (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo j,n,l,t,m,p) = if j == moveJogador j Trepar then (ModoJogo (moveJogador j Trepar),n,l,t,m,p) else (ModoJogo (moveJogador j Trepar),n,l,t,m+1,p)
reageEventoGloss   (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo j,n,l,t,m,p) =if j == moveJogador j InterageCaixa then (ModoJogo (moveJogador j InterageCaixa),n,l,t,m,p) else (ModoJogo (moveJogador j InterageCaixa),n,l,t,m+1,p)
reageEventoGloss  (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo j,n,l,t,m,p) =if j == moveJogador j AndarDireita then (ModoJogo (moveJogador j AndarDireita),n,l,t,m,p) else (ModoJogo (moveJogador j AndarDireita),n,l,t,m+1,p)
reageEventoGloss  (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo j,n,l,t,m,p) =if j == moveJogador j AndarEsquerda then (ModoJogo (moveJogador j AndarEsquerda),n,l,t,m,p) else (ModoJogo (moveJogador j AndarEsquerda),n,l,t,m+1,p)

reageEventoGloss (EventKey (Char 'r') Down _ _) (ModoJogo j,n,l,t,m,p) | n == 1 = (ModoJogo nivel1,n,l,t,0,p) 
                                                                   | n == 2 = (ModoJogo nivel2,n,l,t,0,p)

reageEventoGloss (EventKey (Char 'p') Down _ _) (ModoJogo j,n,l,t,m,p) = (Pausa (Resume,j),n,l,t,m,p)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (Pausa (Resume,j),n,l,t,m,p) = (Pausa (Quit,j),n,l,t,m,p)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (Pausa (Quit,j),n,l,t,m,p) = (Pausa (Resume,j),n,l,t,m,p)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (Pausa (Resume,j),n,l,t,m,p) = (Pausa (Quit,j),n,l,t,m,p)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (Pausa (Quit,j),n,l,t,m,p) = (Pausa (Resume,j),n,l,t,m,p)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa (Resume,j),n,l,t,m,p) = (ModoJogo j,n,l,t,m,p)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa (Quit,j),n,l,t,m,p) = (Controlador Jogar,0,l,t,0,p)
reageEventoGloss _ (ModoJogo v@((Jogo j (Jogador (x,y) b d))), n,l,t,m,p) | elem (Porta,(x,y)) (desconstroiMapa j) && n == 1 = (VenceuNivel Next,1,l,t,m,p)
                                                                      | elem (Porta,(x,y)) (desconstroiMapa j) && n == 2 = (VenceuNivel Next,2,l,t,m,p)
                                                                      |otherwise = (ModoJogo v,n,l,t,m,p) 
reageEventoGloss _ j = j 

reageTempoGloss:: Float -> Estado -> Estado
reageTempoGloss _ w = w

fr :: Int
fr = 50

dm :: Display
dm = InWindow "Block Dude" (1000,1000) (0,0)


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
  estrela <- loadBMP "estrela.bmp"
  play dm (greyN 0.8) fr (estadoGlossInicial [(1,([(Bloco,bloco),(Caixa,caixa),(Porta,porta)],(bonecoE,bonecoD)))
                                             ,(2,([(Bloco,blocoM),(Caixa,caixaM),(Porta,portaM)],(bonecoME,bonecoMD)))
                                             ,(3,([(Bloco,blocoN),(Caixa,caixaN),(Porta,portaN)],(bonecoNE,bonecoND)))] estrela) desenhaEstadoGloss reageEventoGloss reageTempoGloss
