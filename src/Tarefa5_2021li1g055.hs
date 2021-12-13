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



type Estado =Jogo

transformaJogo :: Estado -> Picture
transformaJogo ((Jogo j (Jogador (x,y) b d))) = translate adjustx adjusty (pictures $ transformaJogoAux (desconstroiMapa j) (Jogador (x,y) b d)) 
                                              where adjusty = -32 * fromIntegral (ymax (desconstroiMapa j)) / 2 
                                                    adjustx = -32 * fromIntegral (xmax (desconstroiMapa j)) / 2
transformaJogoAux :: [(Peca,Coordenadas)] -> Jogador -> [Picture]
transformaJogoAux [] (Jogador (x,y) dir b) = [translate z w jogador]
      where  jogador = rectangleSolid 32 32 
             z = 32 * fromIntegral x
             w = 32 * fromIntegral (-y)
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

-- "Estados"GlossInicial
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
reageEventoGloss _ j = j 

reageTempoGloss:: Float -> Estado -> Estado
reageTempoGloss _ w = w

fr :: Int
fr = 50

dm :: Display
dm = InWindow "BlocK Dude" (1000,1000) (0,0)


main :: IO ()
main = play dm (greyN 0.8) fr nivel1 desenhaEstadoGloss reageEventoGloss reageTempoGloss
