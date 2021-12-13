module Fixtures where

import LI12122

m1 :: [(Peca, Coordenadas)]
m1 =
  [ (Porta, (0, 3)),
    (Bloco, (0, 4)),
    (Bloco, (1, 4)),
    (Bloco, (2, 4)),
    (Bloco, (3, 4)),
    (Bloco, (4, 4)),
    (Caixa, (4, 3)),
    (Bloco, (5, 4)),
    (Bloco, (6, 4)),
    (Bloco, (6, 3)),
    (Bloco, (6, 2)),
    (Bloco, (6, 1))
  ]

m1' :: [(Peca,Coordenadas)]
m1' = 
  [ (Porta, (0, 3)),
    (Bloco, (0, 4)),
    (Vazio, (1, 4)),
    (Bloco, (2, 4)),
    (Bloco, (3, 4)),
    (Bloco, (4, 4)),
    (Caixa, (4, 3)),
    (Bloco, (5, 4)),
    (Bloco, (6, 4)),
    (Bloco, (6, 3)),
    (Bloco, (6, 2)),
    (Bloco, (6, 1))
  ]

m2 :: [(Peca, Coordenadas)] 
m2 = 
  [ (Porta, (1,4)),
    (Bloco, (0,0)),
    (Bloco, (0,1)),
    (Bloco, (0,2)),
    (Bloco, (0,3)),
    (Bloco, (0,4)),
    (Bloco, (0,5)),
    (Bloco, (1,5)),
    (Bloco, (2,5)),
    (Bloco, (3,5)),
    (Bloco, (4,5)),
    (Bloco, (5,5)),
    (Bloco, (6,5)),
    (Bloco, (7,5)), 
    (Bloco, (8,5)),
    (Bloco, (9,5)),
    (Bloco, (10,5)),
    (Bloco, (11,5)),
    (Bloco, (12,5)),
    (Vazio, (13,5)),           --Buraco no chão do mapa
    (Bloco, (14,5)),
    (Bloco, (15,5)),
    (Bloco, (16,5)),
    (Bloco, (17,5)),
    (Bloco, (18,5)),
    (Bloco, (19,5)),
    (Bloco, (3,4)),
    (Bloco, (3,3)),
    (Bloco, (8,4)),
    (Caixa, (10,4)),
    (Caixa, (14,4)) 
  ]
m3 :: [(Peca, Coordenadas)]
m3 = [(Porta, (0,2)), 
      (Bloco, (0,3)),
      (Bloco, (1,3)),
      (Bloco, (2,3)),
      (Bloco, (3,3)),
      (Caixa, (4,2)),
      (Bloco, (4,3)),
      (Bloco, (5,3)),
      (Bloco, (6,0)),
      (Bloco, (6,1)),
      (Bloco, (6,2)),
      (Bloco, (6,3))
    ]
m3' :: [(Peca,Coordenadas)]
m3' = [(Porta, (0,2)), 
      (Bloco, (0,3)),
      (Bloco, (1,3)),
      (Bloco, (2,3)),
      (Bloco, (3,3)),
      (Caixa, (4,1)),
      (Bloco, (4,3)),
      (Bloco, (5,3)),
      (Bloco, (6,0)),
      (Bloco, (6,1)),
      (Bloco, (6,2)),
      (Bloco, (6,3))
    ]
m4 :: [(Peca, Coordenadas)] 
m4 = 
  [ (Porta, (1,4)),
    (Bloco, (0,0)),
    (Bloco, (0,1)),
    (Bloco, (0,2)),
    (Bloco, (0,3)),
    (Bloco, (0,4)),
    (Bloco, (0,5)),
    (Bloco, (1,5)),
    (Bloco, (2,5)),
    (Bloco, (3,5)),
    (Bloco, (3,2)),
    (Bloco, (4,5)),
    (Bloco, (5,5)),
    (Bloco, (6,6)),
    (Bloco, (7,5)), 
    (Bloco, (8,5)),
    (Bloco, (9,5)),
    (Bloco, (10,5)),
    (Bloco, (11,5)),
    (Bloco, (12,5)),
    (Bloco, (13,5)),           
    (Bloco, (14,5)),
    (Bloco, (15,5)),
    (Bloco, (16,5)),
    (Bloco, (17,5)),
    (Bloco, (18,5)),
    (Bloco, (19,5)),
    (Bloco, (3,4)),
    (Bloco, (3,3)),
    (Bloco, (8,4)),
    (Caixa, (10,4)),
    (Bloco, (12,4)),
    (Bloco, (12,3)),
    (Caixa, (14,4)),
    (Bloco, (5,6)),
    (Bloco, (7,6)),
    (Caixa, (4,4))
  ]
m5 :: [(Peca,Coordenadas)]
m5 = 
  [ (Bloco,(0,0)),
    (Porta,(0,1)),
    (Bloco,(1,0)),
    (Bloco,(1,1)),
    (Bloco,(0,2)),
    (Bloco,(1,2)),
    (Bloco,(2,2)),
    (Bloco,(2,1)),
    (Bloco,(2,0))
  ]


m1r :: Mapa
m1r =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m2r :: Mapa 
m2r =
  [ [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
    [Bloco,Vazio,Vazio,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
    [Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
    [Bloco,Porta,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Caixa,Vazio,Bloco,Vazio,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio],
    [Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco],
    [Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
  ]
m2r' :: Mapa 
m2r' = 
  [ [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
    [Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
    [Bloco,Porta,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Caixa,Vazio,Bloco,Vazio,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio],
    [Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco],
    [Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
  ] 
m3r :: Mapa 
m3r = 
  [ [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
    [Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
    [Bloco,Porta,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Caixa,Vazio,Vazio,Vazio,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio],
    [Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]
  ]

m1e1 :: Jogo
m1e1 = Jogo m1r (Jogador (6, 0) Oeste False)

m1e2 :: Jogo
m1e2 = Jogo m1r (Jogador (2, 3) Oeste False)

m1e3 :: Jogo
m1e3 = Jogo m2r (Jogador (2, 2) Este False)

m1e4 :: Jogo
m1e4 = Jogo m2r (Jogador (17,4) Este False)

m1e5 :: Jogo 
m1e5 = Jogo m2r' (Jogador (17,4) Este False)