{- |
Module      : Tarefa4_2021li1g055
Description : Movimentação do personagem
Copyright   : Lucas Quintela <a100642@alunos.uminho.pt>;
            : José Rodrigues <a100692@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
-}
module Tarefa4_2021li1g055 where

import LI12122
import Tarefa2_2021li1g055
import Tarefa3_2021li1g055

{-|
== Move Jogador

A função 'moveJogador' é a função que recebe o jogo e o movimento. 

* AndarEsquerda: A função verifica, com recurso às funções 'movimentovalido' e 'movimentoesquerdaBloco' (caso o jogador esteja a carregar um bloco), se o jogador pode avançar para a esquerda.
Se puder andar então vai devolver o mapa com o jogador na sua nova posição, calculada pela função 'verificaposicaoesquerda'.

* AndarDireita: A função verifica, com recurso às funções 'movimentovalido' e 'movimentodireitaBloco' (caso o jogador esteja a carregar um bloco), se o jogador pode avançar para a direita.
Se puder andar então vai devolver o mapa com o jogador na sua nova posição, calculada pela função 'verificaposicaodireita'.

* Trepar: A função começa por verificar se o jogador pode trepar um bloco, testando se este se encontra na ordenada igual a 0 ou se está na ordenada igual a 1 se estiver a carregar um bloco, pois
estando numa coordenada assim, o jogador não tem como subir nenhum bloco. Para além disso, se o jogador estiver nos limites laterais do mapa e ao mesmo tempo estiver virado para esse mesmo limite,
a função devolve o mapa sem o modificar pois o jogador não pode trepar para nenhum lado. Passado estes casos a função verifica se não há blocos em cima jogador e se existe um bloco/caixa em que este
possa subir com recurso às funções 'subir' e 'subirBloco', completando assim o movimento e devolvendo o mapa com o jogador na nova posição. 

* InterageCaixa: A função primeiro verifica existem caixas à volta do jogador que este possa pegar. De seguida a função divide-se em dois casos. Quando o jogador não está a carregar uma caixa, então
vai usar a função 'apanhacaixa' para verificar se pode pegar na caixa. Quando o jogador está a carregar uma caixa, a função irá usar a função auxiliar 'pousaCaixa' para averiguar se pode pousar a caixa.
-}

moveJogador :: Jogo -> Movimento -> Jogo
moveJogador (Jogo l (Jogador (x,y) d b)) AndarEsquerda
        | movimentovalido l (x-1,y) && d == Oeste && b == False = Jogo l (Jogador (verificaposicaoesquerda l (x,y)) Oeste b)
        | movimentovalido l (x-1,y) && d == Este && b == False = Jogo l (Jogador (verificaposicaoesquerda l (x,y)) Oeste b)
        | movimentoesquerdaBloco l (x,y) && d == Oeste && b == True = Jogo l (Jogador (verificaposicaoesquerda l (x,y)) Oeste b)
        | movimentoesquerdaBloco l (x,y) && d == Este && b == True = Jogo l (Jogador (verificaposicaoesquerda l (x,y)) Oeste b)
        | otherwise = Jogo l (Jogador (x,y) Oeste b)
moveJogador  (Jogo l (Jogador (x,y) d b)) AndarDireita
        | movimentovalido l (x+1,y) && d == Oeste && b == False = Jogo l (Jogador (verificaposicaodireita l (x,y)) Este b)
        | movimentovalido l (x+1,y) && d == Este && b == False = Jogo l (Jogador (verificaposicaodireita l (x,y)) Este b)
        | movimentodireitaBloco l (x,y) && d == Oeste && b == True = Jogo l (Jogador (verificaposicaodireita l (x,y)) Este b)
        | movimentodireitaBloco l (x,y) && d == Este && b == True = Jogo l (Jogador (verificaposicaodireita l (x,y)) Este b)
        | otherwise = Jogo l (Jogador (x,y) Este b)
moveJogador (Jogo l (Jogador (x,y) d b)) Trepar
        | y == 0 = Jogo l (Jogador (x,y) d b)
        | y == 1 && b == True = Jogo l (Jogador (x,y) d b) 
        | elem (Bloco,(x,y-1)) (desconstroiMapa l) = Jogo l (Jogador (x,y) d b) 
        | (d == Oeste && x == 0) || (d == Este && x == xmax (desconstroiMapa l)) = Jogo l (Jogador (x,y) d b)
        | b == False = case d of Oeste -> if subir l (x-1,y) then Jogo l (Jogador (x-1,y-1) d b) else Jogo l (Jogador (x,y) d b)
                                 Este -> if subir l (x+1,y) then Jogo l (Jogador (x+1,y-1) d b) else Jogo l (Jogador (x,y) d b)
        | b == True = case d of Oeste -> if subirBloco l (x-1,y) then Jogo l (Jogador (x-1,y-1) d b) else Jogo l (Jogador (x,y) d b)
                                Este -> if subirBloco l (x+1,y) then Jogo l (Jogador (x+1,y-1) d b) else Jogo l (Jogador (x,y) d b)
moveJogador (Jogo l (Jogador (x,y) d b)) InterageCaixa
        | b == False && d == Este && notElem (Caixa,(x+1,y)) m = Jogo l (Jogador (x,y) d b)
        | b == False && d == Oeste && notElem (Caixa,(x-1,y)) m = Jogo l (Jogador (x,y) d b)
        | otherwise = case b of False -> apanhacaixa l (x,y) d
                                True -> pousaCaixa l (x,y) d
        where m = desconstroiMapa l

{-|
A função 'movimentovalido' começa por verificar se o jogador se encontra a tentar mover-se para fora dos limites do mapa laterais, pelo que devolve False. De seguida verifica se existe uma Caixa ou
um bloco na posição para a qual o jogador quer ir, se existir devolve False. Se nenhum dos ultimos casos se verificar, a função irá devolver True. 
-}
movimentovalido :: Mapa -> Coordenadas -> Bool
movimentovalido l (x,y)
        | x+1 == 0 || x-1 == xmax (desconstroiMapa l) = False
        | elem (Caixa, (x,y)) (desconstroiMapa l) = False
        | elem (Bloco ,(x,y)) (desconstroiMapa l) = False
        |otherwise = True
{-|
A função 'verificaposicaoesquerda' calcula a coordenada para a qual o jogador se vai deslocar, verificando o chão à sua frente. Se não existir chão imediatamente à frete do jogador, a função
irá ser chamada recursivamente com a ordenada do jogador com mais unidade, procurando assim o chão em que o jogador irá ficar. Quando encontrar o chão, vai devolver a posição do jogador.
-}
verificaposicaoesquerda :: Mapa -> Coordenadas -> Coordenadas
verificaposicaoesquerda l (x,y) | x == 0 = (x,y)
                                | elem (Caixa, (x-1,y+1)) (desconstroiMapa l) || elem (Bloco, (x-1,y+1)) (desconstroiMapa l) = (x-1,y)
                                | otherwise = verificaposicaoesquerda l (x,y+1)
{-|
Esta função 'movimentoesquerdaBloco' é usada para verificar se o jogador pode avançar mesmo estando a carregar um bloco. Essencialmente, esta função verifica as mesmas coisas que a função 'movimentovalido'
com a adição da condição de que não deve existir um bloco nas coordenadas (x-1,y-1), sendo (x,y) as coordenadas do jogador, pois assim a caixa assim não poderá passar.
-}
movimentoesquerdaBloco :: Mapa -> Coordenadas -> Bool
movimentoesquerdaBloco l (x,y) 
        | x == 0 = False
        | elem (Caixa, (x-1,y)) (desconstroiMapa l) = False
        | elem (Bloco ,(x-1,y)) (desconstroiMapa l) = False
        | elem (Bloco , (x-1,y-1)) (desconstroiMapa l) = False
        |otherwise = True
{-
A função 'verificaposicaodireita' calcula a coordenada para a qual o jogador se vai deslocar, verificando o chão à sua frente. Se não existir chão imediatamente à frete do jogador, a função
irá ser chamada recursivamente com a ordenada do jogador com mais unidade, procurando assim o chão em que o jogador irá ficar. Quando encontrar o chão, vai devolver a posição do jogador.
-}
verificaposicaodireita :: Mapa -> Coordenadas -> Coordenadas
verificaposicaodireita l (x,y) | elem (Caixa, (x+1,y+1)) (desconstroiMapa l) || elem (Bloco, (x+1,y+1)) (desconstroiMapa l) = (x+1,y)
                               | otherwise = verificaposicaodireita l (x,y+1)
{-|
Esta função 'movimentodireitaBloco' é usada para verificar se o jogador pode avançar mesmo estando a carregar um bloco. Essencialmente, esta função verifica as mesmas coisas que a função 'movimentovalido'
com a adição da condição de que não deve existir um bloco nas coordenadas (x+1,y-1), sendo (x,y) as coordenadas do jogador, pois assim a caixa assim não poderá passar.
-}
movimentodireitaBloco :: Mapa -> Coordenadas -> Bool
movimentodireitaBloco l (x,y) 
        | x == xmax (desconstroiMapa l) = False
        | elem (Caixa, (x+1,y)) (desconstroiMapa l) = False
        | elem (Bloco ,(x+1,y)) (desconstroiMapa l) = False
        | elem (Bloco , (x+1,y-1)) (desconstroiMapa l) = False
        |otherwise = True

{-|
A função 'subir' tem como objetivo verificar se o jogador tem algum bloco ou caixa que possa trepar. Para além disso, este também verifica se tem algum bloco em cima do que este quer subir, pois se 
existir o  jogador não poderá subir.
-}
subir :: Mapa -> Coordenadas -> Bool
subir l (x,y) | elem (Caixa, (x,y-1)) m || elem (Bloco, (x,y-1)) m = False
              | elem (Caixa, (x,y)) m || elem (Bloco, (x,y)) m = True
              | otherwise = False
              where m = desconstroiMapa l
{-|
A função 'subirBloco' tem praticamente o mesmo objetivo que a função anterior ('subir') com a adição da verificação se existe algum bloco no local onde a caixa irá ficar depois de o jogador trepar
pois, se existir,  o jogador não poderá trepar.
-}
subirBloco :: Mapa -> Coordenadas -> Bool
subirBloco l (x,y)
        |elem (Caixa, (x,y-1)) m || elem (Bloco, (x,y-1)) m || elem (Bloco, (x,y-2)) m = False
        |elem (Caixa, (x,y)) m || elem (Bloco, (x,y)) m = True
        |otherwise = False
        where m = desconstroiMapa l
{-|
A função 'apanhacaixa' verifica se existe algum bloco em cima do jogador e se também existe uma caixa ou bloco em cima da caixa que se pretende pegar, pois nestas situações o jogador não poderá
pegar na caixa. De seguida, após verificar que existe uma caixa na direção que o jogador está virado, a função irá devolver o jogo retirando a caixa que foi carregada do mapa (com ajuda da função
'substituicaixa') e passando o booleano do jogador a True. 
-}

apanhacaixa :: Mapa -> Coordenadas -> Direcao -> Jogo
apanhacaixa l (x,y) d | (x == 0 && d == Oeste) || (x == xmax m && d == Este) =  Jogo l (Jogador (x,y) d False)
                      | d == Oeste && (elem (Caixa,(x-1,y-1)) m || elem (Bloco,(x-1,y-1)) m || elem (Bloco,(x,y-1)) m )= Jogo l (Jogador (x,y) d False)
                      | d == Este && (elem (Caixa,(x+1,y-1)) m || elem (Bloco,(x+1,y-1)) m || elem (Bloco,(x,y-1)) m )= Jogo l (Jogador (x,y) d False)
                      | d == Oeste && elem (Caixa,(x-1,y)) m = Jogo (constroiMapa (substituicaixa m (Caixa,(x-1,y)))) (Jogador (x,y) Oeste True)
                      | d == Este && elem (Caixa,(x+1,y)) m = Jogo (constroiMapa (substituicaixa m (Caixa,(x+1,y)))) (Jogador (x,y) Este True)
                      | otherwise = Jogo l (Jogador (x,y) d False)
        where m = desconstroiMapa l

{-|
A função 'substituicaixa' tem como objetivo retirar a caixa que foi carregada pelo jogador do mapa. Para isso ela vai comparando as coordenadas da caixa na lista de peças e coordenadas do mapa. Quando
encontrar as coordenadas a função devolve o mapa sem a caixa, tirando-a da lista, ficando assim declarado um Vazio por omissão.
-}
substituicaixa :: [(Peca,Coordenadas)] -> (Peca,Coordenadas) -> [(Peca, Coordenadas)]
substituicaixa [] _ = []
substituicaixa ((p1,(x1,y1)):ms) (p,(x,y)) | p1 == p && x1== x && y1 == y= ms
                                           | otherwise = (p1,(x1,y1)) : substituicaixa ms (p,(x,y))

{-|
A função 'pousaCaixa' começa por verificar se as condições são favoráveis para largar a caixa. Começa por verificar se o jogador está virado para um limites laterais do mapa, pelo que não pode largar
a caixa. De seguida verifica se existe um bloco ou caixa em cima da posição para onde se pretende pousar a caixa e ainda se existe uma porta no sitio onde se quer pousar a caixa, se existir, 
a função devolve o jogo tal como o recebeu. Após passar pelas verificações anteriores, a função verifica se o jogador tem um bloco ou caixa à sua frente (para além de verificar se existe uma porta em 
cima desse possivel blovo/caixa), pois se existir a função irá colocar a caixa na posição acima desse bloco/caixa. Por fim, a função procura o chão onde a caixa vai ficar, devolvendo a caixa na posição 
que corresponde à posição em cima desse chão, para isso a função precisa de uma função auxiliar 'pousaCaixaAux', de salientar que antes de colocar a caixa, a função verifica se existe uma porta no caminho
desta com auxílio à função 'existePorta'.
-}
pousaCaixa :: Mapa -> Coordenadas -> Direcao -> Jogo
pousaCaixa l (x,y) d | (x == 0 && d == Oeste) || (x == xmax m && d == Este) =  Jogo l (Jogador (x,y) d True)
                     | d == Oeste && (elem (Bloco, (x-1,y-1)) m ||  elem (Caixa, (x-1,y-1)) m || elem (Porta, (x-1,y)) m) = Jogo l (Jogador (x,y) d True)
                     | d == Este && (elem (Bloco,(x+1,y-1)) m ||  elem (Caixa, (x+1,y-1)) m || elem (Porta,(x+1,y)) m) = Jogo l (Jogador (x,y) d True)
                     | d == Oeste && (elem (Bloco,(x-1,y)) m || elem (Caixa, (x-1,y)) m) && notElem (Porta, (x-1,y-1)) m = Jogo (constroiMapa((Caixa,(x-1,y-1)):m)) (Jogador (x,y) Oeste False)
                     | d == Este && (elem (Bloco,(x+1,y)) m || elem (Caixa, (x+1,y)) m) && notElem (Porta,(x+1,y+1)) m = Jogo (constroiMapa((Caixa,(x+1,y-1)):m)) (Jogador (x,y) Este False)
                     | d == Oeste && existePorta m (x-1,y) (pousaCaixaAux m (x-1,y+1))= Jogo (constroiMapa (pousaCaixaAux m (x-1, y+1) : m)) (Jogador (x,y) Oeste False)
                     | d == Este && existePorta m (x+1,y) (pousaCaixaAux m (x+1,y+1))=Jogo (constroiMapa (pousaCaixaAux m (x+1, y+1) : m)) (Jogador (x,y) Este False)
                     | otherwise = Jogo l (Jogador (x,y) d True) 
        where m = desconstroiMapa l
{-|
A função 'existePorta' verifica se a porta está no local onde a caixa vai ser colocada e no caminho da queda da mesma. Se não, então irá devolver True, caso contrário, devolve False.
-}
existePorta :: [(Peca,Coordenadas)] -> Coordenadas -> (Peca, Coordenadas) -> Bool 
existePorta m (x,y) (p,(x1,y1)) | x == x1 && y == y1 && notElem (Porta, (x,y)) m = True
                                | elem (Porta, (x,y)) m = False 
                                | otherwise = existePorta m (x,y+1) (p,(x1,y1))

{-|
A função 'pousaCaixaAux' começa por verificar se existe o chão imediatamente à frente do jogador. Se este não existir, a função é chamada recursivamente para a posição da linha abaixo, para assim procurar
o chão mais abaixo.
-}
pousaCaixaAux :: [(Peca, Coordenadas)] -> Coordenadas -> (Peca,Coordenadas)
pousaCaixaAux m (x,y)| elem (Caixa,(x,y)) m || elem (Bloco, (x,y)) m = (Caixa,(x,y-1))
                     | otherwise = pousaCaixaAux m (x,y+1)

{-|
== Correr Movimentos

A função 'corrermovimentos' vai aplicar cada movimento de uma lista a um jogo utilizando a função 'moveJogador' defenida anteriormente.
-}
correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos jogo [] = jogo
correrMovimentos jogo (m:ms) = correrMovimentos (moveJogador jogo m) ms 

