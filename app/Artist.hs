module Artist where

import UdGraphic
import Test.QuickCheck
import Debug.Trace

-- Problema 1

separa :: Comanda -> [Comanda]
separa (com1 :#: com2) = separa com1 ++ separa com2
separa Para = [] -- Cas base
separa com = [com] -- Cas base

-- Problema 2

ajunta :: [Comanda] -> Comanda
ajunta [] = Para -- Cas base
ajunta [com] = com :#: Para -- Cas base
ajunta (com : l) = com :#: ajunta l

-- Problema 3

prop_equivalent :: Comanda -> Comanda -> Bool
prop_equivalent com1 com2 = separa com1 == separa com2

prop_split_join :: Comanda -> Bool
prop_split_join c = prop_equivalent (ajunta (separa c)) c

conteComposta :: [Comanda] -> Bool
conteComposta [] = False
conteComposta ((x :#: y):xs) = True
conteComposta (x:xs) = conteComposta xs

prop_split :: Comanda -> Bool
prop_split c = let llista = separa c
               in not(elem Para llista) && not(conteComposta llista)

-- Problema 4

copia :: Int -> Comanda -> Comanda
copia 1 com = com
copia m com = let n = m-1 
              in com :#: copia n com

-- Problema 5

pentagon :: Distancia -> Comanda
pentagon d = copia 5 (Avança d :#: Gira 72)

-- Problema 6

poligon :: Distancia -> Int -> Angle -> Comanda
poligon d n a = copia n (Avança d :#: Gira a)

prop_poligon_pentagon :: Bool
prop_poligon_pentagon = prop_equivalent (poligon 1 5 72) (pentagon 1)

-- Problema 7

espiral :: Distancia -> Int -> Distancia -> Angle -> Comanda
espiral d 1 _ a = (Avança d :#: Gira a)
espiral d m pas a = let n=m-1
                        sd=d+pas
                    in (Avança d :#: Gira a :#: espiral sd n pas a)

-- Problema 9

ajuntaOpt :: [Comanda] -> Comanda
ajuntaOpt [] = Para -- Cas base
ajuntaOpt [com] = if(com == Para || com == Avança 0 || com == Gira 0) -- Cas base
                  then
                      Para
                  else
                      com 
ajuntaOpt (Avança d:Avança e:l) = ajuntaOpt (Avança (d+e):l)
ajuntaOpt (Gira a:Gira b:l) = ajuntaOpt (Gira (a+b):l)
ajuntaOpt (com : l) = if(com == Para || com == Avança 0 || com == Gira 0)
                      then
                          ajuntaOpt l
                      else
                          com :#: ajuntaOpt l

optimitza :: Comanda -> Comanda
optimitza c = if(c/=copt) then
                optimitza(copt)
              else
                copt
            where 
                copt = ajuntaOpt (separa c)

-- Problema 10

triangle :: Int -> Comanda
triangle n = Gira 90 :#: fTriangle n

fTriangle :: Int -> Comanda
fTriangle 0 = Avança 30
fTriangle n = fTriangle (n-1) :#: Gira 90 :#: fTriangle (n-1) :#: Gira (-90) :#: fTriangle (n-1) :#: Gira (-90) :#: fTriangle (n-1) :#: Gira 90 :#: fTriangle (n-1)

-- Problema 11

fulla :: Int -> Comanda
fulla n = CanviaColor blau :#: fFulla n

fFulla :: Int -> Comanda
fFulla 0 = CanviaColor vermell :#: Avança 30
fFulla n = gFulla (n-1) :#: Branca (Gira (-45) :#: fFulla (n-1)) :#: Branca (Gira 45 :#: fFulla (n-1)) :#: Branca(gFulla (n-1) :#: fFulla (n-1))

gFulla :: Int -> Comanda
gFulla 0 = Avança 30
gFulla n = gFulla (n-1) :#: gFulla (n-1)

-- Problema 12

hilbert :: Int -> Comanda
hilbert n = lHilbert n

lHilbert :: Int -> Comanda
lHilbert 0 = Para
lHilbert n = Gira 90 :#: rHilbert (n-1) :#: Avança 30 :#: Gira (-90) :#: lHilbert (n-1) :#: Avança 30 :#: lHilbert (n-1) :#: Gira (-90) :#: Avança 30 :#: rHilbert (n-1) :#: Gira 90

rHilbert :: Int -> Comanda
rHilbert 0 = Para
rHilbert n = Gira (-90) :#: lHilbert (n-1) :#: Avança 30 :#: Gira 90 :#: rHilbert (n-1) :#: Avança 30 :#: rHilbert (n-1) :#: Gira 90 :#: Avança 30 :#: lHilbert (n-1) :#: Gira (-90)

-- Problema 13

fletxa :: Int -> Comanda
fletxa n = fFletxa n

fFletxa :: Int -> Comanda
fFletxa 0 = Avança 30
fFletxa n = gFletxa (n-1) :#: Gira 60 :#: fFletxa (n-1) :#: Gira 60 :#: gFletxa (n-1)

gFletxa :: Int -> Comanda
gFletxa 0 = Avança 30
gFletxa n = fFletxa (n-1) :#: Gira (-60) :#: gFletxa (n-1) :#: Gira (-60) :#: fFletxa (n-1)

-- Problema 14

branca :: Int -> Comanda
branca n = CanviaColor blau :#: gBranca n

gBranca :: Int -> Comanda
gBranca 0 = CanviaColor vermell :#: Avança 30
gBranca n = fBranca (n-1) :#: Gira (-22.5) :#: Branca(Branca(gBranca (n-1)) :#: Gira 22.5 :#: gBranca (n-1)) :#: Gira 22.5 :#: fBranca (n-1) :#: Branca(Gira 22.5 :#: fBranca (n-1) :#: gBranca (n-1)) :#: Gira (-22.5) :#: gBranca (n-1)

fBranca :: Int -> Comanda
fBranca 0 = Avança 30
fBranca n = fBranca (n-1) :#: fBranca (n-1)
