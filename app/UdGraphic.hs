module UdGraphic (
    Comanda(..),
    Distancia,
    Angle,
    Llapis(..), blau, vermell,
    display,
    execute
    )
    where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT hiding (Angle)
import Data.IORef
import Data.List
import Control.Monad( liftM, liftM2, liftM3 )
import System.Random
import Test.QuickCheck

infixr 5 :#:

-- Punts

data Pnt = Pnt Float Float
  deriving (Eq,Ord,Show)

instance Num Pnt where
  Pnt x y + Pnt x' y'  =  Pnt (x+x') (y+y')
  Pnt x y - Pnt x' y'  =  Pnt (x-x') (y-y')
  Pnt x y * Pnt x' y'  =  Pnt (x*x') (y*y')
  fromInteger          =  scalar . fromInteger
  abs (Pnt x y)        =  Pnt (abs x) (abs y)
  signum (Pnt x y)     =  Pnt (signum x) (signum y)

instance Fractional Pnt where
  Pnt x y / Pnt x' y'  =  Pnt (x/x') (y/y')
  fromRational         =  scalar . fromRational

scalar :: Float -> Pnt
scalar x  =  Pnt x x

scalarMin :: Pnt -> Pnt
scalarMin (Pnt x y)  =  scalar (x `min` y)

scalarMax :: Pnt -> Pnt
scalarMax (Pnt x y)  =  scalar (x `max` y)

dimensions :: Pnt -> (Int,Int)
dimensions (Pnt x y)  =  (ceiling x, ceiling y)

lub :: Pnt -> Pnt -> Pnt
Pnt x y `lub` Pnt x' y'  =  Pnt (x `max` x') (y `max` y')

glb :: Pnt -> Pnt -> Pnt
Pnt x y `glb` Pnt x' y'  =  Pnt (x `min` x') (y `min` y')

pointToSize :: Pnt -> Size
pointToSize (Pnt x y) = Size (ceiling x) (ceiling y)

sizeToPoint :: Size -> Pnt
sizeToPoint (Size x y) = Pnt (fromIntegral x) (fromIntegral y)

-- Colors

data Llapis = Color' GL.GLfloat GL.GLfloat GL.GLfloat
            | Transparent
            deriving (Eq, Ord, Show)

pencilToRGB :: Llapis -> GL.Color3 GL.GLfloat
pencilToRGB (Color' r g b)  =  GL.Color3 r g b
pencilToRGB Transparent  =  error "pencilToRGB: transparent"

blanc, negre, vermell, verd, blau :: Llapis
blanc   = Color' 1.0 1.0 1.0
negre   = Color' 0.0 0.0 0.0
vermell = Color' 1.0 0.0 0.0
verd    = Color' 0.0 1.0 0.0
blau    = Color' 0.0 0.0 1.0

-- Lines

data Ln = Ln Llapis Pnt Pnt
  deriving (Eq,Ord,Show)


-- Window parameters

theCanvas :: Pnt
theCanvas  =  Pnt 800 800

theBGcolor :: GL.Color3 GL.GLfloat
theBGcolor = pencilToRGB blanc



-- Main drawing and window functions

display :: Comanda -> IO ()
display c = do
  initialDisplayMode $= [DoubleBuffered]
  initialWindowSize  $= pointToSize theCanvas
  getArgsAndInitialize
  w <- createWindow "pencilcil Graphics"
  displayCallback $= draw c
  reshapeCallback $= Just (\x -> (viewport $= (Position 0 0, x)))
  --actionOnWindowClose $= ContinueExectuion
  draw c
  mainLoop

draw :: Comanda -> IO ()
draw c = do clear [ColorBuffer]
            loadIdentity
            background
            toGraphic $ rescale $ execute c
            swapBuffers

toGraphic :: [Ln] -> IO ()
toGraphic lines  = sequence_ (map f lines)
  where
  f (Ln pencil startP endP)  =
    GL.color (pencilToRGB pencil) >>
    GL.renderPrimitive GL.LineStrip (toVertex startP >> toVertex endP)

background :: IO ()
background = do GL.color theBGcolor
                GL.renderPrimitive GL.Polygon $ mapM_ GL.vertex
                      [GL.Vertex3 (-1) (-1) 0,
                       GL.Vertex3   1  (-1) 0,
                       GL.Vertex3   1    1  0,
                       GL.Vertex3 (-1)   1 (0::GL.GLfloat) ]


toVertex (Pnt x y)  =  GL.vertex $ GL.Vertex3
 (realToFrac x) (realToFrac y) (0::GL.GLfloat)



-- Definició de les comandes per moure el llapis

type Angle     = Float
type Distancia = Float
data Comanda   = Avança Distancia
               | Gira Angle
               | Comanda :#: Comanda
               | Para
               | CanviaColor Llapis
               | Branca Comanda
                deriving (Eq,Show)


-- Problema 8
-- Pas de comandes a lines a pintar per GL graphics

-- Tipus "EstatLlapis" auxiliar per determinar l'estat del llapis en cada comanda
data EstatLlapis = EstatLlapis Llapis Angle Pnt

-- Predicat auxiliar "polar" que ens calcula la desviació amb l'angle
polar :: Angle -> Pnt
polar a = Pnt (cos rad) (sin rad)
    where
        rad = a * pi / 180

-- Predicat auxiliar "executeLn" que ens retorna les línies i es va actualitzant l'estat del llapis
executeLn :: Comanda -> EstatLlapis -> ([Ln], EstatLlapis)
executeLn (Avança d) (EstatLlapis color angle inici) = (if color == Transparent -- Si el color del llapis és "Transparent" llavors no es dibuixa res
                                          then [] 
                                          else [Ln color inici final], EstatLlapis color angle final) -- Altrament, traça la línia amb totes les dades proporcionades
                                      where
                                          final = inici + scalar d * polar angle -- Càlcul del punt final amb la desviació corresponent (si es que en té)
executeLn Para estat = ([], estat) -- Si la comanda és un Para simplement no fa res
executeLn (Gira a) (EstatLlapis color angle inici) = ([], EstatLlapis color (angle-a) inici) -- S'agafa l'angle "a" i gira respecte l'angle anteriorment utilitzat

-- Les comandes compostes funcionen de forma que s'agafen les línies i l'estat del llapis al executar la primera comanda (com1) i es passa a la següent (com2)
executeLn (com1 :#: com2) estat = (linies, estat2) 
    where
        (linies1, estat1) = executeLn com1 estat
        (linies2, estat2) = executeLn com2 estat1
        linies = linies1 ++ linies2 -- Finalment es concatenen les línies que donaran de resultat haver executat com1 i com2 justament després
executeLn (CanviaColor color) (EstatLlapis _ angle inici) = ([], EstatLlapis color angle inici) -- Actualitza el color del llapis
-- El funcionament de "Branca" és simple: s'executen les comandes que venen després del "Branca", retorna les línies, però l'estat del llapis es deixa en l'estat en que es va començar la "Branca"
executeLn (Branca com) estat = (linies, estat)
    where
        (linies, _) = executeLn com estat

-- Execute com a tal, que retorna les línies que donaran de resultat el dibuix aplicat amb les comandes introduïdes, comença en el punt (0,0) i amb llapis negre
execute :: Comanda -> [Ln]
execute c  =  linies
    where
        (linies, _) = executeLn c (EstatLlapis negre 0 (Pnt 0 0))

-- Rescales all points in a list of lines
--  from an arbitrary scale
--  to (-1.-1) - (1.1)

rescale :: [Ln] -> [Ln]
rescale lines | points == [] = []
              | otherwise    = map f lines
  where
  f (Ln pencil p q)  =  Ln pencil (g p) (g q)
  g p             =  swap ((p - p0) / s)
  points          =  [ r | Ln pencil p q <- lines, r <- [p, q] ]
  hi              =  foldr1 lub points
  lo              =  foldr1 glb points
  s               =  scalarMax (hi - lo) * scalar (0.55)
  p0              =  (hi + lo) * scalar (0.5)
  swap (Pnt x y)  =  Pnt y x


-- Generators for QuickCheck

instance Arbitrary Llapis where
    arbitrary  =  sized pencil
        where
          pencil n  =  elements [negre,vermell,verd,blau,blanc,Transparent]


instance Arbitrary Comanda where
    arbitrary  =  sized cmd
        where
          cmd n  |  n <= 0     =  oneof [liftM (Avança . abs) arbitrary,
                                         liftM Gira arbitrary ]
                 |  otherwise  =  liftM2 (:#:) (cmd (n `div` 2)) (cmd (n `div`2))
