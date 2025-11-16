square :: (Num a) => a -> a
square a = a * a

half :: (Fractional a) => a -> a
half a = a / 2

double :: (Num a) => a -> a
double a = a * 2

class Shape a where
  area :: a -> Double

newtype Square = Square {side :: Double} deriving (Show, Eq)

instance Shape Square where
  area :: Square -> Double
  area (Square s) = square s

newtype Circle = Circle {radius :: Double} deriving (Show, Eq)

instance Shape Circle where
  area :: Circle -> Double
  area (Circle r) = pi * square r

newtype Petal = Petal {petalSide :: Double} deriving (Show, Eq)

instance Shape Petal where
  area :: Petal -> Double
  area (Petal s) = (area . Circle $ half s) - (area . Square $ half s)

newtype Rose = Rose {petal :: Petal} deriving (Show, Eq)

instance Shape Rose where
  area :: Rose -> Double
  -- area calculation by analysis
  area (Rose p) = double $ area p
