-- Question 1; implement rose
square :: (Num a) => a -> a
square a = a * a

half :: (Fractional a) => a -> a
half a = a / 2

double :: (Num a) => a -> a
double a = a * 2

hypotenuse :: (Floating a) => a -> a -> a
hypotenuse a o = sqrt $ square a + square o

class Shape a where
  area :: a -> Double

class ShapeRecursion a where
  inner :: a -> a

newtype Square = Square {side :: Double} deriving (Show, Eq)

instance Shape Square where
  area :: Square -> Double
  area (Square s) = square s

newtype Circle = Circle {radius :: Double} deriving (Show, Eq)

instance Shape Circle where
  area :: Circle -> Double
  area (Circle r) = pi * square r

newtype Petal = Petal {background :: Square} deriving (Show, Eq)

instance Shape Petal where
  area :: Petal -> Double
  area (Petal bg) = area innerCircle - area innerSquare
    where
      halfBackgroundSide = half $ side bg
      innerCircle = Circle halfBackgroundSide
      innerSquare = Square halfBackgroundSide

instance ShapeRecursion Petal where
  inner :: Petal -> Petal
  inner (Petal bg) = Petal $ Square $ hypotenuse halfSide halfSide
    where
      halfSide = half $ side bg

newtype Rose = Rose {petal :: Petal} deriving (Show, Eq)

instance Shape Rose where
  area :: Rose -> Double
  -- area calculation by analysis
  area (Rose p) = double $ area p

instance ShapeRecursion Rose where
  inner :: Rose -> Rose
  inner (Rose p) = Rose $ inner p

roseAreaWithRecurison :: Rose -> Double
roseAreaWithRecurison r
  | petalArea < epsilon = petalArea
  | otherwise = petalArea + roseAreaWithRecurison (inner r)
  where
    epsilon = 0.1
    petalArea = area $ petal r
