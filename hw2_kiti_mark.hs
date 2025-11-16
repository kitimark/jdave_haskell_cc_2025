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

-- Question 2; implement some data structure (prefer avl tree)
data AVLTree a
  = EmptyAVLTree
  | Node
      { value :: a,
        left :: AVLTree a,
        right :: AVLTree a
      }
  deriving (Show, Eq)

instance Foldable AVLTree where
  foldr :: (a -> b -> b) -> b -> AVLTree a -> b
  foldr _ e EmptyAVLTree = e
  foldr f e (Node x lt rt) = foldr f (f x (foldr f e rt)) lt

singleAVLNode :: (Ord a) => a -> AVLTree a
singleAVLNode a = Node a EmptyAVLTree EmptyAVLTree

insertAVLNode :: (Ord a) => AVLTree a -> a -> AVLTree a
insertAVLNode EmptyAVLTree a = singleAVLNode a
insertAVLNode (Node v lt rt) a
  | a == v = rebalanceTree $ Node v lt rt
  | a < v = rebalanceTree $ Node v (insertAVLNode lt a) rt
  | a > v = rebalanceTree $ Node v lt (insertAVLNode rt a)

rebalanceTree :: (Ord a) => AVLTree a -> AVLTree a
rebalanceTree a
  | factor == -2 = rebalanceTreeLeft a
  | factor == 2 = rebalanceTreeRight a
  | otherwise = a
  where
    factor = balanceFactor a

rebalanceTreeLeft :: (Ord a) => AVLTree a -> AVLTree a
rebalanceTreeLeft a
  | factor == -1 = rotateLeftLeft a
  | factor == 1 = rotateLeftRight a
  where
    factor = balanceFactor (left a)

rotateLeftLeft :: AVLTree a -> AVLTree a
rotateLeftLeft (Node v (Node lv llt lrt) rt) = Node lv llt (Node v lrt rt)

rotateLeftRight :: AVLTree a -> AVLTree a
rotateLeftRight (Node v (Node lv llt (Node lrv lrlt lrrt)) rt) =
  Node lrv (Node lv llt lrlt) (Node v lrrt rt)

rebalanceTreeRight :: (Ord a) => AVLTree a -> AVLTree a
rebalanceTreeRight a
  | factor == 1 = rotateRightRight a
  | factor == -1 = rotateRightLeft a
  where
    factor = balanceFactor (right a)

rotateRightRight :: AVLTree a -> AVLTree a
rotateRightRight (Node v lt (Node rv rlt rrt)) = Node rv (Node v lt rlt) rrt

rotateRightLeft :: AVLTree a -> AVLTree a
rotateRightLeft (Node v lt (Node rv (Node rlv rllt rlrt) rr)) =
  Node rlv (Node v lt rllt) (Node rv rlrt rr)

balanceFactor :: AVLTree a -> Int
balanceFactor (Node _ lt rt) = -heightTree lt + heightTree rt

heightTree :: AVLTree a -> Int
heightTree EmptyAVLTree = 0
heightTree (Node _ lt rt) = 1 + max (heightTree lt) (heightTree rt)
