module Vector (
  Vector(..),
  vectorXL,
  vectorYL,
  addV,
  subtractV,
  scaleV,
  rotateV,
  calculateAngle
) where

import           Control.Lens (Lens', lens)

data Vector a = Vector a a

instance (Eq a) => Eq (Vector a) where
  Vector x y == Vector x' y' = x == x' && y == y'

instance (Show a) => Show (Vector a) where
  show (Vector x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

vectorXL :: Lens' (Vector a) a
vectorXL = lens (\(Vector x _) -> x) (\(Vector _ y) x -> Vector x y)

vectorYL :: Lens' (Vector a) a
vectorYL = lens (\(Vector _ y) -> y) (\(Vector x _) y -> Vector x y)

addV :: (Num a) => Vector a -> Vector a -> Vector a
addV (Vector x y) (Vector x' y') = Vector (x + x') (y + y')

subtractV :: (Num a) => Vector a -> Vector a -> Vector a
subtractV (Vector x y) (Vector x' y') = Vector (x - x') (y - y')

scaleV :: (Num a) => a -> Vector a -> Vector a
scaleV a (Vector x y) = Vector (a * x) (a * y)

rotateV :: (Floating a) => a -> Vector a -> Vector a
rotateV a (Vector x y) = Vector (cos a' * x - sin a' * y) (sin a' * x + cos a' * y)
  where a' = a * pi / 180

calculateAngle :: (RealFloat a) => Vector a -> Vector a -> a
calculateAngle (Vector px py) (Vector mx my) = atan2 (my - py) (mx - px) * 180 / pi
