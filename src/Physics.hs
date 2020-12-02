module Physics (
  Vector'(..),
  addV,
  Vector(..),
  Position,
  Velocity,
  Angle(..),
  Acceleration,
  updatePosition,
  updateVelocity
) where

data Vector' = Vector' Float Float
  deriving (Eq, Show)

class Vector a where
  createV :: Float -> Float -> a
  toPair :: a -> (Float, Float)
  rotateV :: Angle -> a -> a
  rotateV (Angle ang) v = let
                    ang' = ang * pi / 180
                    (x, y) = toPair v
                  in createV (cos ang' * x - sin ang' * y) (sin ang' * x + cos ang' * y)
  calculateAngle :: a -> a -> Angle
  calculateAngle v1 v2 = let
                           (x1, y1) = toPair v1
                           (x2, y2) = toPair v2
                         in Angle $ atan2 (y2 - y1) (x2 - x1) * 180 / pi

newtype Position = Position Vector' deriving (Eq, Show)
newtype Velocity = Velocity Vector' deriving (Eq, Show)
newtype Acceleration = Acceleration Vector' deriving (Eq, Show)
newtype Angle = Angle Float deriving (Eq, Show)

instance Vector Position where
  createV x y = Position $ Vector' x y
  toPair (Position (Vector' x y)) = (x, y)

instance Vector Velocity where
  createV x y = Velocity $ Vector' x y
  toPair (Velocity (Vector' x y)) = (x, y)

instance Vector Acceleration where
  createV x y = Acceleration $ Vector' x y
  toPair (Acceleration (Vector' x y)) = (x, y)

instance Vector Vector' where
  createV = Vector'
  toPair (Vector' x y) = (x, y)

addV :: Vector' -> Vector' -> Vector'
addV (Vector' x y) (Vector' x' y') = Vector' (x + x') (y + y')

updatePosition :: Velocity -> Position -> Position
updatePosition (Velocity v) (Position p) = Position $ addV v p

updateVelocity :: Acceleration -> Velocity -> Velocity
updateVelocity (Acceleration v) (Velocity p) = Velocity $ addV v p
