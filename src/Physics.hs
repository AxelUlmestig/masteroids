module Physics (
  Vector(..),
  Position,
  Velocity,
  Acceleration,
  Distance,
  Angle(..),
  Spin(..),
  Radius,
  Mass,
  Hitpoints,
  applyVelocity,
  applyAcceleration,
  subtractVelocity,
  distance,
  bounce,
  movePosition,
  spinAngle
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
  addV :: a -> a -> a
  addV v1 v2 = let
                 (x1, y1) = toPair v1
                 (x2, y2) = toPair v2
               in createV (x1 + x2) (y1 + y2)
  subtractV :: a -> a -> a
  subtractV = addV . scaleV (-1)
  absV :: a -> Float
  absV v = let
             (x, y) = toPair v
           in sqrt $ x * x + y * y
  scaleV :: Float -> a -> a
  scaleV s v = let
                 (x, y) = toPair v
               in createV (s * x) (s * y)
  normalizeV :: a -> a
  normalizeV v = scaleV (1 / absV v) v

newtype Position = Position Vector' deriving (Eq, Show)
newtype Velocity = Velocity Vector' deriving (Eq, Show)
newtype Acceleration = Acceleration Vector' deriving (Eq, Show)
newtype Distance = Distance Vector' deriving (Eq, Show)
newtype Angle = Angle Float deriving (Eq, Show)
newtype Spin = Spin Float deriving (Eq, Show)
type Radius = Float
type Mass = Float
type Hitpoints = Int

instance Vector Position where
  createV x y = Position $ Vector' x y
  toPair (Position (Vector' x y)) = (x, y)

instance Vector Velocity where
  createV x y = Velocity $ Vector' x y
  toPair (Velocity (Vector' x y)) = (x, y)

instance Vector Acceleration where
  createV x y = Acceleration $ Vector' x y
  toPair (Acceleration (Vector' x y)) = (x, y)

instance Vector Distance where
  createV x y = Distance $ Vector' x y
  toPair (Distance (Vector' x y)) = (x, y)

instance Vector Vector' where
  createV = Vector'
  toPair (Vector' x y) = (x, y)

addV' :: Vector' -> Vector' -> Vector'
addV' (Vector' x y) (Vector' x' y') = Vector' (x + x') (y + y')

scaleV' :: Float -> Vector' -> Vector'
scaleV' k (Vector' x y) = Vector' (k * x) (k * y)

subtractV' :: Vector' -> Vector' -> Vector'
subtractV' v1 = addV' (scaleV' (-1) v1)

dotProduct :: Vector' -> Vector' -> Float
dotProduct (Vector' x1 y1) (Vector' x2 y2) = x1 * x2 + y1 * y2

movePosition :: Distance -> Position -> Position
movePosition (Distance d) (Position p) = Position $ addV' d p

distance :: (Float, Float) -> Position -> Position -> Distance
distance (xMax, yMax) (Position (Vector' x1 y1)) (Position (Vector' x2 y2)) = let
    dx = smallestModulusDistance xMax x1 x2
    dy = smallestModulusDistance yMax y1 y2
  in createV dx dy

smallestModulusDistance :: Float -> Float -> Float -> Float
smallestModulusDistance xMax x1 x2 = let
                    small = min x1 x2
                    big = max x1 x2
                  in signum (x2 - x1) * minAbs (big - small) (big - small - xMax)

minAbs :: Float -> Float -> Float
minAbs a b = if abs a < abs b then a else b

applyVelocity :: Velocity -> Position -> Position
applyVelocity (Velocity v) (Position p) = Position $ addV' v p

applyAcceleration :: Acceleration -> Velocity -> Velocity
applyAcceleration (Acceleration v) (Velocity p) = Velocity $ addV' v p

subtractVelocity :: Velocity -> Velocity -> Velocity
subtractVelocity (Velocity v1) (Velocity v2) = Velocity $ subtractV' v1 v2

spinAngle :: Spin -> Angle -> Angle
spinAngle (Spin s) (Angle a) = Angle (a + s)

bounce :: (Float, Float) -> (Position, Velocity, Float) -> (Position, Velocity, Float) -> (Velocity, Velocity)
bounce border (p1, Velocity v1, m1) (p2, Velocity v2, m2) = let
                                                       (Distance direction) = normalizeV $ distance border p1 p2
                                                       a = (2 / (1 / m1 + 1 / m2)) * dotProduct direction (subtractV' v2 v1) -- 'a' in https://www.sjsu.edu/faculty/watkins/collision.htm
                                                       v1' = subtractV' (scaleV' (a / m1) direction) v1
                                                       v2' = addV'      (scaleV' (a / m2) direction) v2
                                                     in if p1 == p2 then -- please quickcheck
                                                       (Velocity v1, Velocity v2)
                                                     else
                                                       (Velocity v1', Velocity v2')
