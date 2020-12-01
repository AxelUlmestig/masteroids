module Newtypes (
  Position(..),
  Velocity(..),
  Angle(..),
  Acceleration(..),
  calculateAngle,
  updatePosition,
  updateVelocity,
  rotateA
) where

import           Vector (Vector (Vector), addV, rotateV)

newtype Position = Position (Vector Float) deriving (Eq, Show)
newtype Velocity = Velocity (Vector Float) deriving (Eq, Show)
newtype Acceleration = Acceleration (Vector Float) deriving (Eq, Show)
newtype Angle = Angle Float deriving (Eq, Show)

calculateAngle :: Position -> Position -> Angle
calculateAngle (Position (Vector px py)) (Position (Vector mx my)) = Angle $ atan2 (my - py) (mx - px) * 180 / pi

updatePosition :: Velocity -> Position -> Position
updatePosition (Velocity v) (Position p) = Position $ addV v p

updateVelocity :: Acceleration -> Velocity -> Velocity
updateVelocity (Acceleration v) (Velocity p) = Velocity $ addV v p

rotateA :: Angle -> Acceleration -> Acceleration
rotateA (Angle ang) (Acceleration acc) = Acceleration $ rotateV ang acc
