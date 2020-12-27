module Constants (
  defaultHeight,
  defaultWidth,

  playerAcceleration,
  playerMass,
  playerMaxHP,
  playerRadius,

  asteroidDefaultSpin,
  asteroidMass,
  asteroidRadius,
  asteroidSpriteDefaultRadius,

  laserBaseSpeed,
  laserDamage,
  laserMass,
  laserRadius
) where

import           Physics (Acceleration, Hitpoints, Mass, Radius, Spin (Spin),
                          Velocity, createV)

defaultHeight :: Int
defaultHeight = 800

defaultWidth :: Int
defaultWidth = 800

-- Player
playerAcceleration :: Acceleration
playerAcceleration = createV 0.3 0

playerMass :: Mass
playerMass = 10

playerMaxHP :: Hitpoints
playerMaxHP = 10

playerRadius :: Radius
playerRadius = 50

-- Asteroid
asteroidDefaultSpin :: Spin
asteroidDefaultSpin = Spin 1

asteroidMass :: Mass
asteroidMass = 30

asteroidRadius :: Radius
asteroidRadius = 50

asteroidSpriteDefaultRadius :: Radius
asteroidSpriteDefaultRadius = 50

-- Laser

laserBaseSpeed :: Velocity
laserBaseSpeed = createV 30 0

laserDamage :: Hitpoints
laserDamage = 1

laserMass :: Mass
laserMass = 1

laserRadius :: Radius
laserRadius = 5
