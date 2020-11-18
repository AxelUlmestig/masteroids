module Main where

import           Control.Lens                       (over, set, view)
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game

import           GameState                          (GameState (..),
                                                     gameStateAcceleratingL,
                                                     gameStateMousePositionL,
                                                     gameStatePlayerAngleL,
                                                     gameStatePlayerPositionL,
                                                     gameStatePlayerVelocityL,
                                                     initGameState)
import           Vector                             (Vector (..), addV,
                                                     calculateAngle, rotateV,
                                                     vectorXL, vectorYL)

playerAcceleration = Vector 0.3 0

windowDisplay :: Display
windowDisplay = InWindow "Window" (800, 800) (10, 10)

type World = (Float, Float, Float)

main :: IO ()
main = do
  asteroidBMP <- rotate 90 <$> loadBMP "assets/playerShip2-blue.bmp"
  play
    windowDisplay
    black
    60
    initGameState
    (drawingFunc asteroidBMP)
    inputHandler
    updateFunc

drawingFunc :: Picture -> GameState -> Picture
drawingFunc picture GameState{ playerPosition = (Vector x y), playerAngle = a } = translate x y (rotate (-a) picture)

inputHandler :: Event -> GameState -> GameState
inputHandler (EventKey (SpecialKey KeyUp) Down _ _) gs    = over gameStatePlayerPositionL (addV (Vector 0 10)) gs
inputHandler (EventKey (SpecialKey KeyDown) Down _ _) gs  = over gameStatePlayerPositionL (addV (Vector 0 (-10))) gs
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) gs = over gameStatePlayerPositionL (addV (Vector 10 0)) gs
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) gs  = over gameStatePlayerPositionL (addV (Vector (-10) 0)) gs
inputHandler (EventMotion (mx, my)) gs                    = set  gameStateMousePositionL (Vector mx my) gs
inputHandler (EventKey (SpecialKey KeySpace) Down _ _) gs = set  gameStateAcceleratingL True gs
inputHandler (EventKey (SpecialKey KeySpace) Up _ _) gs   = set  gameStateAcceleratingL False gs
inputHandler _ gs                                         = gs

updateFunc :: Float -> GameState -> GameState
updateFunc _ = foldr (.) id [angleUpdate, velocityUpdate, accelerationUpdate, borderPatrolX, borderPatrolX', borderPatrolY, borderPatrolY']
  where
    angleUpdate gs = set gameStatePlayerAngleL (calculateAngle (playerPosition gs) (mousePosition gs)) gs
    velocityUpdate gs = over gameStatePlayerPositionL (addV (playerVelocity gs)) gs
    accelerationUpdate gs = if accelerating gs then over gameStatePlayerVelocityL (addV (rotateV (playerAngle gs) playerAcceleration)) gs else gs
    borderPatrolX gs = if view (gameStatePlayerPositionL . vectorXL) gs > 400 then set (gameStatePlayerPositionL . vectorXL) (-400) gs else gs
    borderPatrolX' gs = if view (gameStatePlayerPositionL . vectorXL) gs < (-400) then set (gameStatePlayerPositionL . vectorXL) 400 gs else gs
    borderPatrolY gs = if view (gameStatePlayerPositionL . vectorYL) gs > 400 then set (gameStatePlayerPositionL . vectorYL) (-400) gs else gs
    borderPatrolY' gs = if view (gameStatePlayerPositionL . vectorYL) gs < (-400) then set (gameStatePlayerPositionL . vectorYL) 400 gs else gs
