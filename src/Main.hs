module Main where

import           Control.Lens                       (over, set, view)
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game

import           Assets                             (Assets, fireSprite,
                                                     loadAssets, playerSprite)
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

main :: IO ()
main = do
  assets <- loadAssets
  play
    windowDisplay
    black
    60
    initGameState
    (drawingFunc assets)
    inputHandler
    updateFunc

drawingFunc :: Assets -> GameState -> Picture
drawingFunc assets GameState{ playerPosition = (Vector x y), playerAngle = a, accelerating = acc } = pictures [player, fire]
  where
    player = translate x y (rotate (-a) (playerSprite assets))
    fire = if acc then translate x' y' (rotate (-a) (fireSprite assets)) else Blank
      where
        (Vector x' y') = addV (Vector x y) (rotateV (180 + a) (Vector 50 0))

inputHandler :: Event -> GameState -> GameState
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
    borderPatrolX  gs = if view (gameStatePlayerPositionL . vectorXL) gs > 400    then set (gameStatePlayerPositionL . vectorXL) (-400) gs  else gs
    borderPatrolX' gs = if view (gameStatePlayerPositionL . vectorXL) gs < (-400) then set (gameStatePlayerPositionL . vectorXL) 400 gs     else gs
    borderPatrolY  gs = if view (gameStatePlayerPositionL . vectorYL) gs > 400    then set (gameStatePlayerPositionL . vectorYL) (-400) gs  else gs
    borderPatrolY' gs = if view (gameStatePlayerPositionL . vectorYL) gs < (-400) then set (gameStatePlayerPositionL . vectorYL) 400 gs     else gs
