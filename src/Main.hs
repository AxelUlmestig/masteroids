module Main where

import           Control.Lens                       (over, set)
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game

import           GameState                          (GameState (..),
                                                     gameStateMousePositionL,
                                                     gameStatePlayerAngleL,
                                                     gameStatePlayerPositionL,
                                                     initGameState)
import           Vector                             (Vector (..), addV,
                                                     calculateAngle)

windowDisplay :: Display
windowDisplay = InWindow "Window" (750, 750) (10, 10)

type World = (Float, Float, Float)

main :: IO ()
main = do
  asteroidBMP <- rotate 90 <$> loadBMP "assets/playerShip2-blue.bmp"
  play
    windowDisplay
    white
    60
    initGameState
    (drawingFunc asteroidBMP)
    inputHandler
    updateFunc

drawingFunc :: Picture -> GameState -> Picture
drawingFunc picture GameState{ playerPosition = (Vector x y), playerAngle = a } = translate x y (rotate a picture)

inputHandler :: Event -> GameState -> GameState
inputHandler (EventKey (SpecialKey KeyUp) Down _ _)    = over gameStatePlayerPositionL (addV (Vector 0 10))
inputHandler (EventKey (SpecialKey KeyDown) Down _ _)  = over gameStatePlayerPositionL (addV (Vector 0 (-10)))
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) = over gameStatePlayerPositionL (addV (Vector 10 0))
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _)  = over gameStatePlayerPositionL (addV (Vector (-10) 0))
inputHandler (EventMotion (mx, my) )                   = set gameStateMousePositionL (Vector mx my)
inputHandler _                                         = id

updateFunc :: Float -> GameState -> GameState
updateFunc _ gs = set gameStatePlayerAngleL (calculateAngle (playerPosition gs) (mousePosition gs)) gs
