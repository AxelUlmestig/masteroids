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
windowDisplay = InWindow "Window" (500, 500) (10, 10)

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
inputHandler (EventKey (SpecialKey KeyUp) Down _ _) gs    = over gameStatePlayerPositionL (addV (Vector 0 10)) gs
inputHandler (EventKey (SpecialKey KeyDown) Down _ _) gs  = over gameStatePlayerPositionL (addV (Vector 0 (-10))) gs
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) gs = over gameStatePlayerPositionL (addV (Vector 10 0)) gs
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) gs  = over gameStatePlayerPositionL (addV (Vector (-10) 0)) gs
inputHandler (EventMotion (mx, my) ) gs                   = set gameStatePlayerAngleL (calculateAngle (playerPosition gs) (Vector mx my)) gs
inputHandler _ gs                                         = gs

updateFunc :: Float -> GameState -> GameState
updateFunc = const id
-- updateFunc :: Float -> World -> World
-- updateFunc _ (x, y, a) = (towardCenter x, towardCenter y, a)
--   where
--     towardCenter :: Float -> Float
--     towardCenter c = if abs c < 0.25
--       then 0
--       else if c > 0
--         then c - 0.25
--         else c + 0.25
