{-# LANGUAGE RankNTypes #-}

module Main where

import           Control.Lens                       (Lens', over, set, view)
import           Data.Fixed                         (mod')
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game

import           Assets                             (Assets, fireSprite,
                                                     loadAssets, playerSprite)
import           GameState                          (GameState (..),
                                                     defaultHeight,
                                                     defaultWidth,
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
windowDisplay = InWindow "Window" (defaultWidth, defaultHeight) (10, 10)

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
inputHandler (EventMotion (mx, my)) gs                    = set gameStateMousePositionL (Vector mx my) gs
inputHandler (EventKey (SpecialKey KeySpace) Down _ _) gs = set gameStateAcceleratingL True gs
inputHandler (EventKey (SpecialKey KeySpace) Up _ _) gs   = set gameStateAcceleratingL False gs
inputHandler (EventResize (width, height)) gs             = gs { gameWidth = width, gameHeight = height }
inputHandler _ gs                                         = gs

updateFunc :: Float -> GameState -> GameState
updateFunc _ = foldr (.) id [angleUpdate, velocityUpdate, accelerationUpdate, borderPatrolX, borderPatrolY]
  where
    angleUpdate gs = set gameStatePlayerAngleL (calculateAngle (playerPosition gs) (mousePosition gs)) gs
    velocityUpdate gs = over gameStatePlayerPositionL (addV (playerVelocity gs)) gs
    accelerationUpdate gs = if accelerating gs then over gameStatePlayerVelocityL (addV (rotateV (playerAngle gs) playerAcceleration)) gs else gs
    borderPatrolX gs = borderPatrol (gameWidth gs) (gameStatePlayerPositionL . vectorXL) gs
    borderPatrolY gs = borderPatrol (gameHeight gs) (gameStatePlayerPositionL . vectorYL) gs

borderPatrol :: Int -> Lens' GameState Float -> GameState -> GameState
borderPatrol width l gs = let
                            f value =  ((value + limit) `mod'` fromIntegral width) - limit
                              where
                                limit = fromIntegral width / 2
                          in over l f gs
