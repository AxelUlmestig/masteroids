{-# LANGUAGE RankNTypes #-}

module ProgressGameState (progressGameState) where

import           Control.Lens (Lens', over, set)
import           Data.Fixed   (mod')

import           GameState    (GameState (..), gameStatePlayerAngleL,
                               gameStatePlayerPositionL,
                               gameStatePlayerVelocityL)
import           Vector       (Vector (..), addV, calculateAngle, rotateV,
                               vectorXL, vectorYL)

playerAcceleration :: Vector Float
playerAcceleration = Vector 0.3 0

progressGameState :: Float -> GameState -> GameState
progressGameState _ = foldr (.) id [angleUpdate, velocityUpdate, accelerationUpdate, borderPatrolX, borderPatrolY]
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
