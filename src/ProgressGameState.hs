{-# LANGUAGE RankNTypes #-}

module ProgressGameState (progressGameState, acceleratePlayer) where -- todo remove acceleratePlayer

import           Control.Lens    (at, imap, ix, over, set, view)
import           Data.Fixed      (mod')
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromJust)

import           GameState       (GameState (..), gameStateAnglesL,
                                  gameStatePositionsL, gameStateVelocitiesL,
                                  playerId)
import           Vector          (Vector (..), addV, calculateAngle, rotateV)

playerAcceleration :: Vector Float
playerAcceleration = Vector 0.3 0

progressGameState :: Float -> GameState -> GameState
progressGameState _ = foldr (.) id [updatePositions, borderPatrol', updatePlayerAngle, acceleratePlayer]

updatePlayerAngle :: GameState -> GameState
updatePlayerAngle gs = let
                         -- pp will crash if the player position is missing
                         pp = fromJust $ view (gameStatePositionsL . at playerId) gs :: Vector Float
                         mp = mousePosition gs :: Vector Float
                       in
                         set (gameStateAnglesL . ix playerId) (calculateAngle pp mp) gs

acceleratePlayer :: GameState -> GameState
acceleratePlayer gs = if accelerating gs
                      then let
                        -- playerAngle will crash if the player angle is missing
                        playerAngle = fromJust $ view (gameStateAnglesL . at playerId) gs
                        acceleration = rotateV playerAngle playerAcceleration
                      in over (gameStateVelocitiesL . ix playerId) (addV acceleration) gs
                      else gs

borderPatrol' :: GameState -> GameState
borderPatrol' gs = let
                     constrainPosition (Vector x y) = Vector (constrain (gameWidth gs) x) (constrain (gameHeight gs) y)
                     constrain width value          = ((value + limit) `mod'` fromIntegral width) - limit
                       where
                         limit = fromIntegral width / 2
                   in over gameStatePositionsL (fmap constrainPosition) gs

updatePositions :: GameState -> GameState
updatePositions gs = let
                       applyVelocity :: Int -> Vector Float -> Vector Float
                       applyVelocity i = addV $ M.findWithDefault (Vector 0 0) i (velocities gs)
                     in over gameStatePositionsL (imap applyVelocity) gs
