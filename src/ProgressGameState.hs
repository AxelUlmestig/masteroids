{-# LANGUAGE RankNTypes #-}

module ProgressGameState (progressGameState) where

import           Control.Lens    (at, imap, ix, over, set, view)
import           Data.Fixed      (mod')
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromJust)

import           GameState       (EntityType (Player), GameState (..),
                                  gameStateAnglesL, gameStateEntityTypesL,
                                  gameStatePositionsL, gameStateVelocitiesL)
import           Newtypes        (Acceleration (Acceleration), Angle,
                                  Position (Position), Velocity (Velocity),
                                  calculateAngle, rotateA, updatePosition,
                                  updateVelocity)
import           Vector          (Vector (Vector))

playerAcceleration :: Acceleration
playerAcceleration = Acceleration $ Vector 0.3 0

progressGameState :: Float -> GameState -> GameState
progressGameState _ = foldr (.) id [updatePositions, borderPatrol', updatePlayerAngle, acceleratePlayer]

updatePlayerAngle :: GameState -> GameState
updatePlayerAngle gs = let
                         mp = mousePosition gs :: Position

                         update :: Int -> GameState -> GameState
                         update pid = let
                                        -- pp will crash if the player position is missing
                                        pp = fromJust $ view (gameStatePositionsL . at pid) gs :: Position
                                      in set (gameStateAnglesL . ix pid) (calculateAngle pp mp)

                         setAngles = foldr (.) id $ update <$> entities Player gs
                       in
                         setAngles gs

acceleratePlayer :: GameState -> GameState
acceleratePlayer gs = if accelerating gs
                      then let
                        update :: Int -> GameState -> GameState
                        update pid = let
                                       -- playerAngle will crash if the player angle is missing
                                       playerAngle = fromJust $ view (gameStateAnglesL . at pid) gs :: Angle
                                       acceleration = rotateA playerAngle playerAcceleration :: Acceleration
                                     in over (gameStateVelocitiesL . ix pid) (updateVelocity acceleration)

                        setVelocities = foldr (.) id $ update <$> entities Player gs
                      in setVelocities gs
                      else gs

borderPatrol' :: GameState -> GameState
borderPatrol' gs = let
                     constrainPosition (Position (Vector x y)) = Position $ Vector (constrain (gameWidth gs) x) (constrain (gameHeight gs) y)
                     constrain width value          = ((value + limit) `mod'` fromIntegral width) - limit
                       where
                         limit = fromIntegral width / 2
                   in over gameStatePositionsL (fmap constrainPosition) gs

updatePositions :: GameState -> GameState
updatePositions gs = let
                       applyVelocity :: Int -> Position -> Position
                       applyVelocity i = updatePosition $ M.findWithDefault (Velocity (Vector 0 0)) i (velocities gs)
                     in over gameStatePositionsL (imap applyVelocity) gs

entities :: EntityType -> GameState -> [Int]
entities et = fmap fst . filter ((==et) . snd) . M.toList . view gameStateEntityTypesL
