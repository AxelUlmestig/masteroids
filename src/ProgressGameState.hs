{-# LANGUAGE RankNTypes #-}

module ProgressGameState (progressGameState) where

import           Control.Lens    (at, imap, ix, over, set, view)
import           Data.Fixed      (mod')
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromJust)

import           GameState       (EntityType (Player), GameState (..),
                                  gameStateAnglesL, gameStateEntityTypesL,
                                  gameStatePositionsL, gameStateSpinL,
                                  gameStateVelocitiesL)
import           Physics         (Acceleration, Angle, Position, calculateAngle,
                                  createV, rotateV, spinAngle, toPair,
                                  updatePosition, updateVelocity)

playerAcceleration :: Acceleration
playerAcceleration = createV 0.3 0

progressGameState :: Float -> GameState -> GameState
progressGameState _ = foldr (.) id [updatePositions, borderPatrol', updatePlayerAngle, acceleratePlayer, updateAngles]

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
                                       acceleration = rotateV playerAngle playerAcceleration :: Acceleration
                                     in over (gameStateVelocitiesL . ix pid) (updateVelocity acceleration)

                        setVelocities = foldr (.) id $ update <$> entities Player gs
                      in setVelocities gs
                      else gs

borderPatrol' :: GameState -> GameState
borderPatrol' gs = let
                     constrainPosition pos = createV (constrain (gameWidth gs) x) (constrain (gameHeight gs) y)
                       where
                         (x, y) = toPair pos

                     constrain width value = ((value + limit) `mod'` fromIntegral width) - limit
                       where
                         limit = fromIntegral width / 2
                   in over gameStatePositionsL (fmap constrainPosition) gs

updatePositions :: GameState -> GameState
updatePositions gs = let
                       applyVelocities = foldr (.) id . imap (flip M.adjust) . fmap updatePosition . view gameStateVelocitiesL
                     in over gameStatePositionsL (applyVelocities gs) gs

updateAngles :: GameState -> GameState
updateAngles gs = let
                    applySpin = foldr (.) id . imap (flip M.adjust) . fmap spinAngle . view gameStateSpinL
                  in over gameStateAnglesL (applySpin gs) gs

entities :: EntityType -> GameState -> [Int]
entities et = fmap fst . filter ((==et) . snd) . M.toList . view gameStateEntityTypesL
