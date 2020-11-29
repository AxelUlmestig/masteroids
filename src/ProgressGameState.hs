{-# LANGUAGE RankNTypes #-}

module ProgressGameState (progressGameState) where

import           Control.Lens    (at, imap, ix, over, set, view)
import           Data.Fixed      (mod')
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromJust)

import           GameState       (EntityType (Player), GameState (..),
                                  gameStateAnglesL, gameStateEntityTypesL,
                                  gameStatePositionsL, gameStateVelocitiesL)
import           Vector          (Vector (..), addV, calculateAngle, rotateV)

playerAcceleration :: Vector Float
playerAcceleration = Vector 0.3 0

progressGameState :: Float -> GameState -> GameState
progressGameState _ = foldr (.) id [updatePositions, borderPatrol', updatePlayerAngle, acceleratePlayer]

updatePlayerAngle :: GameState -> GameState
updatePlayerAngle gs = let
                         mp = mousePosition gs :: Vector Float

                         update :: Int -> GameState -> GameState
                         update pid = let
                                        -- pp will crash if the player position is missing
                                        pp = fromJust $ view (gameStatePositionsL . at pid) gs :: Vector Float
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
                                       playerAngle = fromJust $ view (gameStateAnglesL . at pid) gs
                                       acceleration = rotateV playerAngle playerAcceleration
                                     in over (gameStateVelocitiesL . ix pid) (addV acceleration)

                        setVelocities = foldr (.) id $ update <$> entities Player gs
                      in setVelocities gs
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

entities :: EntityType -> GameState -> [Int]
entities et = fmap fst . filter ((==et) . snd) . M.toList . view gameStateEntityTypesL
