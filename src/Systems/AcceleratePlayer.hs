module Systems.AcceleratePlayer (acceleratePlayer) where

import           Control.Lens    (at, ix, over, view)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromJust)

import           GameState       (EntityType (Player), GameState, accelerating,
                                  gameStateAnglesL, gameStateEntityTypesL,
                                  gameStateVelocitiesL)
import           Physics         (Acceleration, Angle, applyAcceleration,
                                  createV, rotateV)

playerAcceleration :: Acceleration
playerAcceleration = createV 0.3 0

acceleratePlayer :: GameState -> GameState
acceleratePlayer gs = if accelerating gs
                      then let
                        update :: Int -> GameState -> GameState
                        update pid = let
                                       -- playerAngle will crash if the player angle is missing
                                       playerAngle = fromJust $ view (gameStateAnglesL . at pid) gs :: Angle
                                       acceleration = rotateV playerAngle playerAcceleration :: Acceleration
                                     in over (gameStateVelocitiesL . ix pid) (applyAcceleration acceleration)

                        setVelocities = foldr (.) id $ update <$> players gs
                      in setVelocities gs
                      else gs

players :: GameState -> [Int]
players = fmap fst . filter ((==Player) . snd) . M.toList . view gameStateEntityTypesL
