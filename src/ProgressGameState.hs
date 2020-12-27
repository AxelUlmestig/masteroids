module ProgressGameState (progressGameState) where

import           GameState                (GameState)

import           Systems.AcceleratePlayer (acceleratePlayer)
import           Systems.ApplySpin        (applySpin)
import           Systems.ApplyVelocities  (applyVelocities)
import           Systems.BorderPatrol     (borderPatrol)
import           Systems.HandleCollisions (handleCollisions)
import           Systems.HandleZeroHP     (handleZeroHP)
import           Systems.SetPlayerAngle   (setPlayerAngle)

progressGameState :: Float -> GameState -> GameState
progressGameState _ = foldr (.) id [
                                     acceleratePlayer,
                                     applySpin,
                                     applyVelocities,
                                     borderPatrol,
                                     handleCollisions,
                                     setPlayerAngle,
                                     handleZeroHP
                                   ]
