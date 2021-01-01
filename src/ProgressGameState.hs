module ProgressGameState (progressGameState) where

import           Control.Monad            ((>=>))

import           GameState                (GameState)
import           SoundEffects             (playSoundEffects)
import           Systems.AcceleratePlayer (acceleratePlayer)
import           Systems.ApplySpin        (applySpin)
import           Systems.ApplyVelocities  (applyVelocities)
import           Systems.BorderPatrol     (borderPatrol)
import           Systems.HandleCollisions (handleCollisions)
import           Systems.HandleZeroHP     (handleZeroHP)
import           Systems.SetPlayerAngle   (setPlayerAngle)

progressGameState :: Float -> GameState -> IO GameState
progressGameState _ = playSoundEffects . foldr (>=>) pure [
                                     pure . acceleratePlayer,
                                     pure . applySpin,
                                     pure . applyVelocities,
                                     pure . borderPatrol,
                                     pure . handleCollisions,
                                     pure . setPlayerAngle,
                                     pure . handleZeroHP
                                   ]
