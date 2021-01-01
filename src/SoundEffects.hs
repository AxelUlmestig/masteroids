module SoundEffects (playSoundEffects, SoundEffect(..)) where

import           Control.Monad.Writer.Strict (Writer, runWriter)

import           GameState                   (GameState)

data SoundEffect = FireLaserSound
                 deriving (Eq, Show)

playSoundEffects :: Writer [SoundEffect] GameState -> IO GameState
playSoundEffects x = do
                       let (gs, soundEffects) = runWriter x
                       mapM_ playSound soundEffects
                       pure gs

-- TODO: play sounds
playSound :: SoundEffect -> IO ()
playSound = print
